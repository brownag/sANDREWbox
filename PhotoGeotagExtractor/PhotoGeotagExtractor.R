# PhotoGeotagExtractor
###  Author: andrew brown
###  Version: 0.4
###  Last update: 2019/11/06
# input_path - this is the path to unsorted JPG images
input_path <- "S:/NRCS/Archive_Andrew_Brown/CA649/Pictures/unsorted/"

# output_path - this is the path to "sorted" site folders
output_path <- "S:/NRCS/Archive_Andrew_Brown/CA649/Pictures/sorted/" 

# point_source - rgdal data source name of known data point locations (e.g. NASIS sites)
point_source = 'S:/NRCS/Archive_Andrew_Brown/CA649/Points' 

# point_layer - layer name within point_source folder/geodatabase
point_layer = 'ca649_dp' 

# projection information for data extracted from EXIF (assumed)
device_projection <- '+proj=longlat +datum=WGS84 +ellps=GRS80'

# [currently not supported] device_timezone - convert image timezone to local timezone
device_timezone <- Sys.timezone()                            

# threshold_distance - maximum distance of image centroids to related points in point_layer
#                      NOTE: when calculating image centroids, _half_ the threshold_dist in hierarchical clustering
threshold_dist <- 100              

# scaling_factor - percentage to scale sorted image by, using magick::image_scale()
scaling_factor <- "100%"

### SETUP ###
# # input_path - this is the path to unsorted JPG images
# input_path <- "S:/NRCS/Archive_Andrew_Brown/CA649/Pictures/"
# 
# # output_path - this is the path to "sorted" site folders
# output_path <- "S:/NRCS/Archive_Andrew_Brown/CA649/Pictures/sorted/" 
# 
# # point_source - rgdal data source name of known data point locations (e.g. NASIS sites)
# point_source = 'S:/NRCS/Archive_Andrew_Brown/CA649/Points' 
# 
# # point_layer - layer name within point_source folder/geodatabase
# point_layer = 'ca649_dp' 
# 
# # projection information for data extracted from EXIF (assumed)
# device_projection <- '+proj=longlat +datum=WGS84 +ellps=GRS80'
# 
# # [currently not supported] device_timezone - convert image timezone to local timezone
# device_timezone <- Sys.timezone()                            
# 
# # threshold_distance - maximum distance of image centroids to related points in point_layer
# #                      NOTE: when calculating image centroids, _half_ the threshold_dist in hierarchical clustering
# threshold_dist <- 100              
# 
# # scaling_factor - percentage to scale sorted image by, using magick::image_scale()
# scaling_factor <- "50%"

### get @brownag fork of EXIFr
devtools::install_github("brownag/EXIFr")
library(EXIFr)

### install & load required packages, if needed
packz <- c("rgdal","magick")
newpackz <- packz[!(packz %in% installed.packages()[,"Package"])]
if(length(newpackz)) 
  install.packages(newpackz)
loaded <- lapply(packz, FUN=require, character.only=TRUE, quietly=TRUE)
if(sum(as.numeric(loaded))!=length(packz)) {
  stop("Failed to load one or more required packages!")
  geterrmessage()
}

# load datapoint locations
dp_points <- readOGR(dsn = point_source, layer = point_layer, stringsAsFactors=FALSE) 
dp_points <- spTransform(dp_points, CRS(device_projection))

# list image files, remove non-JPG files
imgfile <- as.character(list.files(path=input_path, full.names=TRUE, recursive = TRUE))
imgfile <- imgfile[grepl(pattern=".*\\.JPE?G", x=imgfile, ignore.case=TRUE)] #keep only JPEG

getImageEXIF <- function(list.imgfile, .fields = c('DateTimeOriginal','GPSLatitude','GPSLongitude')) {
  do.call('rbind', lapply(list.imgfile, function(f, .fields) {
    f.exif <- EXIFr::read_exif_tags(f)
    tags <- names(f.exif)
    
    res <- data.frame(FileName = as.character(f))
    for(tag in tags) {
      res <- cbind(res, as.data.frame(f.exif[[tag]]))
    }
    colnames(res) <- c('FileName', tags)
    
    if(any(!.fields %in% names(res))) {
      res[, .fields[!.fields %in% names(res)]] <- NA
    }
    return(res[, .fields])
  }, .fields))
}

# read EXIF data from all JPG files in input_path
imgexif <- getImageEXIF(imgfile, .fields = c('FileName','DateTimeOriginal','ApertureValue',
                                                    'SubjectDistance','SubjectDistanceRange',
                                                    'FocalLength','DigitalZoomRatio', 
                                                    'GPSLatitude','GPSLatitudeRef','GPSLongitude',
                                                    'GPSLongitudeRef','ImageWidth','ImageLength','Orientation'))
if(!dir.exists(output_path))
  dir.create(output_path, recursive = TRUE)

# write table of EXIF data for all JPG files to output_path
write.csv(imgexif, paste0(output_path,"~imgexif_raw.csv"))

# parse EXIF rational degrees, minutes and seconds into decimal degrees
parseCoordinate <- function(dms, .split=" ") {
  as.numeric(lapply(lapply(strsplit(as.character(dms), split = .split), 
                           function(dds) lapply(dds, EXIFr::rational_to_numeric)), 
                      function(i) ifelse(length(i) < 3, NA, i[[1]] + i[[2]] / 60 + i[[3]] / 3600)))
}

# hack for now; see issue #1
imgexif$y_std <- parseCoordinate(imgexif$GPSLatitude)
imgexif$x_std <- -parseCoordinate(imgexif$GPSLongitude)

# identify images without coordinates, store in separate data frame
bad.idx <- which(is.na(imgexif$y_std) | is.na(imgexif$x_std))
imgexif.bad <- imgexif[bad.idx, ] 

# remove bad ids
if(length(bad.idx))
  imgexif <- imgexif[-bad.idx,]

# promote imgexif to SpatialPointsDataFrame
coordinates(imgexif) <- ~ x_std + y_std
proj4string(imgexif) <- device_projection

# inspect photo locations
plot(imgexif)
maps::map(database="county", regions = "ca", add=T)
points(dp_points, col="blue", pch = "*")

# calculate spatial distance matrix -- just images against each other
distmat1 <- spDists(imgexif, longlat=TRUE) * 1000
hr1 <- hclust(dist(distmat1), method = "complete", members=NULL)

# apply user defined distance threshold to cluster groups of pictures
imgexif$image_centroid <- cutree(hr1, h = threshold_dist / 2)

# calculate centroids
image_centroids <- as.data.frame(do.call('rbind',lapply(split(imgexif, f = imgexif$image_centroid), function(sub) {
  # colMeans(coordinates(sub))
  # use median, not mean -- less sensitive to outliers
  apply(coordinates(sub), 2, median)
})))
coordinates(image_centroids) <- ~ x_std + y_std
proj4string(image_centroids) <- device_projection
points(image_centroids, col="red", cex=2)

# calculate spatial distance matrix of centroids against datapoint locations
imgvdp <- spDists(x = imgexif, y=dp_points, longlat=TRUE) * 1000

# determine which dp_point has lowest distance to each image centroid
imgexif$closest_dp <- apply(imgvdp, 1, function(i) which(i == min(i)))

# calculate shortest distance of dp_point to centroid
imgexif$distance_to_closest <- apply(imgvdp, 1, function(i) i[i == min(i)])

# these centroids are too far from the closest known point
imgexif$closest_dp[which(imgexif$distance_to_closest > threshold_dist)] <- NA

paste0("Found a data point within ",threshold_dist," meters for ",round(sum(!is.na(imgexif$closest_dp)) / length(imgexif$closest_dp) * 100),"% of the ",length(imgexif)," images in ", input_path)

imgsets <- split(imgexif, imgexif$closest_dp)
imgunmatched <- split(imgexif[is.na(imgexif$closest_dp),], imgexif[is.na(imgexif$closest_dp),]$image_centroid)

paste0("Moving matched images...")
for(i in 1:length(imgsets)) {
  pat <- paste0(output_path, dp_points$ident[as.numeric(names(imgsets)[i])],"/")
  if(!dir.exists(pat)) 
    dir.create(pat)
  filez <- as.character(imgsets[[names(imgsets)[i]]]$FileName)
  res <- lapply(as.list(1:length(filez)), function(f) {
    fi <- filez[as.numeric(f)]
    if(!is.na(fi) & length(fi)) {
      img <- image_read(fi)
      img_s <- image_scale(img, scaling_factor)
      image_write(image = img_s, path = paste0(pat, basename(fi)))
    }
  })
  if(length(res)) {
    posixtime <- strptime(as.character(imgsets[[names(imgsets)[i]]]$DateTimeOriginal), format = "%Y:%m:%d %H:%M:%OS")
    medtime <- median(posixtime)
    mintime <- min(posixtime)
    maxtime <- max(posixtime)
    
    # round elapsed time, convert to hours
    elps <- round(maxtime - mintime,1) 
    if(attr(elps, "units") == "days") elps <- elps * 24
    if(attr(elps, "units") == "mins") elps <- elps / 60
    if(attr(elps, "units") == "secs") elps <- elps / (60^2)
    
    # if a pit takes longer than 3 hours based on first and last pic ... worth inspecting -- so print some debug info
    if(!is.na(elps) & length(elps) & elps > 3)
      paste(dp_points[i,]$ident, "elapsed:", round(elps,2) , "hours (start:", mintime, "median:", medtime, "stop: ",maxtime,")")
  }
}

paste0("Moving unmatched images...")
pat <- paste0(output_path, "~unmatched/")
if(!dir.exists(pat)) dir.create(pat)
for(i in 1:length(imgunmatched)) {
  if(length(imgunmatched[[names(imgunmatched)[i]]])) {
    pat <- paste0(output_path, "~unmatched/", i, "/")
    if(!dir.exists(pat)) dir.create(pat)
    #file.copy(imgunmatched[[as.character(i)]]$FileName, pat)
    filez <- as.character(imgunmatched[[names(imgunmatched)[i]]]$FileName)
    lapply(as.list(1:length(filez)), function(f) {
      fi <- filez[as.numeric(f)]
      if(!is.na(fi) & length(fi)) {
        img <- image_read(fi)
        img_s <- image_scale(img, scaling_factor)
        image_write(image = img_s, path = paste0(pat, basename(fi)))
      }
    })
  }
}
print(paste("Unable to match", length(imgunmatched),"files with known site."))

paste0("Moving images without spatial reference...")
pat <- paste0(output_path, "~no_sp/")
if(!dir.exists(pat)) 
  dir.create(pat)
if(length(imgexif.bad)) {
  filez <- as.character(imgexif.bad$FileName)
  if(length(filez)) {
    res <- lapply(as.list(1:length(filez)), function(f) {
      fi <- filez[as.numeric(f)]
      if(!is.na(fi) & length(fi)) {
        img <- image_read(fi)
        img_s <- image_scale(img, scaling_factor)
        image_write(image = img_s, path = paste0(pat, basename(fi)))
      }
    })
  }
}
print(paste("Found", length(res),"files with no spatial information."))
print("Done")