#PhotoGeotagExtractor
#@author: andrew brown
#@version: 0.4; 11/06/19

### SETUP ###
input_path <- "S:/NRCS/Archive_Andrew_Brown/CA649/Pictures/"
output_path <- "S:/NRCS/Archive_Andrew_Brown/CA649/Pictures/sorted/" #this is the path to KML/KMZ output and "sorted" site folders

point_source = 'S:/NRCS/Archive_Andrew_Brown/CA649/Points' #path to feature class containing existing site points for labeling clusters
point_layer = 'ca649_dp' #what layer to use within geodatabase. supplied to rgdal::readOGR()

device_projection <- '+proj=longlat +datum=WGS84 +ellps=GRS80'           #projection information for data extracted from EXIF
device_timezone <- Sys.timezone()                           #timezone associated with device, used for coercing date objects; 

threshold_dist <- 100              #meters; maximum distance between points for cluster membership and naming from DP

scaling_factor <- "50%" # image scaling factor for magick::image_scale()

### get @brownag fork of EXIFr
devtools::install_github("brownag/EXIFr")
library(EXIFr)
library(rgdal)
library(magick)

dp_points <- readOGR(dsn = point_source, layer = point_layer, stringsAsFactors=FALSE) 
dp_points <- spTransform(dp_points, CRS(device_projection))

imgfile <- as.character(list.files(path=input_path, full.names=TRUE, recursive = TRUE))
imgfile <- imgfile[grepl(pattern="IMG_.*\\.JPE?G", x=imgfile, ignore.case=TRUE)] #keep only JPEG

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

# write table of EXIF data for all JPG files to output_path
write.csv(imgexif, paste0(output_path,"/~imgexif_raw.csv"))

# parse EXIF rational degrees, minutes and seconds into decimal degrees
parseCoordinate <- function(dms, .split=" ") {
  as.numeric(lapply(lapply(strsplit(as.character(dms), split = .split), 
                           function(dds) lapply(dds, EXIFr::rational_to_numeric)), 
                      function(i) ifelse(length(i) < 3, NA, i[[1]] + i[[2]] / 60 + i[[3]] / 3600)))
}

imgexif$y_std <- parseCoordinate(imgexif$GPSLatitude)
imgexif$x_std <- -parseCoordinate(imgexif$GPSLongitude)

bad.idx <- which(is.na(imgexif$y_std) | is.na(imgexif$x_std))
imgexif.bad <- imgexif[bad.idx, ] 
imgexif <- imgexif[-bad.idx,]

# promote imgexif to SpatialPointsDataFrame
coordinates(imgexif) <- ~ x_std + y_std
proj4string(imgexif) <- device_projection

# inspect photo locations
plot(imgexif)
points(dp_points, col="blue")

# calculate spatial distance matrix -- just images against each other
distmat1 <- spDists(imgexif, longlat=TRUE) * 1000
hr1 <- hclust(dist(distmat1), method = "complete", members=NULL)

# apply user defined distance threshold to cluster groups of pictures
imgexif$image_centroid <- cutree(hr1, h = threshold_dist / 2)

image_centroids <- as.data.frame(do.call('rbind',lapply(split(imgexif, f = imgexif$image_centroid), function(sub) {
  colMeans(coordinates(sub))
})))
coordinates(image_centroids) <- ~ x_std + y_std
proj4string(image_centroids) <- device_projection
points(image_centroids, col="red")

# calculate spatial distance matrix of centroids against datapoint locations
imgvdp <- spDists(x = imgexif, y=dp_points, longlat=TRUE) * 1000

# determine which dp_point has lowest distance to each image centroid
imgexif$closest_dp <- apply(imgvdp, 1, function(i) which(i == min(i)))

# calculate shortest distance of dp_point to centroid
imgexif$distance_to_closest <- apply(imgvdp, 1, function(i) i[i == min(i)])

# these centroids are too far from the closest known point
imgexif$closest_dp[which(distance_to_closest > threshold_dist)] <- NA

paste0("Found a data point within ",threshold_dist," meters for ",round(sum(!is.na(imgexif$closest_dp)) / length(imgexif$closest_dp) * 100),"% of the ",length(imgexif)," images in ", input_path)

imgsets <- split(imgexif, imgexif$closest_dp)
imgunmatched <- split(imgexif[is.na(imgexif$closest_dp),], imgexif[is.na(imgexif$closest_dp),]$image_centroid)

paste0("Moving matched images...")
for(i in 1:length(imgsets)) {
  pat <- paste0(output_path, dp_points$ident[i],"/")
  if(!dir.exists(pat)) dir.create(pat)
  filez <- as.character(imgsets[[as.character(i)]]$FileName)
  res <- lapply(as.list(1:length(filez)), function(f) {
    img <- image_read(filez[as.numeric(f)])
    img_s <- image_scale(img, scaling_factor)
    image_write(image = img_s, path = paste0(pat, basename(filez[f])))
  })
  if(length(res)) {
    posixtime <- strptime(as.character(imgsets[[as.character(i)]]$DateTimeOriginal), format = "%Y:%m:%d %H:%M:%OS")
    medtime <- median(posixtime)
    mintime <- min(posixtime)
    maxtime <- max(posixtime)
    
    # round elapsed time, convert to hours
    elps <- round(maxtime - mintime,1) 
    if(attr(elps, "units") == "days") elps <- elps * 24
    if(attr(elps, "units") == "mins") elps <- elps / 60
    if(attr(elps, "units") == "secs") elps <- elps / (60^2)
    
    # if a pit takes longer than 3 hours based on first and last pic ... worth inspecting -- so print some debug info
    if(elps > 3)
      paste(dp_points[i,]$ident, "elapsed:", round(elps,2) , "hours (start:", mintime, "median:", medtime, "stop: ",maxtime,")")
  }
}

paste0("Moving unmatched images...")
pat <- paste0(output_path, "~unmatched/")
if(!dir.exists(pat)) dir.create(pat)
for(i in 1:length(imgunmatched)) {
  if(length(imgunmatched[[as.character(i)]])) {
    pat <- paste0(output_path, "~unmatched/", i, "/")
    if(!dir.exists(pat)) dir.create(pat)
    #file.copy(imgunmatched[[as.character(i)]]$FileName, pat)
    filez <- as.character(imgunmatched[[as.character(i)]]$FileName)
    lapply(as.list(1:length(filez)), function(f) {
      img <- image_read(filez[as.numeric(f)])
      img_s <- image_scale(img, scaling_factor)
      image_write(image = img_s, path = paste0(pat, basename(filez[f])))
    })
  }
}

paste0("Moving images without spatial reference...")
pat <- paste0(output_path, "~no_sp/")
if(!dir.exists(pat)) dir.create(pat)
if(length(imgexif.bad)) {
  filez <- as.character(imgexif.bad[[as.character(i)]]$FileName)
  lapply(as.list(1:length(filez)), function(f) {
    img <- image_read(filez[as.numeric(f)])
    img_s <- image_scale(img, scaling_factor)
    image_write(image = img_s, path = paste0(pat, basename(filez[f])))
  })
}
