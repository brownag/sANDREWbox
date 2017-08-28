#Photo geotag to KML script
#@author: andrew brown
#@version: 0.2b; 1/13/17

### install & load required packages, if needed
packz <- c("sp","rgdal","stringr","pixmap","RCurl","utils","magick","dtw","proxy")
newpackz <- packz[!(packz %in% installed.packages()[,"Package"])]
if(length(newpackz)) install.packages(newpackz)
loaded <- lapply(packz,FUN=require,character.only=TRUE)
if(sum(as.numeric(loaded))!=length(packz)) {
  stop("Failed to load one or more required packages!")
  geterrmessage()
}

###SETUP###
#User-defined settings
threshold_dist <- 20               #meters; maximum distance between points for cluster membership and naming from DP
threshold_time <- 20               #minutes; maximum time between points for cluster membership

make_kmz <- TRUE              #requires WinZip. Creates a standalone file containing KML and images

scaling_factor <- "50%"       #string supplied to magick::image_scale() for adjusting image output

name_by_nearby_point <- TRUE  #default: FALSE; creates a new site ID for each cluster of photos
                              # if TRUE allows a separate shapefile of points to be loaded (e.g. exported from GPS, dp layer, NASIS sites etc). 
                              # if one of the points in that feature class falls within the specified threshold_dist then the name of that point will
                              # be used for the corresponding cluster. orherwise it will just get the next available numeric value.

placemark_names <- NA         #default is NA; uses either numeric or nearby point names. Need to know a priori the number of clusters.
                              # alternately can specify a vector containing pre-defined site IDs. if this is non-NA, no other naming scheme will be used. 
                            
placemark_postfix_start <- 53 #default numbering starts from 1. Change to the first site ID number used for this picture set. 
                              # Sites will be incremented based on the temporal order of the pictures.

placemark_prefix <- "2016CA63060" #default: ""; string to precede the site ID number. can be used to make e.g. NASIS user site IDs. 

point_source = 'L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb' #path to feature class containing existing site points for labeling clusters

point_layer = 'ca630_dp' #what layer to use within geodatabase. supplied to rgdal::readOGR()

centroid_function <- mean     #default: mean; function to use for aggregating x,y,z data from multiple images in a cluster;

script_dir <- "E:/scripts/geotag_to_KML/"    #path to script directory (e.g. git repository instance)

image_directory <- paste0(script_dir,"~to_sort")

output_path <- paste0(script_dir,"~sorted") #this is the path to KML/KMZ output and "sorted" site folders

#Implementation specific parameters
template_file <- 'kml_template.dat'
device_projection <- '+proj=longlat +datum=WGS84'           #projection information for data extracted from EXIF
device_timezone <- 'PST'                                    #timezone for data extracted from EXIF

#External dependencies
exiftool_path <- paste0(script_dir,"exiftool(-k).exe")          #this executable is required for extracting EXIF data from JPGs
winzip_path <- "\"C:\\Program Files (x86)\\WinZip\\wzzip.exe\"" #winzip is used for creating KMZ files

if(name_by_nearby_point)
  dp_points <- readOGR(dsn = point_source, layer = point_layer, stringsAsFactors=FALSE) 

if(!dir.exists(output_path))
  dir.create(output_path,showWarnings=FALSE,recursive=TRUE)
###########

###INTERNAL SETUP###
degms_regex <- "([0-9]+) deg ([0-9]+)' ([0-9]+\\.[0-9]+)\" (.)" # pattern for capturing degrees, min, sec and hemisphere for conversion to decimal degrees
elev_regex <- "([0-9\\.]+) m.*"                                 # pattern for capturing elevation numeric value from EXIF string
###########

### FUNCTION DEFINITIONS ###
getDecDegrees = function(s) {
  ff <- str_match(s,degms_regex)
  ff1 <- as.numeric(ff[2:4])
  hemi <- ff[5]
  sign=1
  if(hemi == "S" || hemi == "W")  sign = -1
  return(sign*(ff1[1] + (ff1[2]/60) + (ff1[3]/3600)))
  #Decimal degrees = Degrees + (Minutes/60) + (Seconds/3600)
}

getNumericElevation = function(s) {
  ff <- str_match(s,elev_regex)
  return(as.numeric(ff[2]))
}

makePhotoStringByCentroid=function(x,usecdata=TRUE) {
  #takes vector of file names and creates CDATA string for export
  imagez <- paste0("<img src=\"./",x,"\" />")
  buf=""
  if(usecdata)
    buf <- "<![CDATA["
  for(i in imagez) {
    buf <- paste0(buf,i,"<br />")
  }
  if(usecdata)
    buf <- paste0(buf,"]]>")
  return(buf)
}

makePlacemarkByCentroid = function(x,n) {
  #takes a SpatialPointsDataFrame (or subset of one) and creates a placemark for it, using first coordinate
  #x is the data, n is a unique id to use for the name field (probably an integer unless otherwise specified)
  datetime <- x$date[1]
  photostring <- makePhotoStringByCentroid(x$filename)
  buf <- ""
  buf <- paste0("\t\t\t<Placemark>\n\t\t\t\t",paste0("<name>",n,"</name>\n\t\t\t\t"))
  buf <- paste0(buf,"<TimeStamp><when>",datetime,"</when></TimeStamp>\n\t\t\t\t")
  buf <- paste0(buf,"<description>",makePhotoStringByCentroid(x$filename,usecdata=TRUE),"</description>\n\t\t\t\t")
  buf <- paste0(buf,"<ExtendedData><SchemaData schemaUrl=\"#schema0\"><SimpleData name=\"pdfmaps_photos\">",photostring,"</SimpleData></SchemaData></ExtendedData>\n\t\t\t\t")
  buf <- paste0(buf,"<Point><coordinates>",x$clng[1],",",x$clat[1],",",floor(x$celev[1]),"</coordinates></Point>\n\t\t\t</Placemark>")
  return(buf)
}

makeKML = function(output,placemarks,folder) {
  fileName <- paste0(script_dir,template_file) 
  buf <- readChar(fileName, file.info(fileName)$size)
  buf <- sub("%%%FOLDERNAME%%%",folder,buf)
  buf <- sub("%%%PLACEMARKS%%%",placemarks,buf)
  sink(output)
  cat(buf)
  sink()
}
######

### MAIN APPLICATION LOGIC ###

# load exif data from images in target directory  
filez <- as.character(list.files(path=image_directory,full.names=TRUE))
filez <- filez[grepl(pattern=".*\\.JPG",x=filez,ignore.case=TRUE)] #keep only JPEG
exiftool_callz <- paste0(exiftool_path," \"",filez,"\"")
dat <- data.frame(path=character(), filename = character(), date = character(), 
               lat=numeric(),lng=numeric(), elev = numeric(), 
               bearing = character(),imgh=numeric(),imgw=numeric(),etime=character(),flen=character())
for(g in exiftool_callz) {
  raw <- system(g,intern=TRUE)
  r <- strsplit(raw,"\n")
  goober <- str_match(r, "(.*): (.*)")
  l <- goober[,3]
  names(l) <- str_trim(goober[,2])
  #print(names(l))
  field_names_GPS <- c('Directory','File Name',"GPS Date/Time","GPS Altitude","GPS Latitude","GPS Longitude","Image Height","Image Width") 
  #ideal clustering based on GPS location
  field_names_min <- c('Directory','File Name',"Date/Time Original","Image Height","Image Width") 
  #minimum information to cluster is just the timestamp
  
  if(sum(field_names_GPS %in% names(l)) >= length(field_names_GPS)) {
    pname <- paste0(l[['Directory']],'/',l[['File Name']])
    fname <- l[['File Name']]
    print(l[["GPS Date/Time"]])#debug
    datz <- strptime(l[["GPS Date/Time"]],"%Y:%m:%d %H:%M:%S")
    elez <- getNumericElevation(l[["GPS Altitude"]])
    latz <- getDecDegrees(l[["GPS Latitude"]])
    lngz <- getDecDegrees(l[["GPS Longitude"]])
    beaz <- NA
    try(expr=(beaz<-as.numeric(l[['GPS Img Direction']])),silent = TRUE) #sometimes this fails w/ no bearing information, so we do not require it above
    imghz <- as.numeric(l[['Image Height']])
    imgwz <- as.numeric(l[['Image Width']])
    dat <- rbind(dat,data.frame(path=pname,filename=fname, date=datz, lat=latz, lng=lngz, elev=elez, bearing=beaz, imgh=imghz, imgw=imgwz))
  } else if (sum(field_names_min %in% names(l)) >= length(field_names_min)) {
    pname <- paste0(l[['Directory']],'/',l[['File Name']])
    fname <- l[['File Name']]
    print(l[["Date/Time Original"]])#debug
    datz <- strptime(l[["Date/Time Original"]],"%Y:%m:%d %H:%M:%S",tz="America/Los_Angeles")
    imghz <- as.numeric(l[['Image Height']])
    imgwz <- as.numeric(l[['Image Width']])
    dat <- rbind(dat,data.frame(path=pname,filename=fname, date=datz, lat=NA, lng=NA, elev=NA, bearing=NA, imgh=imghz, imgw=imgwz))
  } else {
    #This shouldn't happen unless something is wrong with the exiftool call/paths
    message("Cannot get EXIF data for image!")
  }
}

#now we have a data frame with one record per image; some may have spatial info others may just have timestamp
idx.no_sp = which(is.na(dat$lat)) #if latitude is NA then we will assume spatial info is missing and subset those out

dat_min = dat[idx.no_sp, ] 

if(length(idx.no_sp) > 0)
  dat = dat[-idx.no_sp, ]

#if any of the images are missing spatial data, use timestamp clustering based on the supplied point file 

timepoints <- readOGR(dsn = "E:\\Points", layer = "02.01.17", stringsAsFactors=FALSE) 
dptimes <- as.numeric(as.POSIXct(strptime(timepoints$time,"%Y/%m/%d %H:%M:%S",tz = "GMT")))
placemark_names_min=c()
dp_points_tagged_min=0
if(nrow(dat_min) > 0) {
  distmat <- dist(dat_min$date)
  hr <- hclust(distmat, method = "complete", members=NULL)
  plot(hr)
  dat_min$centroid=cutree(hr, h=threshold_time*60)
  c_times <- as.numeric(as.POSIXlt(aggregate(dat_min$date,by=list(dat_min$centroid),FUN=mean)[,2],tz="GMT"))
  dat_min$c_time=NA
  if(name_by_nearby_point) {
    for(c in 1:length(levels(factor(dat_min$centroid)))) {
      idx <- which(dat_min$centroid == c)
      dat_min$c_time[idx] = c_times[c]
      diffz=abs(as.numeric(c_times[c] - dptimes))
      diffz[which(diffz>threshold_time*60)] = NA
      if(sum(is.na(diffz)) == length(diffz)) {
        print(paste0("Failed to identify a user site ID for cluster: ",c))
        placemark_names_min <- c(placemark_names_min,paste0(c))
      } else {
        bestdiff=which(diffz == min(diffz, na.rm=T))
        placemark_names_min <- c(placemark_names_min,(timepoints[bestdiff,]$ident))
        print(paste0("Cluster ",c," is ",floor(diffz[bestdiff]/60)," minutes from ",timepoints[bestdiff,]$ident))
        dp_points_tagged_min <- dp_points_tagged_min+1
      }
    }  
    print(paste0("Tagged ",dp_points_tagged_min, " out of ", length(placemark_names_min), " clusters to existing points in the DP layer."))
  } else {
    placemark_names_min <- c(placemark_names_min,paste0(c))
  } 
}

#if any of the images have spatial data, go a ahead with the spatial clustering
if(nrow(dat) > 0) { 
  #elevate dataframe to SpatialPointsDataFrame and specify device projection system
  coordinates(dat) <- ~lng+lat
  proj4string(dat) <- device_projection
  
  # cluster coordinates using threshold_dist
  distz <- spDists(dat,longlat=TRUE)*1000 #gives distance between points in meters (KM*1000); 
  #      may generate warning due to issues with different projections and calculation of distance
  hr <- hclust(dist(distz), method = "complete", members=NULL)
  plot(hr) #debug: check tree visually
  dat$centroid <- cutree(hr, h=threshold_dist)#cut the tree at distance threshold_dist to define clusters
  print(paste0("Created ",length(levels(factor(as.numeric(dat$centroid))))," clusters from ",length(dat$centroid)," images. ",(length(filez)-length(dat$centroid)), " were missing spatial information in EXIF data."))
  
  #calculate "centroids" by aggregating values for each cluster (defined centroid_function in setup)
  c_lat <- aggregate(dat$lat,by=list(dat$centroid),FUN=centroid_function)[,2]
  c_lng <- aggregate(dat$lng,by=list(dat$centroid),FUN=centroid_function)[,2]
  c_elev <- aggregate(dat$elev,by=list(dat$centroid),FUN=centroid_function)[,2]
  
  #add centroid values to SpatialPointsDataFrame along with individual data
  dat$clat <- numeric(length(dat$filename))
  dat$clng <- numeric(length(dat$filename))
  dat$celev <- numeric(length(dat$filename))
  for(i in as.numeric(levels(factor(dat$centroid)))) {
    who=which(dat@data$centroid == i)
    dat@data[who,]$clat <- c_lat[i]
    dat@data[who,]$clng <- c_lng[i]
    dat@data[who,]$celev <- c_elev[i]
  }
  dat@data$filename <- as.character(dat@data$filename)
  dat2=dat@data                         #make a copy of the data we have updated
  coordinates(dat2) <- ~clng+clat+celev #elevate the copy to SpatialPointsDataFrame, this time using the centroid values for points. 
                                        #each record still retains individual locations.
  proj4string(dat2) <- device_projection
  
  ncclust <- levels(factor(dat2$centroid))
  placemark_names <- c()
  
  if(name_by_nearby_point) {
    dp_points_tagged <- 0
    #try to name centroids based on DP layer points within threshold distance
    dat_dp <- spTransform(dat2,proj4string(dp_points)) # convert to CRS of dp layer
    for(ddp in 1:length(ncclust)) {
      sdat_dp <- dat_dp[which(dat_dp$centroid == ddp),]
      dpdistz <- spDistsN1(pt=coordinates(sdat_dp)[1,1:2],pts=dp_points) #calculates distances from all centroids to all dp points
      dpid <- which(dpdistz==min(dpdistz))[1] #if multiple meet the threshold, take the first (closest)
      print(paste("Cluster",ddp,"is",dpdistz[dpid],"meters from",dp_points[dpid,]$IDENT)) 
      if(dpdistz[dpid] <= threshold_dist) {
        placemark_names <- c(placemark_names,(dp_points[dpid,]$IDENT))
        dp_points_tagged <- dp_points_tagged+1
      } else {
        placemark_names <- c(placemark_names,paste0(ddp))
      }  
    }
    print(paste0("Tagged ",dp_points_tagged, " out of ", length(placemark_names), " clusters to existing points in the DP layer."))
  }
}

foldername <- Sys.Date()
placemarkz <- ""
if(is.na(placemark_names) && placemark_postfix_start > 0) { 
  # if the placemark name list is NA then we will just number them from start to start+number of clusters
  placemark_names <- placemark_postfix_start:(placemark_postfix_start+length(ncclust)) 
  placemark_names <- paste0(placemark_prefix, placemark_names) #adds the prefix, default is ""
}

for(j in as.numeric(ncclust)) {
  who <- which(dat2$centroid == j)
  subse <- dat2[who,]
  placemarkz <- paste0(placemarkz,makePlacemarkByCentroid(subse,placemark_names[j]),"\n") 
  #TODO: fail elegantly if there aren't enough names for centroids;
}
makeKML(paste0(output_path,"/doc.kml"),placemarkz,foldername)

#make KMZ file
if(make_kmz) {
  dir.create(paste0(output_path,"/images"),recursive=TRUE,showWarnings=FALSE)
  for(f in 1:length(filez)) {
    #instead of using system copy, use magick to read in source, resize and write to target directory
    #file.copy(filez[f],paste0(output_path,"\\images\\",dat2$filename[f]))
    img <- image_read(filez[f])
    img_s <- image_scale(img,scaling_factor)
    image_write(image=img_s,path=paste0(output_path,"\\images\\",dat2$filename[f]))
  }
  system(paste0(winzip_path, " ",output_path,"/",foldername,".kmz \"",output_path,"/*.*\""," \"",output_path,"/images/*.*\""))
}

#make a folder for each cluster, named by site id/placemark name
for(p in 1:length(ncclust)) {
  clust <- ncclust[p]
  outdir <- paste0(output_path,"/",placemark_names[p])
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  who <- which(dat2$centroid == p)
  subse <- dat2[who,]
  for(s in 1:length(subse)) {
    #instead of using system copy, use magick to read in source, resize and write to target directory
    #file.copy(paste0(subse[s,]$path),paste0(outdir,"/",subse[s,]$filename))
    img <- image_read(paste0(subse[s,]$path))
    img_s <- image_scale(img,scaling_factor)
    image_write(image=img_s,path=paste0(outdir,"/",subse[s,]$filename))
  }
}