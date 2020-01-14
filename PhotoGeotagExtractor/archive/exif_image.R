#read EXIF image attribute names from file

#definitions file for exif attributes (may be device specific)
exif_attr <- read.csv("E:\\scripts\\geotag_to_KML\\exif_attributes.csv",stringsAsFactors = F)

exiftoolpath <- paste0(script_dir,"exiftool(-k).exe")          #this executable is required for extracting EXIF data from JPGs

.EXIFImageCollectionValidity <- function(object) { 
  return(TRUE) #TODO: actually check validity... :)
}
  
#create an S4 class for handling EXIF data
setClass("EXIFImage", validity=.EXIFImageCollectionValidity, representation = representation(exifdata='list'))

## init
"EXIFImage" <- function(filename){
  # creation of the object (includes a validity check)
  new("EXIFImage", exifdata=getEXIF())
}

E

#EXIFImage accessor methods
setGeneric("setExifToolPath", function(object, path) standardGeneric("setExifToolPath"))
setMethod("setExifToolPath", signature(object = "EXIFImage"), function(object, path) {
  object@exiftool_path = path
  return(object)
})

#make EXIFImage() from file
setGeneric("getEXIF", function(object, filepath) standardGeneric("getEXIF"))

setMethod("getEXIF", signature(object = "EXIFImage"), function(object, filepath) {

})


.getEXIFinternal <- function(exiftoolpath, filepath) {
  if(!is.na(exiftoolpath)) { 
    callz <- paste0(exiftoolpath," \"",filepath,"\"")
    raw <- system(callz,intern=TRUE)
    r <- strsplit(raw,"\n")
    
    m <- str_match(r, "(.*): (.*)")
    values <- m[,3] #values
    keys <- str_trim(m[,2]) #keys
    return(setNames(as.list(values(exif_attr$attr %in% keys)), exif_attr$attr))
  }
}

callz <- paste0(exiftool_path," \"",filez[1],"\"")
raw <- system(callz,intern=TRUE)
r <- strsplit(raw,"\n")

m <- str_match(r, "(.*): (.*)")
values <- m[,3] #values
keys <- str_trim(m[,2]) #keys
attrz <- exif_attr$attr[exif_attr$name %in% keys]
valz <- values[exif_attr$name %in% keys]
obj <- EXIFImage(filename="foo.jpg")
