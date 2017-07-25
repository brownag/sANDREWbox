#image resize recursive v0.1
#author: andrew brown
#purpose: finds all image files in a user-specified parent directory. 
#         resizes them to the specified geometry (using magick::image_scale)
#         writes transformed imagesto specified output folder, retaining parent folder structure.
###--------------------------------------------------------------------------------------------------------------------------

#paths to find source images and output location; INCLUDE TRAILING SLASHES; output directory will be created recursively if needed
base.path <- "S:\\NRCS\\120 ADS Administrative Services\\120-19 Personal Property\\Vehicles\\A286998_Dodge Dakota\\"
out.path <- "S:\\NRCS\\120 ADS Administrative Services\\120-19 Personal Property\\Vehicles\\A286998_Dodge Dakota\\resize\\"

geom <- "25%" #can supply a variety of geometries to image_scale() such as percentage, target dimensions (pixels) etc.

file.extensions <- "jpg|jpeg|png|gif" #pipe delimited string of file extensions; case insensitive; no "." required
out.format <- "jpeg" #format for exporting resized images

#ARGUMENTS
### - base.path - path to directory containing images (may contain subfolders)
### - out.path - path to directory to place resized images (need not exist)
### - geom - geometry argument used to specify scaling transformation to magick package
### - file.extensions - regular expression pattern denoting file extension types (case-insensitve)
###--------------------------------------------------------------------------------------------------------------------------

library(magick)

#if output path does not exist, make it (recursively)
if(!dir.exists(out.path))
  dir.create(out.path,recursive=T)
  
#get a list of ALL the files within the base.path, recursively
filez <- paste0(base.path,list.files(base.path,recursive=T))

isImage <- function(x) {
  #isImage() takes one or more file names/paths and determines if they reference image files (returns true or false)
  
  #image file extension types are defined by the user at top of script with a pipe delimited list that is used within a regex group below
  #note that the regex pattern checks that a "." occurs before extension, and that extension is at end of file string
  return(grepl(x, pattern=paste0("\\.(",file.extensions,")$"), ignore.case=T))
}

for(p in filez) {
  if(isImage(p)) { #if file has image extension
    img <- magick::image_read(p) #read image into memory
    img <- magick::image_scale(img, geom) #apply scaling transformation with user-specified geomoetry
    foo <- gsub(pattern = base.path, x = p, replacement=out.path, fixed = T) #create a set of output paths using same hierarchy as input
    if(!dir.exists(dirname(foo))) # if output path directory does not exist, create it (recursively)
       dir.create(dirname(foo), recursive = T)
    magick::image_write(image = img, path = foo, format = out.format) #write scaled image to file
  }
}

#img #show what image in memory in plot/graphics pane
