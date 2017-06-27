#image resize recursive v0.1
#author: andrew brown
#purpose: finds all image files in a user-specified parent directory. 
#         resizes them to the specified geometry (using magick::image_scale)
#         writes transformed imagesto specified output folder, retaining parent folder structure.

###--------------------------------------------------------------------------------------------------------------------------
base.path <- "S:\\NRCS\\Archive_Andy_Paolucci\\NEON_SJER_6_9_2017\\"
out.path <- "S:\\NRCS\\Archive_Andy_Paolucci\\NEON_SJER_6_9_2017\\resize\\"

geom <- "25%" #can supply a variety of geometries to image_scale() such as percentage, target dimensions (pixels) etc.

file.extensions <- "jpg|jpeg|png|gif" #pipe delimited list of file extensions; concatenated to regex pattern to check that it is an extension in filename

out.format <- "jpeg" #format for exporting resized images

#ARGUMENTS
### - base.path - path to directory containing images (may contain subfolders)
### - out.path - path to directory to place resized images (need not exist)
### - geom - geometry argument used to specify scaling transformation to magick package
### - file.extensions - regular expression pattern denoting file extension types (case-insensitve)
###--------------------------------------------------------------------------------------------------------------------------

install.packages("magick")
library(magick)

if(!dir.exists(out.path))
  dir.create(out.path,recursive=T)
  
filez <- paste0(base.path,list.files(base.path,recursive=T))

isImage <- function(x) {
  #takes one or more file name/paths and determines if they reference image files (true or false)
  #image file extension types are defined by the user at top of script with a pipe delimited list that is used within a regex group below
  #note that the regex pattern checks that a "." occurs before extension, and that extension is at end of file string
  return(grepl(x, pattern=paste0("\\.(",file.extensions,")$"), ignore.case=T))
}

for(p in filez) {
  if(isImage(p)) {
    img <- magick::image_read(p)
    img <- magick::image_scale(img, geom)
    foo <- gsub(pattern = base.path, x = p, replacement=out.path, fixed = T)
    if(!dir.exists(dirname(foo)))
       dir.create(dirname(foo), recursive = T)
    magick::image_write(image = img, path = foo, format = out.format)
  }
}
img
