# PhotoGeotagExtractor
###  Author: andrew brown
###  Version: 0.4
###  Last update: 2019/11/06

Extract EXIF Geotag information from JPEG files, and use it to sort/cluster site photos using a shapefile containing known site locations (e.g. NASIS sites)

Now using all _native *R* code_! 

Relies on my fork of [cmartin/EXIFr](https://github.com/cmartin/EXIFr), which adds parsing functionality for a much broader set of TIFF IFD tags than the original.

Note that `EXIFr` is NOT the same package as `exifr`, which can be found on CRAN. `exifr` relies on either an external Perl library, or a compiled executable, neither of which are an option on USDA Common Computing Environment.

## Get brownag's fork of `EXIFr`
```r
devtools::install_github("brownag/EXIFr")
```

## Setup (found at top of _PhotoGeotagExtractor.R_)

```r
# input_path - this is the path to unsorted JPG images
input_path <- "S:/NRCS/Archive_Andrew_Brown/CA649/Pictures/"

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

# threshold_distance - maximum distance of image cluster centroids to related points in point_layer
#                      NOTE: when calculating image centroids, _half_ the threshold_dist is used to define clusters
threshold_dist <- 100              

# scaling_factor - percentage to scale sorted image by, using magick::image_scale()
scaling_factor <- "50%"
```