## Example: Fetch SSURGO data for arbitrarily large/complex areas with the FedData package
#@author: Andrew Brown

# sf package for USAboundaries
library(sf)

# USAboundaries for getting county geometry
library(USAboundaries)

# FedData for routines for accessing/stitching multi-SSA SSURGO data
library(FedData)

library(magrittr)

# label for your files
project.label <- "CentralSierra"
state <- "California"
county.fips <- c("009", "109", "003", "005")

# use USAboundaries package to get all counties in california
contemporary <- us_counties(states = c(state))

# select just polygons for fips codes specified
project.template <- contemporary[contemporary$countyfp %in% county.fips,]

# merge into a single polygon
project.template$grpid <- 1
project.template <- st_union(project.template)

## OR you can load your data from your own shapefile with rgdal
#library(rgdal)
#your.template <- readOGR(dsn = "./path/to/shapefile/", layer = "filenamenoextension")

# inspect template
plot(st_geometry(project.template))

# get_ssurgo downloads the requisite SSAs to the folder ./RAW/SSURGO/
# then it extracts spatial and tabular data (.SHP & .CSV) into ./EXTRACTIONS/
your.ssurgo <- get_ssurgo(template = as(project.template, 'Spatial'), label = project.label, force.redo = TRUE)

# create unique ID and ensure correct datatype for MUKEY
mupoly <- readOGR(dsn = paste0("./EXTRACTIONS/",project.label,"/SSURGO"),
                  layer = paste0(project.label,"_SSURGO_Mapunits"), stringsAsFactors = FALSE)
mupoly$OBJECTID <- 1:nrow(mupoly)
mupoly$MUKEY <- as.double(mupoly$MUKEY)
writeOGR(mupoly, dsn = paste0("./EXTRACTIONS/",project.label,"/SSURGO"),
         layer = paste0(project.label,"_SSURGO_Mapunits"), driver="ESRI Shapefile",
         overwrite_layer = TRUE)

# optional: save Rdata representation of get_ssurgo() result for further processing in R
save(your.ssurgo, file = paste0(project.label,".Rda"))