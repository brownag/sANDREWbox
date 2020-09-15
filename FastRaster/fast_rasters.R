library(raster)
library(fasterize)
library(exactextractr)

library(sf)
library(dplyr)
library(ggplot2)
library(rasterVis)


### LOAD SSURGO DATA AND CONVERT TO 10m raster

# some polygons associated with an MLRA project
polygons <- sf::read_sf("E:/CA649/workflow/output/working_progress.shp") 

# extract musym information, create factor
polygons$musym <- as.factor(polygons$musym)
polynames <- levels(polygons$musym)
polycodes <- 1:length(polynames)

### convert SSURGO feature class to raster (10m) quickly with fasterize()
rpolygons <- fasterize::fasterize(polygons, field = "musym", raster(polygons, res = 10))
rpolygons <- ratify(rpolygons)

# set up raster attribute table for symbols
rat <- levels(rpolygons)[[1]]
rat$musym <- polynames
rat$code <- polycodes
levels(rpolygons) <- rat

musym.colors <- viridis::viridis(length(polynames))
names(musym.colors) <- as.character(polycodes)

# use rasterVis gplot() [ wrapper around ggplot() for RasterLayer ]
rasterVis::gplot(rpolygons) +
  geom_raster(aes(fill = as.factor(value)), show.legend = T) +
  scale_fill_discrete(type = musym.colors, labels = polynames,
                      na.value = "transparent") +
  coord_equal()

######
######
######

### SUMMARIZE SLOPE GRADIENT BY DELINEATION

# load a slope raster (10m resolution)
rslope <- raster("C:/Geodata/project_data/MUSum_10m_MLRA/Slope_SON_int_AEA.tif")

# quickly extract raster values by delineation
rslope_ext <- exact_extract(rslope, polygons, include_xy = TRUE)

# note: we get a coverage fraction and an XY position for each value 
head(rslope_ext[[1]])

# iterate over delineations, calculate the [weighted] quantiles for data from each delineation
rslope_qua <- do.call('rbind', lapply(rslope_ext, function(x) {
  quantile(x$value, probs = c(0,0.05,0.5,0.95,1), names = FALSE)
}))

# combine spatial data with delineation summaries
rslope_cmb <- cbind(polygons, Q = rslope_qua)

# lets look at just a single mapunit symbol
rslope_cmb_sub <- filter(rslope_cmb, musym == "7092")

# colored by median slope
ggplot() + 
  geom_sf(data = rslope_cmb_sub, aes(fill = Q.3))
