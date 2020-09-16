library(sf)
library(raster)
library(fasterize)
library(stars)
library(dplyr)
library(ows4R)
library(ggplot2)

# web services info: https://fsapps.nwcg.gov/wms.php
# 
## 1km MODIS fire detection centroids for the continental United States for the current year.
# https://fsapps.nwcg.gov/afm/cgi-bin/mapserv.exe?map=conus_mod14_wfs.map
# 
# fire_pts <- read_sf("E:/Wildfire/SierraNF/CreekFire/CreekFire_MODIS_20200915.shp")

## 375m VIIRS I Band fire detection centroids for the continental United States for the current year.
# https://fsapps.nwcg.gov/afm/cgi-bin/mapserv.exe?map=conus_viirs_iband_wfs.map
# 
fire_pts <- read_sf("E:/Wildfire/SierraNF/CreekFire/CreekFire_VIIRSI_20200915.shp")

fire_pts$JULIAN <- as.numeric(fire_pts$JULIAN)

# raster templates based on 30m beam radiance rasters
str_template <- read_stars("C:/Geodata/project_data/ssro2_ann_beam_rad_int.tif")
r_template <- raster("C:/Geodata/project_data/ssro2_ann_beam_rad_int.tif")

# spatial extent of fire
fire_chl <- st_as_sf(st_convex_hull(st_union(fire_pts)))
fire_pts_sub <- filter(fire_pts, JULIAN >= 249)

# TODO: figure out how to do ~this with sf/stars
# r <- raster::raster(sf::as_Spatial(fire_chl))
# res(r) <- 0.01
# foo <- raster::rasterize(coordinates(sf::as_Spatial(fire_pts_sub)), r, fun = "count")
# plot(foo)

# temporal extent of fire

# create a grid of 10000 cells for point aggregation
fire_str <- st_as_stars(st_bbox(fire_chl), n = 10000)
fire_str[[1]][] <- NA
fire_ras <- st_rasterize(fire_pts_sub, fire_str, raster = "JULIAN")
plot(fire_ras)

# julian.days <- unique(fire_pts$JULIAN)
# julian.colors <- viridis::viridis(length(julian.days))
# names(julian.colors) <- julian.days

origin_pts <- st_sf(id = 19, geometry = st_sfc(st_point(c(-119.261175, 37.19147))))
st_crs(origin_pts) <- st_crs(fire_pts)

ggplot() +
  geom_stars(data = fire_ras) +
  geom_sf(data = origin_pts) +
  geom_sf(data = origin_pts, size = 3, fill = "RED", pch = 25) +
  scale_fill_continuous(name = "Julian Day", na.value = "transparent") +
  coord_sf()

ggplot() +
  geom_sf(data = fire_pts_sub, aes(col = JULIAN)) +
  geom_sf(data = origin_pts, size = 3, fill = "RED", pch = 25) +
  coord_sf() 

