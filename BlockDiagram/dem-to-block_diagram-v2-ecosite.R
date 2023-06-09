# DEM to Block Diagram v2
#  
# last revised: 06/09/2023
#               tested and working w/ terra 1.7.29; rayshader 0.35.7
# @authors: andrew brown, andrew paolucci, dylan beaudette
#
library(rgl)
library(rayshader) 
library(sf)
library(terra)
library(soilDB)

#### SETUP

## 1. digital elevation model (TIFF, or other raster-compatible format) for a chunk of space
#     e.g. pan to desired area in ArcMap, and Data > Export Data > By Data Frame
elev_orig <- rast('demo_dem.tif')

## 2. OPTIONAL: resample raster input
target_resolution <- c(5, 5) # define a coarser or finer resolution

## 3. read shapefile for overlay (must cover full extent of elevation .TIF)
#       for example, ssurgo data symbolized on musym
# thematic_shp <- try(st_read(dsn = '.', layer = 'demo_ssurgo', stringsAsFactors = FALSE))
thematic_shp <- st_as_sf(soilDB::SDA_spatialQuery(elev_orig, what = "mupolygon"))

#### Optional: load boundary from shapefile
# extent.poly <- st_read("sub_extent.shp", stringsAsFactors = FALSE)

## 4. thematic attribute - the column name in shapefile attribute table 
mu.col <- "ecoclassid"

# group levels in mu.col to omit from result
omit.groups <- c("Not assigned")
omit.color <- "white"
  
# amend thematic layer
if (inherits(thematic_shp, 'try-error')) {
  thematic_shp <- st_as_sf(soilDB::SDA_spatialQuery(st_as_sfc(st_bbox(thematic_shp)), what = "mupolygon"))
  thematic_shp$MUSYM <- thematic_shp$mukey
}

# handling for mapping ecositeid or name
if (mu.col %in% c("ecoclassid", "ecoclassname")) {
  thematic_shp <- merge(
    thematic_shp,
    soilDB::get_SDA_coecoclass(mukeys = thematic_shp$mukey, method = "dominant condition")
  )
}

# if extent polygon not defined, calculate from DEM
if (!exists("extent.poly")) {
  extent.poly <- st_sf(bound = 1, geom = st_as_sfc(st_bbox(elev_orig, crs = crs(elev_orig))))
}

# use extent polygon to crop and mask overlay shapefile and elevation
thematic_shp <- st_transform(thematic_shp, st_crs(elev_orig))
extent.poly <- st_transform(extent.poly, st_crs(elev_orig))
thematic_shp <- suppressWarnings(st_crop(thematic_shp, extent.poly))
thematic_shp <- st_cast(thematic_shp, 'MULTIPOLYGON')

my.mask <- terra::mask(elev_orig, terra::rasterize(vect(extent.poly), elev_orig))
elev <- terra::crop(my.mask, extent.poly)

## copy elevation raster
elev_template <- elev

## change raster resolution in template (leaving all else the same)
res(elev_template) <- target_resolution

## resample elevation raster to target resolution
if (!all(res(elev_template) == res(elev_orig))) {
  elev <- resample(elev, elev_template)
} 

names(elev) <- "elev"

# convert elevation raster -> matrix
elmat <- rayshader::raster_to_matrix(raster::raster(elev))

# calculate number of groups
grp <- unique(thematic_shp[[mu.col]])
n.grp <- length(grp)

col_vector <- rev(hcl.colors(ifelse(length(grp) < 3, 3, n.grp), 'cividis')[seq_len(n.grp)])
names(col_vector) <- unique(thematic_shp[[mu.col]])

col_vector[names(col_vector) %in% omit.groups] <- omit.color

# replace individual colors (Optional) 
## RGB Method col_vector[4] <- rgb(0,0,132/255)
#col_vector[1] <- "#E4A358" 
#col_vector[2] <- "#A0B7CB" 
#col_vector[3] <- "#A1CC7D" 
#col_vector[4] <- "#FFFFB3" 
#col_vector[5] <- "#FDBF6F" 
#col_vector[6] <- "#999999" 

ov <- rayshader::generate_polygon_overlay(geometry = thematic_shp, 
                                          extent = raster::extent(raster::raster(elev)), 
                                          heightmap = elmat,
                                          data_column_fill = mu.col, 
                                          palette = col_vector[1:n.grp],
                                          linewidth = 2,
                                          linecolor = "white")

raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)

# important to clear the rgl window if any settings are adjusted
rgl::clear3d()

# interactive 3D plot via rgl
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_overlay(ov, rescale_original = TRUE) %>%
  add_shadow(raymat, max_darken = 0.4, rescale_original = TRUE) %>%
  # add_shadow(ambmat, max_darken = 0.4, rescale_original = TRUE) %>%
  plot_3d(
    elmat,
    zscale = 5,
    fov = 0,
    theta = 30,
    water = 0,
    zoom = 0.75,
    phi = 45,
    soil = TRUE,
    windowsize = c(1000, 800),
    lineantialias = TRUE
  )

# take a static picture of the rgl window
plot(1, type = "n")
render_snapshot()

# generate a legend, add to plot
order.grp <- order(grp)
legend(x = "bottomleft",
       legend = grp[order.grp], 
       bg = "white",
       fill = col_vector[order.grp])



