# dem-to-block_diagram.R
# 03/11/2019
# last revised: 02/27/2019
# @author: andrew brown; 
#          based on demo by dylan beaudette

library(rayshader)
library(rgl)
library(raster)
library(rgdal)
library(viridis)
library(sf)
library(fasterize)
library(gstat)

######
### SETUP
######

## 1. read shapefile for overlay (must cover full extent of elevation .TIF)
#       for example, ssurgo data symbolized on musym
thematic_shp <- readOGR(dsn='C:/PATH/TO/A/GEODATABASE.gdb',
                        layer="YOURLAYERNAME", stringsAsFactors = FALSE)

## 2. thematic attribute - the column name in shapefile attribute table 
mu.col <- "MUSYM"

# group levels in mu.col to omit from result
omit.groups <- c("W","8034","7078","7076","7079","7083","7085")

## 3. digital elevation model (TIFF, or other raster-compatible format) for a chunk of space
#     e.g. pan to desired area in ArcMap, and Data > Export Data > By Data Frame
elev_orig <- fasterize::raster('YOURDEM.tif')

## 4. OPTIONAL: resample raster input
target_resolution <- res(elev_orig) # c(5,5) # define a coarser or finer resolution

## 5. OPTIONAL: Apply inverse-distance weighting interpolation to minimize DEM artefacts?
idw_smooth <- FALSE
focal_length <- 7 # size of focal window (an n x n square)
pct_dem_train <- 15 # percentage of DEM to use in spatial interpolation (100% = exact match)

######
### END SETUP
######

## copy elevation raster
elev_template <- elev_orig

## change raster resolution in template (leaving all else the same)
res(elev_template) <- target_resolution

## resample elevation raster to desired template 
if(all(res(elev_template) == res(elev)))
  elev <- resample(elev_orig, elev_template)
names(elev) <- "elev"

  # inverse distance weighted interp using a subset of the data
if(idw_smooth) {
  train.pt <- as(elev, 'SpatialPointsDataFrame')
  
  # take 10% of the DEM pixels
  train.pt <- train.pt[sample(1:nrow(train.pt), size=floor(nrow(train.pt) / pct_dem_train)),]
  
  # fit a gstat model, using just location as predictor
  gs <-  gstat(formula=elev~1, locations=train.pt, nmax=5, set=list(idp=0))
  
  # do inverse-distance weighted interpolation using gstat model and original raster
  elev_i <- interpolate(elev, gs)
  
  # inspect difference between interpolated and original
  #plot(elev - elev_i, col=viridis(12))
  
  # apply focal window median smoothing
  elev <- focal(elev_i, w=outer(rep(1, focal_length), rep(1, focal_length)), median)
}

## save to file
# writeRaster(elev, filename='lidar_Tm_test_5m.tif')

######
### END SETUP
######

# convert elevation raster -> matrix
elmat <- rayshader::raster_to_matrix(elev)
  #matrix(extract(elev, extent(elev), buffer=1000), nrow=ncol(elev), ncol=nrow(elev))

# calculate (rectangular) boundary of DEM, use that to cut the overlay shapefile

# note: there is no specific reason your extent polygon has to be rectangular
#       but it is done here because rasters are commonly rectangular and we wan 
extent.poly <- as(extent(elev), 'SpatialPolygons')
proj4string(extent.poly) <-  proj4string(elev)
thematic_shp <- spTransform(thematic_shp, CRS(proj4string(elev)))
extent.poly <- spTransform(extent.poly, CRS(proj4string(elev)))
thematic_shp <- crop(thematic_shp, y = extent.poly)

# calculate number of groups
grp <- unique(thematic_shp[[mu.col]])
n.grp <- length(grp)

# generate initial color palette with n.grp colors
first.colors <- viridis(n.grp)

if(exists(omit.groups) & length(omit.groups)) {
  first.colors[match(grp[grp %in% omit.groups], grp)] <- NA
  new.colors <- viridis(n.grp - sum(is.na(first.colors)))
  first.colors[!is.na(first.colors)] <- new.colors
}

# assign numeric value that is 1:1 with mukey
thematic_shp$munum <- match(thematic_shp[[mu.col]], unique(thematic_shp[[mu.col]]))

# produce raster
#theme <- rasterize(x = thematic_shp, y = elev, 'munum')
theme <- fasterize(sf = st_as_sf(thematic_shp), raster = elev, field = 'munum')

# inspect raster representation of theme musym
plot(theme, col=first.colors)

# create RGB array from rasterized theme
tf <- tempfile()
old.par = par(no.readonly = TRUE)
on.exit(par(old.par))
png(tf, width = nrow(elmat), height = ncol(elmat))

fliplr = function(x) { x[,ncol(x):1] }

cols <- col2rgb(first.colors[values(theme)])

par(mar = c(0,0,0,0))
raster::image(fliplr(raster_to_matrix(theme)), 
              axes = FALSE, 
              col = first.colors)
dev.off()
load.array <- png::readPNG(tf)

# compute shadows
raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)

# with big files save the intermediates in case R session crashes
#save(raymat, ambmat, file = "intermediates.Rda")

# INTERACTIVE 3D PLOT WITH RGL

# set perspective with right-mouse + drag
# zoom with mouse wheel
# rotate with left-mouse + drag

# important to clear the rgl window if any settings are adjusted
rgl::rgl.clear()

# interactive 3D plot via rgl
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(load.array, alphalayer = 0.9) %>%
  #add_water(detect_water(elmat, cutoff = 0.99, min_area = 4000), color="desert") %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.4) %>%
  plot_3d(elmat, zscale=2, fov=0, theta=30, water = 0,
          zoom=0.75, phi=45, windowsize = c(1000,800), lineantialias = TRUE)

# take a static picture of the rgl window
render_snapshot()


