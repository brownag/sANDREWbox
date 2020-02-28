# dem-to-block_diagram.R

# last revised: 02/28/2020
# @authors: andrew paolucci, andrew brown, dylan beaudette

library(rayshader)
library(rgl)
library(RColorBrewer)

library(sf)
library(raster)

library(fasterize)
library(gstat)

#### SETUP

## 1. read shapefile for overlay (must cover full extent of elevation .TIF)
#       for example, ssurgo data symbolized on musym
thematic_shp <- st_read('dredge_ssurgo.shp', stringsAsFactors = FALSE)

## 2. thematic attribute - the column name in shapefile attribute table 
mu.col <- "MUSYM"

# group levels in mu.col to omit from result
omit.groups <- c("W")

## 3. digital elevation model (TIFF, or other raster-compatible format) for a chunk of space
#     e.g. pan to desired area in ArcMap, and Data > Export Data > By Data Frame
elev_orig <- raster('dredge_tailings.tif')

# if needed, define additional extent constraints (default uses full extent of DEM)

# example: take a small subset (1/100th) of the DEM
# extent.poly <- st_as_sf(as(extent(elev_orig) / 10, 'SpatialPolygons'))
# extent.poly <- st_set_crs(extent.poly, crs(elev_orig))

# example: load boundary from shapefile
#extent.poly <- st_read("sub_extent.shp", stringsAsFactors = FALSE)

## 4. OPTIONAL: resample raster input
target_resolution <- c(5,5) # define a coarser or finer resolution

## 5. OPTIONAL: Apply inverse-distance weighting interpolation to minimize DEM artifacts?
idw_smooth <- FALSE
focal_length <- 7 # size of focal window (an n x n square)
pct_dem_train <- 15 # random % of DEM to use in spatial interpolation (100% = exact match)
gstat.nmax <- 5 # number of neighbors to use in making prediction

#### END SETUP

# if extent polygon not defined, calculate from DEM
if(!exists("extent.poly"))
  extent.poly <- st_sf(bound=1, geom=st_as_sfc(st_bbox(elev_orig, crs=crs(elev_orig))))

# use extent polygon to crop and mask overlay shapefile and elevation
thematic_shp <- st_transform(thematic_shp, st_crs(elev_orig))
extent.poly <- st_transform(extent.poly, st_crs(elev_orig))
thematic_shp <- suppressWarnings(st_crop(thematic_shp, extent.poly))
thematic_shp <- st_cast(thematic_shp, 'MULTIPOLYGON')

my.mask <- mask(elev_orig, fasterize(extent.poly, elev_orig))
elev <- crop(my.mask, extent.poly)

## copy elevation raster
elev_template <- elev

## change raster resolution in template (leaving all else the same)
res(elev_template) <- target_resolution

## resample elevation raster to target resolution
if(!all(res(elev_template) == res(elev_orig))) {
  elev <- resample(elev, elev_template)
} 
names(elev) <- "elev"

  # inverse distance weighted interp using a subset of the data
if(idw_smooth) {
  # warning -- this can be very slow with detailed rasters....
  train.pt <- st_as_sf(as(elev, 'SpatialPoints'))
  
  # take percentage of the DEM pixels
  train.pt <- train.pt[sample(1:nrow(train.pt), size=floor(nrow(train.pt) / pct_dem_train)),]
  
  # fit a gstat model, using just location as predictor
  gs <-  gstat(formula = elev ~ 1, # predict elevation as function of location
               locations = train.pt, # random training subset of dem points
               nmax = gstat.nmax, # number of neighboring points
               set = list(idp = 0))
  
  # do inverse-distance weighted interpolation using gstat model and original raster
  elev_i <- interpolate(elev, gs)
  
  # inspect difference between interpolated and original
  #plot(elev - elev_i, col=heat.colors(12))
  
  # apply focal window median smoothing
  elev <- focal(elev_i, w=outer(rep(1, focal_length), rep(1, focal_length)), median)
}

# convert elevation raster -> matrix
elmat <- rayshader::raster_to_matrix(elev)

# calculate number of groups
grp <- unique(thematic_shp[[mu.col]])
n.grp <- length(grp)

# generate initial color palette with n.grp colors
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
first.colors <- sample(col_vector, n.grp)

if(exists(omit.groups) & length(omit.groups)) {
  first.colors[match(grp[grp %in% omit.groups], grp)] <- NA
  new.colors <- sample(col_vector, n.grp - sum(is.na(first.colors)))
  first.colors[!is.na(first.colors)] <- new.colors
}

# assign numeric value that is 1:1 with mukey
thematic_shp$munum <- match(thematic_shp[[mu.col]], unique(thematic_shp[[mu.col]]))

# produce raster
theme <- fasterize(thematic_shp, elev, field = 'munum')

# inspect raster representation of theme musym
plot(theme, col = first.colors)

#### Change color scheme

# CHECK: Plot initialcolor scheme
plot(theme, col = first.colors)

new.colors <- first.colors

# CHECK: Mapunit ID : Color pie chart
pie(rep(1, n.grp), 
    col = new.colors, 
    labels = paste(grp, ":", new.colors))

# replace individual colors (Optional) RGB Method colors[4] <- rgb(0,0,132/255)
# new.colors[1] <- "#E4A358" 
# new.colors[2] <- "#A0B7CB" 
# new.colors[3] <- "#A1CC7D" 
# new.colors[4] <- "#FFFFB3" 
# new.colors[5] <- "#FDBF6F" 
# new.colors[6] <- "#999999" 

# CHECK: inspect an individual color
barplot(c(1), col = new.colors[1])

# CHECK: new color scheme
plot(theme, col = new.colors)

# create RGB array from rasterized theme (mapunit colors)
tf <- tempfile()

old.par = par(no.readonly = TRUE)
on.exit(par(old.par))

fliplr <- function(x) { x[,ncol(x):1] }

png(tf, width = nrow(elmat), height = ncol(elmat))
  par(mar = c(0,0,0,0))
  raster::image(fliplr(raster_to_matrix(theme)), 
                axes = FALSE, 
                col = new.colors)
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
  plot_3d(elmat, zscale=0.8, fov=0, theta=30, water = 0,
          zoom=0.75, phi=45, windowsize = c(1000,800), lineantialias = TRUE)

# take a static picture of the rgl window
plot(1, type="n")
render_snapshot()

# generate a legend, add to plot
order.grp <- order(grp)
legend(x = "bottomleft", legend=grp[order.grp], fill=new.colors[order.grp])


