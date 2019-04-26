library(rasterVis)

## clean using rgeos zerobuffer
library(rgdal)
library(raster)
test_shp <- readOGR(dsn = 'S:/NRCS/Archive_Andy_Paolucci/BarnegatBay/BB_soils.shp')
elev <- raster('S:/NRCS/Archive_Andy_Paolucci/BarnegatBay/bbrasterdem_clip.tif')

# use rgeos gBuffer (of width near zero) to fix geometry
clean_shp <- rgeos::gBuffer(test_shp, byid=TRUE, width = 0)

# save result
writeOGR(clean_shp, dsn = 'S:/NRCS/Archive_Andy_Paolucci/BarnegatBay', 
         layer="BB_soils_cleanbuf", driver="ESRI Shapefile", overwrite_layer = TRUE)

# did it work?
clean_shp$munum <- match(clean_shp$MUSYM, unique(clean_shp$MUSYM))
clean_shp <- spTransform(clean_shp, CRS(proj4string(elev)))
r <- rasterize(x=clean_shp, y=elev, field='munum')
plot(r)

######
######
######

## clean using sf zerobuffer + lwgeom st_make_valid
library(sf)
library(lwgeom)

# convert sp object to sf (simple features)
test_sf <- st_as_sf(test_shp)

# are any features invalid? [yes]
validity <- st_is_valid(test_sf, reason = TRUE)
invalid.idx <- which(validity != "Valid Geometry")
invalid.reason <- unique(validity[invalid.idx])
data.frame(RECORD_ID=invalid.idx, REASON=invalid.reason)

# plot the polygons that are invalid for above reasons
plot(as(test_sf[invalid.idx, ], 'Spatial'))

## do the cleaning -- two ways 
clean_sf_sp_buf <- as(st_buffer(test_sf, 0.0), 'Spatial')
clean_sf_sp_make <- as(st_make_valid(test_sf), 'Spatial')

writeOGR(clean_sf_sp_buf, dsn = 'S:/NRCS/Archive_Andy_Paolucci/BarnegatBay', 
         layer="BB_soils_cleansf_buf", driver="ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(clean_sf_sp_make, dsn = 'S:/NRCS/Archive_Andy_Paolucci/BarnegatBay', 
         layer="BB_soils_cleansf_make", driver="ESRI Shapefile", overwrite_layer = TRUE)

# did it work?
clean_sf_sp_buf$munum <- match(clean_sf_sp_buf$MUSYM, unique(clean_sf_sp_buf$MUSYM))
clean_sf_sp_buf <- spTransform(clean_sf_sp_buf, CRS(proj4string(elev)))
r <- rasterize(x=clean_sf_sp_buf, y=elev, field='munum')
levelplot(r)

clean_sf_sp_make$munum <- match(clean_sf_sp_make$MUSYM, unique(clean_sf_sp_make$MUSYM))
clean_sf_sp_make <- spTransform(clean_sf_sp_make, CRS(proj4string(elev)))
r <- rasterize(x=clean_sf_sp_make, y=elev, field='munum')
levelplot(r)
contourplot(elev) + layer(sp.polygons(as(test_sf[invalid.idx, ], 'Spatial'), fill = "yellow"))
