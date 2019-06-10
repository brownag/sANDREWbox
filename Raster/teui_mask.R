# raster mask for TEUI example
# @author: andrew brown

library(raster)
library(rgdal)

# load an input raster/raster stack
# for example: modeled design freezing index rating from CDEC data for the Sierra Nevada
ras <- raster("S:/NRCS/Archive_Andrew_Brown/Scripts/DFI90_CDEC_800m.tif")

# load a shapefile (subset of the raster extent)
# for example: the soil survey area boundary from CA630
ext <- readOGR("L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/final/FG_CA630_GIS_2018.gdb", "ca630_b")

# reproject shapefile to match projection of raster
ext.transformed <- spTransform(ext, CRS(proj4string(ras)))

# look at full extent of raster and mask area
plot(ras)
lines(ext.transformed)

# create a mask
mymask <- mask(ras, ext.transformed)

# see that we have values in just the CA630 extent
plot(mymask)

# use raster algebra to convert all NON-NA values to 1 (i.e. they are in the extent)
mymask2 <- calc(mymask, function(v) return(as.numeric(!is.na(v))))

# look at the mask
plot(mymask2)

# write to file for use in TEUI
writeRaster(mymask2, filename = "ca630_teui_mask.tif")


