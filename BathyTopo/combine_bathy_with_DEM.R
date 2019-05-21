# A simple routine for combining terrestrial DEM and bathymetric DEM
# @author: andrew brown

library(raster)

# load input rasters
dem <- raster("L:/NRCS/MLRAShared/Geodata/project_data/MUSum_10m_MLRA/DEM_TEM_int_AEA.tif")
bth <- raster("S:/NRCS/Archive_Andrew_Brown/Projects/MorroBay/MorroBathy/MorroBathy/morro_bathy1.tif")

# project bathymetry to match terrestrial DEM
bth.p <- projectRaster(bth, crs = CRS(proj4string(dem)))

# crop DEM to match extent of (reprojected) bathymetry
dem.c <- crop(dem, FedData::polygon_from_extent(bth.p))

# reproject terrestrial DEM to match grid/resolution of bathymetry
dem.p <- projectRaster(dem.c, bth.p)

# scale datum of bathymetry so maximum value from bathymetry = elevation 0 in DEM
values(bth.p) <- values(bth.p) - max(values(bth.p), na.rm=T)

final_dem <- dem.p
buf <- values(final_dem)
buf[!is.na(values(bth.p))] <- bth.p[!is.na(values(bth.p))]
values(final_dem) <- buf
plot(final_dem)

writeRaster(final_dem, "test.tif", overwrite=T)
