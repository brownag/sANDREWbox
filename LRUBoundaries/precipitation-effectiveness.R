# subhumid
library(raster)

precip <- brick("C:/Geodata/climate/raster/final_monthly_ppt_800m.tif")
mpet40 <- brick("C:/Geodata/climate/raster/monthly_pet_40degLat_800m.tif")

res <- brick(precip)
for(i in 1:12) {
  res[[i]] <- precip[[i]] / mpet40[[i]]
}

precip.effective.index <- 10*stackApply(res, indices=1, fun=sum, na.rm = TRUE)

writeRaster(precip.effective.index, "C:/Geodata/climate/raster/PEindex_40degLat_800m.tif", overwrite=TRUE)
