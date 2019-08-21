# aridity index calculator
library(rgdal)
library(raster)
setwd('AridityIndices')
abr <- raster("test.tif")

pet <- brick("L:/NRCS/MLRAShared/Geodata/project_data/MUSum_PRISM/final_monthly_pet_800m.tif")
apet <- calc(pet / 10, fun = sum)


ppt <- brick("L:/NRCS/MLRAShared/Geodata/project_data/MUSum_PRISM/final_monthly_ppt_800m.tif")
ppt <- projectRaster(ppt, pet)

appt <- raster("L:/NRCS/MLRAShared/Geodata/project_data/MUSum_PRISM/final_MAP_mm_800m.tif")
appt <- projectRaster(appt, pet)

mat <- brick("L:/NRCS/MLRAShared/Geodata/project_data/MUSum_PRISM/final_monthly_tavg_800m.tif")

# UNEP
## AIu = P / PET
## NOTE: this method has been criticized due to use of annual mean P/PET
AIu <- appt / apet
plot(AIu)
writeRaster(AIu, "AIu.tif", overwrite=T)

wb <- stack(ppt, pet)

#Thornthwaite (1948) Aridity Index
## AIt = 100 * d/n
### where d is sum of monthly differences between precip and ET, for the months when precip is less than ET
###       n is the sum of potential ET for those months
AIt <- calc(wb, fun = function(x) {
  ppt <- x[1:12]
  pet <- x[13:24] / 10 #convert to mm
  idx <- which(pet > ppt)
  d <- abs(sum(ppt[idx] - pet[idx]))
  n <- sum(pet[idx])
  return(100 * d / n)
}, filename="AIt.tif", overwrite=T)
plot(AIt)

#Thornthwaite (1948) Heat Index
HIt <- calc(mat, fun=function(x) {
  sum((x / 5) ^ 1.514, na.rm = TRUE)
}, filename="HIt.tif", overwrite=TRUE)

# water deficit -- sum(pet - ppt) for months where pet > ppt - for Sam
wd <- calc(wb, fun = function(x) {
  ppt <- x[1:12]
  pet <- x[13:24] / 10 #convert to mm
  idx <- which(pet > ppt)
  d <- sum(ppt[idx] - pet[idx])
  return(d)
}, filename="water_deficit.tif", overwrite=T)
plot(wd)

# Mikhail Ivanovich Budyko (1958)
## AIb =  100*R/LP 
### where R is mean annual net radiation, P is mean annual precip, L is latent heat of vaporization of water

# 1 mm of water, spread over an area of 1 square meter, weights 0.999972kg @ 4 degrees C
# L = 2.260 MJ/kg
# R = annual beam radiance, corrected for reflection; multiply ABR by (1 - albedo) ~= factor of 0.7

net_abr <- abr * 0.7 # units: MJ/m^2; TODO: use raster of surface albedo derived from statsgo or similar
appt_r <- projectRaster(appt, net_abr)
writeRaster(crop(appt_r, extent(net_abr)), "appt_90m.tif", overwrite=TRUE)
bb <- brick(net_abr, appt_r * 0.001) # convert precip into units of meters
AIb <- calc(bb, fun = function(x) {
  return(x[1] / (2.260 * x[2]*0.999972)) # removed factor of 100
}, filename="AIb.tif", overwrite=T)
plot(AIb)
AIb
