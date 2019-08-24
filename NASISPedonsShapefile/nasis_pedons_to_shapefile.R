library(soilDB)
library(sp)
library(rgdal)

#makes your NASIS selected set into a soil profile collection and a shapefile

custom.layername <- "osdtudpedon_CA729"
default.layername <- paste0('nasis_points_', format(Sys.time(), '%Y%m%d'))

layername <- default.layername
if(custom.layername != "")
  layername <- custom.layername

x <- fetchNASIS()
good.idx <- which(!is.na(x$x_std))
x.bad <- x[-good.idx, ]
x <- x[good.idx, ]
coordinates(x) <- ~ x_std + y_std
proj4string(x) <- '+proj=longlat +datum=NAD83'
x.sub <- as(x, 'SpatialPointsDataFrame')

# overlay on shapefile (eg. ssa boundary)
#bd <- readOGR(dsn=".", layer="ca729_b")
#bd <- spTransform(bd, CRS(proj4string(x.sub)))
#x.sub$IN_CA729 <- !is.na(over(x.sub, bd)$AREASYM)

writeOGR(x.sub, dsn=getwd(), layer = layername, driver='ESRI Shapefile', overwrite_layer = T)
