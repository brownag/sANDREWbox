library(soilDB)
library(sp)
library(rgdal)

#makes your NASIS selected set into a soil profile collection and a shapefile

x <- fetchNASIS()
good.idx <- which(!is.na(x$x_std))
x.bad <- x[-good.idx, ]
x <- x[good.idx, ]
coordinates(x) <- ~ x_std + y_std
proj4string(x) <- '+proj=longlat +datum=NAD83'
x.sub <- as(x, 'SpatialPointsDataFrame')

writeOGR(x.sub,dsn=getwd(),layer = paste0('nasis_points_', format(Sys.time(), '%Y%m%d')), driver='ESRI Shapefile')
