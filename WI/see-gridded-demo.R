library(raster)
library(rasterVis)
library(viridis)
library(sp)
library(rgdal)
library(soilDB)

see <- seriesExtent('magnor')
r <- raster('S:/NRCS/Archive_Dylan_Beaudette/SoilWeb/series-extent/gridded-version/magnor.tif')

see <- spTransform(see, CRS(projection(r)))

levelplot(
  r,
  col.regions=viridis,
  main = 'MAGNOR',
  margin = FALSE, 
  scales = list(draw = FALSE),
  panel=function(...) {
    panel.levelplot(...)
    sp.polygons(see, col='red', lwd=1)
  }
  )
