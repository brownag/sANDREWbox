install.packages("grainchanger")
library(grainchanger)
library(sf)
library(raster)
r <- raster('C:/Geodata/project_data/MUSum_10m_SSR2/SSR2_Slope10m_AEA.tif')
load('../MLRAProjectTools/mupoly_bak.Rda')
s.x <- spTransform(s, CRS(proj4string(r)))
r.new <- crop(r, s.x)
r <- r.new
r.new.template <- r.new
res(r.new.template) <- 800
r.new <- resample(r.new, r.new.template)

# moving-window aggregation using Shannon evenness
foo <- winmove_agg(g = r.new, dat = r, d = 10,
                         type = "rectangle",
                         fun = "var_range")
r.new$foo <- foo

library(ggplot2)
ggplot(r.new) + geom_sf(aes(fill = foo))
