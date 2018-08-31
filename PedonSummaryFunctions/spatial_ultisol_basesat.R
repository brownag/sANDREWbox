#check ultisol grab sample locations
library(soilDB)
library(raster)
p <- fetchNASIS_pedons()
p.sub <- p[grepl(p$site_id,pattern="^R.*"),]

k <- fetchKSSL(pedlabsampnum = p.sub$pedlabsampnum[1])
for(i in p.sub$pedlabsampnum[2:nrow(site(p.sub))]) 
  k <- rbind(k, fetchKSSL(pedlabsampnum = i))
#k.na <- k[which(is.na(k$x)),]
#k <- k[-which(is.na(k$x)),]
coordinates(k) <- ~ x + y
proj4string(k) <- "+proj=longlat +ellps=WGS84"

source("PedonSummaryFunctions\\pedon_summary_functions.R") 
source("PedonSummaryFunctions\\fine_earth_fractions.R")

eff_p <- raster('L:/NRCS/MLRAShared/Geodata/climate/raster/effective_precipitation_800m.tif')
map_p <- raster('L:/NRCS/MLRAShared/Geodata/climate/raster/final_MAP_mm_800m.tif')

eff_p1 <- extract(y=k@sp,eff_p)
map_p1 <- extract(y=k@sp,map_p)

summary(lm(~eff_p1))
k$minbs <- as.numeric(profileApply(k,FUN=function(p) return(min(p$bs82))))

plot(k@sp, pch=19)
points(k@sp[k$minbs <= 35], col="RED", pch=19)
points(k@sp[], col="BLUE", pch=19)
k$taxonname[k$minbs <= 35]
