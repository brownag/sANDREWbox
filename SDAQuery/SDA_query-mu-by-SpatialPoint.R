library(soilDB)
library(rgdal)

#get just site records 
f <- fetchNASIS()

coordinates(f) <- ~ x_std + y_std
proj4string(f) <- '+proj=longlat +datum=WGS84'

res <- SDA_spatialQuery(f@sp, what="geom")
res.tab <- SDA_spatialQuery(f@sp)
res <- merge(res, res.tab, by="mukey")

res2 <- as.data.frame(res[over(f@sp, res)$gid,])
res2$series <- toupper(f$taxonname)

#add a hornitos mapunit from CA648
res2 <- rbind(res2[,-2], data.frame(series="Hornitos", mukey=462951, muname="Hornitos gravelly fine sandy loam, 0 to 8 percent slopes"))

res2 <- res2[order(res2$series),]

res2
paste0(unique(res2$mukey),collapse = ",")
