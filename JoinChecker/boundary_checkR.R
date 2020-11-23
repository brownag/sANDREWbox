# boundary checker
library(sf)
gdb.path <- "E:/CA649/Geodata/Offical_Geodatabase/FGCA649_Projects_2020_0921_agb.gdb"

# load target boundary
boundary <- read_sf(dsn = gdb.path, layer = "ca649_b")
boundline <- st_cast(boundary, "MULTILINESTRING")
boundpt <- st_cast(boundline, "POINT")

# load new geodatabase
newgdb <- read_sf(dsn = gdb.path, layer = "ca649_a")


# bbuffer <- st_buffer(boundline, 0.000001)
# gdbbuffer <- st_intersection(newgdb, bbuffer)
gdbbuffer <- st_union(newgdb)

res <- st_cast(st_cast(gdbbuffer, "MULTIPOINT"), "POINT")
plot(res[,"AREASYMBOL"])

res.sub <- boundpt[which(!boundpt$Shape %in% res),]
plot(res.sub[,"AREASYMBOL"])

rgdal::writeOGR(as_Spatial(st_as_sf(res.sub)),dsn = "JoinChecker",driver = "ESRI Shapefile",
                layer = "boundary_vertex_QC", overwrite_layer = TRUE)

rgdal::writeOGR(as_Spatial(st_as_sf(boundpt)),dsn = "JoinChecker",driver = "ESRI Shapefile",
                layer = "boundary_vertex", overwrite_layer = TRUE)
