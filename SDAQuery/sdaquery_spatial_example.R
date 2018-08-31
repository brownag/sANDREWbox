library(soilDB)
q <- "select G.MupolygonWktWgs84 as geom, mapunit.mukey, muname
FROM mapunit
CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
WHERE mukey IN ('1403430','461933','460904','463113', '462675', '462104')"

# Peters 0-8 mini-eval: 
## CA067189 CA077212 CA644PtB  CA645PrB  CA648PnB (NOT IN ORDER) ('1403430','461933','460904','463113', '462675', '462104')

res <- SDA_query(q)
s <- processSDA_WKT(res)
plot(s)

mu_source <- 'L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb' # polygon feature path 
mu_layer <- 'ca630_a' # polygon feature class for overlay NASIS pedons 
library(rgdal)
mu <- readOGR(dsn = mu_source, layer = mu_layer, stringsAsFactors=FALSE)
mu <- spTransform(mu, CRS(proj4string(s)))
plot(mu, add=T)
writeOGR(s, dsn=".", layer="peters", driver="ESRI Shapefile", overwrite_layer=T)
