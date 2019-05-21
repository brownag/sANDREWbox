# SDA query - by component name, component kind and MLRA
library(soilDB)
library(rgdal)

soils <- c("Auburn","Bonanza","Dunstone","Exchequer","Millerton")
mlras <- c("18")

q <- paste0("select * from mapunit 
            INNER JOIN component ON component.mukey = mapunit.mukey
            INNER JOIN legend ON legend.lkey = mapunit.lkey
            WHERE compname IN ",format_SQL_in_statement(soils)," 
              AND compkind != 'family'
              AND majcompflag = 'Yes'
              AND compname NOT LIKE '%fami%' 
              AND muname NOT LIKE '%fami%'
              AND areasymbol != 'US';")
res <- SDA_query(q)

# use national mapunit symbol to make MUKEY lookup table
nmusyms <- unique(res$nationalmusym)
q <- paste0('select mukey, muname, nationalmusym, musym from mapunit 
            WHERE nationalmusym IN ', 
            format_SQL_in_statement(nmusyms),";")
mukey.lut.res <- SDA_query(q)
mukey.lut <- mukey.lut.res$mukey
names(mukey.lut) <- mukey.lut.res$nationalmusym
knitr::kable(mukey.lut.res)

# do SDA spatial query using MUKEY to create WKT 

# first, we need to chunk the MUKEYs (groups of 20 to get around SDA max query size limits)
mukey.lut.split <- split(mukey.lut, ceiling(seq_along(mukey.lut) / 20))
res2 <- do.call('rbind', lapply(mukey.lut.split, FUN = function(x) {
  q <- paste0("select G.MupolygonWktWgs84 as geom, mapunit.mukey, musym
    FROM mapunit CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
    WHERE mukey IN ",format_SQL_in_statement(x),";")
  return(SDA_query(q))
}))
s <- processSDA_WKT(res2)
plot(s)
plot(FedData::polygon_from_extent(s), add=T, border="RED")

# write to file
s <- spTransform(s, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
mukey.to.series <- do.call('rbind',lapply(split(res[,c('mukey','compname', 'comppct_r')], f = res$mukey), function(x) {
  idx <- which(x$comppct_r == max(x$comppct_r))[1]
  return(x[idx,])
}))
s <- merge(s, mukey.to.series, by="mukey")
rgdal::writeOGR(s, dsn = '.', layer='mvo_ssurgo',driver='ESRI Shapefile', overwrite_layer = T)
