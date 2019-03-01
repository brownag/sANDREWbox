library(soilDB)
library(rgdal)

q1 <- paste0("SELECT legend.lkey, legend.areasymbol, mu.musym, mu.mukey, mu.muname, compname, comppct_r, geomfname, geomftname, cogd.rvindicator
             FROM legend 
             INNER JOIN mapunit mu ON mu.lkey = legend.lkey
             INNER JOIN component co ON mu.mukey = co.mukey
             INNER JOIN cogeomordesc cogd ON co.cokey = cogd.cokey
             WHERE legend.areasymbol = 'CA696';")



c <- SDA_query(q1)
c.sub <- subset(c, grepl(c$geomftname,pattern="Landform") & c$rvindicator == "Yes")
c.sub

q2 <- paste0("select G.MupolygonWktWgs84 as geom, mapunit.mukey, muname
            FROM mapunit
            CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
            WHERE mukey IN ",format_SQL_in_statement(unique(c.sub$mukey)))
res <- SDA_query(q2)
s <- processSDA_WKT(res)
plot(s)

dominant_condition <- function(df) {
  df2 <- aggregate(df$comppct_r, by=list(df$geomfname), FUN=sum)
  return(df2[which(df2$x == max(df2$x)),1])
}

#(lapply(split(c.sub, c.sub$mukey), function(d) { return(sum(d$comppct_r)) }))
foo <- lapply(split(c.sub, c.sub$mukey), dominant_condition)
foo2 <- data.frame(mukey=names(foo), dominant_landform=as.character(foo))

s.new <- merge(s, foo2, by="mukey")

writeOGR(s.new, dsn="E:/RAE_PresentationPictures/Geodata", layer="CA696_landform", driver="ESRI Shapefile", overwrite_laye =T)

