library(soilDB)
library(rgdal)
#areasyms <- c("CA630")
problem.areas <- read.csv("S://NRCS//Archive_Andrew_Brown//Drafts//technical_team//Meeting_20181212//MLRA18_problem_components_20180522.csv")
mukeyz <- unique(problem.areas$MUKEY)

buf <- 0
for(k in 1:length(areasyms)) {
  AREA.SYM <- areasyms[k]
  q1 <- paste0("SELECT legend.lkey, legend.areasymbol, mu.musym, mu.mukey, mu.muname, compname, comppct_r, geomfname, geomftname, cogd.rvindicator
               FROM legend 
               INNER JOIN mapunit mu ON mu.lkey = legend.lkey
               INNER JOIN component co ON mu.mukey = co.mukey
               INNER JOIN cogeomordesc cogd ON co.cokey = cogd.cokey
               WHERE legend.areasymbol = 'CA628';")
  
  q.es <- paste0("SELECT legend.lkey, legend.areasymbol, mu.musym, mu.mukey, mu.muname, compname, comppct_r, ecoclassid
               FROM legend 
               INNER JOIN mapunit mu ON mu.lkey = legend.lkey
               INNER JOIN component co ON mu.mukey = co.mukey
               INNER JOIN coecoclass coes ON co.cokey = coes.cokey
               WHERE legend.areasymbol = \'",AREA.SYM,"\';")
  
  c <- SDA_query(q.es)
  c.sub <- c
  #c.sub <- subset(c, grepl(c$geomftname,pattern="Landform") & c$rvindicator == "Yes")
  #c.sub
  q2 <- paste0("select G.MupolygonWktWgs84 as geom, mapunit.mukey, muname
              FROM mapunit
              CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
              WHERE mukey IN ",format_SQL_in_statement(mukeyz),";")
  res <- SDA_query(q2)
  s <- processSDA_WKT(res)
  if(buf == 0) {
    buf <- s
  } else {
    buf <- rbind(buf, s)
  }
}
s <- buf

dominant_condition <- function(df, col.name) {
  df2 <- aggregate(df$comppct_r, by=list(df[,col.name]), FUN=sum)
  return(df2[which(df2$x == max(df2$x)),1])
}

#(lapply(split(c.sub, c.sub$mukey), function(d) { return(sum(d$comppct_r)) }))
foo <- lapply(split(c.sub, c.sub$mukey), dominant_condition, 'ecoclassid')
foo2 <- data.frame(mukey=names(foo), dominant_class=as.character(foo))

s.new <- merge(s, foo2, by="mukey")
plot(s.new[grepl(s.new$dominant_class, pattern="R018XI"),], col=factor(s.new$dominant_class))
plot(s.new[grepl(s.new$dominant_class, pattern="R018XD"),], col="YELLOW", add=TRUE)
CA630 <- s.new

#CA649 <- s.new
#writeOGR(s.new, dsn="E:/RAE_PresentationPictures/Geodata", layer="CA696_landform", driver="ESRI Shapefile", overwrite_laye =T)

