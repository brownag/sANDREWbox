#map of dominant orders
library(rgdal)
library(soilDB)
library(dplyr)

ca630_b <- readOGR(dsn="L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/FG_CA630_GIS_2018_0818_TKK.gdb",layer="ca630_b")
ca630_a <- readOGR(dsn="L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/FG_CA630_GIS_2018_0818_TKK.gdb",layer="ca630_a")

get_legend_from_NASIS_db <- function() {
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  q <- "SELECT nationalmusym, dmuiid, coiid, musym, muname, mustatus, compname, comppct_r, taxorder FROM legend AS l
          INNER JOIN lmapunit AS lmu ON lmu.liidref = l.liid
          INNER JOIN mapunit AS mu ON mu.muiid = lmu.muiidref
          INNER JOIN correlation AS cor ON cor.muiidref = mu.muiid
          INNER JOIN datamapunit AS dmu ON dmu.dmuiid = cor.dmuiidref
          INNER JOIN component AS co ON co.dmuiidref = dmu.dmuiid 
        WHERE cor.repdmu = 1 AND lmu.mustatus != 4"
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  RODBC::odbcClose(channel)
  return(d)
}

l <- uncode(get_legend_from_NASIS_db())
l$taxorder <- as.character(l$taxorder)
l$taxorder[is.na(l$taxorder)] <- "not-assigned"
dominant_order <- ldply(split(l, l$musym), .fun=function(x) {
  foo <- aggregate(x$comppct_r, by=list(x$taxorder), FUN=sum)
  dom <- which(foo[,2] == max(foo[,2]))
  return(foo[dom[1], 1])
})
names(dominant_order) <- c("MUSYM", "dominantorder")
dolist <- list()
dolist[dominant_order$MUSYM] <- dominant_order$dominantorder

ca630_a$dominant_order <- as.character(dolist[ca630_a$MUSYM])
writeOGR(ca630_a, dsn=paste0(getwd(), '/DominantTaxa'), layer="ca630_dominantorder",driver="ESRI Shapefile", overwrite=T)
