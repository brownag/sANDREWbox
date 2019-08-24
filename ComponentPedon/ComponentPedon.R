#fetch matching NASIS component data component pedon table, take RV and get the site data from copedon
fetchNASIS_copedons <- function(rv=FALSE) {
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  q <- "SELECT * FROM copedon"
  if(rv) {
    q <- paste0(q, " WHERE rvindicator = 1;")
  } else q <- paste0(q, ";")
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  RODBC::odbcClose(channel)
  return(d)
}

get_legend_from_NASIS_db <- function() {
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  q <- "SELECT nationalmusym, dmuiid, dmudesc, coiid, musym, muname, mustatus, muacres FROM legend AS l
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

library(soilDB)
nasis.copedons <- fetchNASIS_copedons(rv = T)
nasis.pedons <- fetchNASIS_pedons()
rv.pedons <- merge(site(nasis.pedons), nasis.copedons, all.y=T, by.x="peiid", by.y="peiidref")
rv.pedons.sub <- rv.pedons[which(rv.pedons$pedonpurpose == "laboratory sampling site"),]
coordinates(rv.pedons.sub) <- ~ x_std + y_std
proj4string(rv.pedons.sub) <- "+proj=longlat +datum"



df <- merge(site(fetchNASIS_components()), get_legend_from_NASIS_db(), all.x=T)
df.sub <- df[which(df$compname == 'Copperopolis'),]
df.agg <- data.frame(cbind(aggregate(df.sub$muacres, by=list(df.sub$coiid), FUN=function(x) x[1]), pct=aggregate(df.sub$comppct_r, by=list(df.sub$coiid), FUN=function(x) x[1])[,2]))
total.acres <- sum(df.agg$x * df.agg$pct/100, na.rm=T)
total.acres
