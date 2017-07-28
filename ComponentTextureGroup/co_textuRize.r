#takes an SPC comprised of components and adds a field containing 
#     a list of the textural groups (non-RV included) allowed in this component horizon
fetchNASISComponentTextureGroups <- function() {
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT chiidref as chiidref, chtgiid, texture, stratextsflag, rvindicator FROM chtexturegrp;"

  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  RODBC::odbcClose(channel)
  return(d)  
}

fetchNASISComponentTextures <- function() {
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT chtgiidref as chtgiidref, chtiid, texcl, lieutex FROM chtexture;"
  
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  RODBC::odbcClose(channel)
  return(d)  
}
library(soilDB )
f <- fetchNASIS_component_data(rmHzErrors = F)
fnew <- merge(fetchNASISComponentTextureGroups(), uncode(fetchNASISComponentTextures()), by.x = "chtgiid", by.y = "chtgiidref")
# for(ff in 1:nrow(fnew)) { 
#   if(is.na(fnew[ff,]$texcl)) {
#     fnew[ff,]$texcl <- fnew[ff,]$lieutx
#   }#TODO: in lieu textures? retain in parallel data struct, cant mix or factor coding will need to be handled
# }
# fnew$texcl <-  factor(fnew$texcl)
key <- setNames(1:length(levels(fnew$texcl)),levels(fnew$texcl))
col_tgrp <- aggregate(names(key)[fnew$texcl],by=list(fnew$chiidref),FUN=c)
colnames(col_tgrp) <- c("chiid","cotxt_grp")
horizons(f) <- merge(horizons(f), col_tgrp, by.x="chiid")

#texcl - vector of numeric values (NASIS codes) corresponding to a set of texture groups
# returns a range for each of sand, silt and clay corresponding to the CLASS LIMITS imposed by the set of texture groups
getFineEarthLimitsFromTextureGroup <- function(texcl) {
  buf <- matrix(nrow=3,ncol=2)
  rownames(buf) <- c("sand","silt","clay")
  colnames(buf) <- c("low", "high")
  buf[1,] <- c(getSandLow(texcl),getSandHigh(texcl))
  buf[2,] <- c(getSiltLow(texcl),getSiltHigh(texcl))
  buf[3,] <- c(getClayLow(texcl),getClayHigh(texcl))
  return(buf)
}

getClayHigh <- function(texcl) {
  if(any(texcl == 21)) return(100) #clay
  else if(any(texcl == 20)) return(60)  #sic
  else if(any(texcl == 19)) return(55)  #sc
  else if(any(texcl == 18 | texcl == 17)) return(40)  #cl, sicl
  else if(any(texcl == 16)) return(35)  #scl
  else if(any(texcl == 14 | texcl == 13)) return(27)  #l, sil
  else if(any(texcl == 12 | texcl == 11 | texcl == 10 | texcl == 9)) return(20)  #sl, fsl, vfsl, cosl
  else if(any(texcl == 8 | texcl == 7 | texcl == 6 | texcl == 5)) return(15)  #ls, lvfs, lfs, lcos
  else if(any(texcl == 15)) return(12) #si
  else if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(10)
  return(NA)
}
getClayLow <- function(texcl) {
  if(any(texcl <=12 & texcl >= 1) | any(texcl == 14 | texcl == 15)) return(0)
  else if(any(texcl == 13)) return(7)
  else if(any(texcl == 16)) return(20)
  else if(any(texcl == 17 | texcl == 18)) return(27)
  else if(any(texcl == 19)) return(35)
  else if(any(texcl == 20 | texcl == 21)) return(40)
  return(NA)
}

getSiltHigh <- function(texcl) {
  if(any(texcl == 15)) return(100)
  else if(any(texcl == 14)) return(88)
  else if(any(texcl == 18)) return(73)
  else if(any(texcl == 20)) return(60)
  else if(any(texcl == 17)) return(53)
  else if(any(texcl <= 13 & texcl >= 9)) return(50)
  else if(any(texcl == 21)) return(40)
  else if(any(texcl <= 8 & texcl >=5)) return(30)
  else if(any(texcl == 16)) return(28)
  else if(any(texcl == 19)) return(20)
  else if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(15)
  return(NA)
}
getSiltLow <- function(texcl) {
  if(any(texcl <= 12 & texcl >= 1) | any(texcl == 16 | texcl == 19 | texcl == 21)) return(0)
  else if(any(texcl == 17)) return(15)
  else if(any(texcl == 13)) return(28)
  else if(any(texcl == 20 | texcl == 18)) return(40)
  else if(any(texcl == 14)) return(50)
  else if(any(texcl == 15)) return(80)
  return(NA)
}
getSandHigh <- function(texcl) {
  if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(100)
  else if(any(texcl <= 8 & texcl >= 5)) return(90)
  else if(any(texcl <= 12 & texcl >= 9)) return(85)
  else if(any(texcl == 16)) return(80)
  else if(any(texcl == 19)) return(65)
  else if(any(texcl == 13)) return(52)
  else if(any(texcl == 14)) return(50)
  else if(any(texcl == 17 | texcl == 21)) return(45)
  else if(any(texcl == 15 | texcl == 18 | texcl == 20)) return(20)
  return(NA)
}

getSandLow <- function(texcl) {
  if(any(texcl == 21 | texcl == 20 | texcl == 18 | texcl == 14 | texcl == 15)) return(0)
  else if(any(texcl == 17)) return(20)
  else if(any(texcl == 13)) return(23)
  else if(any(texcl <= 12 & texcl >= 9)) return(43)
  else if(any(texcl == 16 | texcl == 19)) return(45)
  else if(any(texcl <= 8 & texcl >= 5)) return(70)
  else if(any(texcl <= 4 & texcl >= 1)) return(85)
  return(NA)
}

