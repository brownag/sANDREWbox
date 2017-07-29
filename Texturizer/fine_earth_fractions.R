#fine-earth fraction ranges (v0.1 07/27/17 andrew brown)
# logic derived directly from NASIS calculation "Textural Class versus Particle Size Separates" written by Cathy Seybold (last updated 4/07/14)
# 
# PURPOSE: intended for use with the ternary fine earth fractions (sand, silt, clay) commonly estimated in the field; does NOT check sand fractions needed for vfs/fs/ms/cos/vcos subclasses. If these subclasses are used, the sand fraction modifier is assumed to be supported by the data/describer's experience. The sand, silt and clay fractions are still checked against the limits as if they were sand, loamy sand and sandy loam textural classes, respectively.

#texcl - vector of numeric values (NASIS codes) corresponding to a set of texture groups
# returns a range for each of sand, silt and clay corresponding to the CLASS LIMITS imposed by the set of texture groups
getFineEarthLimitsFromTextureGroupIDs <- function(texcl) {
  if(!is.na(texcl)) {
    buf <- matrix(nrow=3,ncol=2)
    rownames(buf) <- c("sand", "silt", "clay")
    colnames(buf) <- c("low", "high")
    buf[1,] <- c(getSandLow(texcl), getSandHigh(texcl))
    buf[2,] <- c(getSiltLow(texcl), getSiltHigh(texcl))
    buf[3,] <- c(getClayLow(texcl), getClayHigh(texcl))
    return(buf)
  } else return(NULL)
}

getFineEarthLimitsFromTextureClassName <- function(texclname) {
  texcl <- as.numeric(code(data.frame(texcl = tolower(texclname))))
  return(getFineEarthLimitsFromTextureGroupIDs(texcl))
}

#function suitable for profileApply
# skipNA (default TRUE) will not record errors for NA sand, silt, clay fractions. e.g. if only clay is populated, only clay range is checked
checkProfileFineEarthLimits <- function(pedon, skipNA = TRUE) {
  h <- horizons(pedon) 
  h <- cbind(data.frame(pedon_id=pedon$pedon_id), h)
  rez <- apply(h, 1, FUN=checkHorizonFineEarthLimits, skipNA)
  return(all(unlist(rez)))
}

is.between <- function(x, a, b) { (x - a)  *  (b - x) >= 0 }

checkHorizonFineEarthLimits <- function(horizon, skipNA = TRUE) {
  horizon <- (as.data.frame(t(horizon)))
  limitz <- getFineEarthLimitsFromTextureClassName(horizon$texture_class)
  if(!is.null(limitz)) { #limits will be null if the texture class is NA
    rez = c(F,F,F)
    fracz <- c(as.numeric(levels(horizon[['sand']])), as.numeric(levels(horizon[['silt']])), as.numeric(levels(horizon[['clay']])))
    if(length(fracz) < 3) return(FALSE)
    names(fracz) <- c("sand","silt","clay")
    for(ff in 1:3) {
      if(!is.na(fracz[ff])) 
        rez[ff] <- is.between(fracz[ff], limitz[ff,1], limitz[ff,2])
      else rez[ff] <- TRUE
    }
    # print(limitz)
    if(any(is.na(rez) & !skipNA)) return(FALSE)
    if(all(rez[!is.na(rez)])) {
      return(TRUE)
    } else {
      errfrac <- which(!rez)
      print(paste0(horizon$pedon_id,":",horizon$hzname," horizon ",names(fracz)[errfrac], " content (",fracz[errfrac],"%) is out of range (",limitz[errfrac,1],"-",limitz[errfrac,2],"%) for specified texture class (",toupper(horizon$texture_class),"). PID:",horizon$peiid,"|HID:",horizon$phiid))
      return(FALSE)
    }
  } else {
    if(!skipNA) return(FALSE)
  }
}

#NEW SOILDB type functions
#takes an SPC comprised of components and adds a field containing 
#     a list of the textural groups (non-RV included) allowed in this component horizon

fetchNASISComponentTexture <- function(coiids) {
  if(!requireNamespace('RODBC')) stop('please install the `RODBC` package', call.=FALSE)
  q <- "SELECT co.dmuiidref, co.coiid, chiidref as chiidref, chtgiid, cht.chtiid, texture, cht.texcl, cht.lieutex, stratextsflag, rvindicator FROM (chtexturegrp chtg
  INNER JOIN chtexture cht ON cht.chtgiidref = chtg.chtgiid
  INNER JOIN chorizon ch ON chtg.chiidref = ch.chiid
  INNER JOIN component_View_1 co ON co.coiid = ch.coiidref);"
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  RODBC::odbcClose(channel)
  return(d)  
}

#function suitable for profileApply
# skipNA works same as for pedon horizon
# The "Component" version of these functions has a few extra steps not found in the Pedon validation:
# 1. queries NASIS to get the texture group table
# 2. joins texture groups to supplied component coiid
# 3. constructs a range from all possible textures in texture group table for that component, and an RV range
# 4. verifies that fine earth Low and High in Component horizons correspond to listed texture groups for each horizon
# 5. verifies that RV fine earth fractions for each horizon fall within RV texture group.

appendTextureGroupDataToComponent <- function(hzs) {
  #TODO: fetchNASISComponentTextureXXX should take coiid and add "WHERE coiid == FOO" to prefilter query results on NASIS side 
  fnew <- fetchNASISComponentTexture()
  #key <- unique(data.frame(clazz=uncode(fnew)$texcl, id=fnew$texcl))
  col_tgrp <- aggregate(fnew$texcl,by=list(fnew$dmuiidref,fnew$coiid,fnew$chiidref),FUN=c)
  colnames(col_tgrp) <- c("dmuiid","coiid","chiid","texcl")
  return(merge(horizons(hzs), col_tgrp, by.x="chiid",by.y="chiid"))
}

checkComponentFineEarthLimits <- function(component, skipNA=TRUE) {
  h <- horizons(component) 
  rez <- apply(h, checkCHorizonFineEarthLimits, skipNA)
}

checkCHorizonFineEarthLimits <- function(chorizon, skipNA=TRUE) {
  horizon <- (as.data.frame(t(chorizon)))
  ##TODO: make query utilize coiid to prefilter
  
}

####
# LOGIC DERIVED FROM TEXTURAL CLASS VERSUS PARTICLE SIZE SEPARATES
####
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
  if(is.between(texcl,1,12) | any(texcl == 16 | texcl == 19 | texcl == 21)) return(0)
  else if(any(texcl == 17)) return(15)
  else if(any(texcl == 13)) return(28)
  else if(any(texcl == 20 | texcl == 18)) return(40)
  else if(any(texcl == 14)) return(50)
  else if(any(texcl == 15)) return(80)
  return(NA)
}

getSandHigh <- function(texcl) {
  if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(100)
  else if(is.between(texcl, 5, 8)) return(90)
  else if(is.between(texcl, 9, 12)) return(85)
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
  else if(is.between(texcl, 9, 12)) return(43)
  else if(any(texcl == 16 | texcl == 19)) return(45)
  else if(is.between(texcl, 5, 8)) return(70)
  else if(is.between(texcl, 1, 4)) return(85)
  return(NA)
}