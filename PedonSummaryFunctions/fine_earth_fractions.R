library(dplyr)
library(plyr)
#fine-earth fraction ranges (v0.1 07/27/17 andrew brown)
# logic derived directly from NASIS calculation "Textural Class versus Particle Size Separates" written by Cathy Seybold (last updated 4/07/14)
# 
# PURPOSE: intended for use with the ternary fine earth fractions (sand, silt, clay) commonly estimated in the field; does NOT check sand fractions needed for vfs/fs/ms/cos/vcos subclasses. If these subclasses are used, the sand fraction modifier is assumed to be supported by the data/describer's experience. The sand, silt and clay fractions are still checked against the limits as if they were sand, loamy sand and sandy loam textural classes, respectively.

#texcl - vector of numeric values (NASIS codes) corresponding to texture groups
# returns a range for each of sand, silt and clay corresponding to the CLASS LIMITS imposed by the set of texture groups
getFineEarthLimitsFromTextureGroupID <- function(texcl) {
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
  return(getFineEarthLimitsFromTextureGroupID(texcl))
}

#function suitable for profileApply
# skipNA (default TRUE) will not record errors for NA sand, silt, clay fractions. e.g. if only clay is populated, only clay range is checked
checkProfileFineEarthLimits <- function(pedon, skipNA = TRUE, debug = FALSE) {
  h <- horizons(pedon) 
  h <- cbind(data.frame(pedon_id=pedon$pedon_id), h)
  rez <- apply(h, 1, FUN=checkHorizonFineEarthLimits, skipNA)
  if(debug)
    print(rez)
  return(all(unlist(rez)))
}

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
    if(any(is.na(rez) & !skipNA)) return(FALSE)
    if(all(rez[!is.na(rez)])) {
      return(TRUE)
    } else {
      errfrac <- which(!rez)
      print(paste0(horizon$pedon_id,":",horizon$hzname," horizon ",names(fracz)[errfrac], " content (",fracz[errfrac],"%) is out of range (",limitz[errfrac,1],"-",limitz[errfrac,2],"%) for specified texture class (",toupper(horizon$texture_class),"). PID:",horizon$peiid,"|HID:",horizon$phiid))
      return(FALSE)
    }
  } else {
    if(!skipNA) 
      return(FALSE)
    else
      return(TRUE)
  }
}

#NEW SOILDB type functions
#takes an SPC comprised of components and adds a field containing 
#     a list of the textural groups (non-RV included) allowed in this component horizon

fetchNASISComponentTexture <- function(coiids) {
  if(!requireNamespace('RODBC')) stop('please install the `RODBC` package', call.=FALSE)
  q <- paste0("SELECT co.dmuiidref, co.coiid, chiidref as chiidref, chtgiid, cht.chtiid, texture, cht.texcl, cht.lieutex, stratextsflag, rvindicator FROM (chtexturegrp chtg
  INNER JOIN chtexture cht ON cht.chtgiidref = chtg.chtgiid
  INNER JOIN chorizon ch ON chtg.chiidref = ch.chiid
  INNER JOIN component_View_1 co ON co.coiid = ch.coiidref) WHERE co.coiid IN (", paste0(coiids,collapse=", "), ") ;")
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  RODBC::odbcClose(channel)
  return(d)  
}

checkComponentFineEarthLimits <- function(component, skipNA=TRUE) {
  h <- horizons(component) 
  rez <- ddply(h, .variables = c("chiid"), .fun = checkCHorizonFineEarthLimits)
}

getComponentAWCRV <- function(component) {
  return(setNames(unname(ddply(horizons(component), .variables = c("chiid"), .fun = getComponentHorizonAWCRV)$V1), nm=horizons(component)$chiid))
}

getComponentAWCLow <- function(component) {
  return(setNames(unname(ddply(horizons(component), .variables = c("chiid"), .fun = getComponentHorizonAWCLow)$V1), nm=horizons(component)$chiid))
}

getComponentAWCHigh <- function(component) {
  return(setNames(unname(ddply(horizons(component), .variables = c("chiid"), .fun = getComponentHorizonAWCHigh)$V1), nm=horizons(component)$chiid))
}

makeComponentTextureSummary <- function(component) {
  fnew <- fetchNASISComponentTexture(component$coiid)
  df <- merge(horizons(component), fnew, by.x="chiid", by.y="chiidref") #do the join to make 1 row per texture group (many per horizon)
  rez <- ddply(df, .variables="chiid", .drop=F, .fun = function(hz) { #collapse back to 1 row per horizon via ddply
    texgrps <- as.character(uncode(data.frame(texcl=hz$texcl))$texcl) #convert texture codes to names
    rvgrp <- texgrps[which(hz$rvindicator == 1)[1]] #save name of rv texture
    texorder <- order(as.numeric(hz$texcl)) #order the texture classes by their ID; coarsest = smallest number; finer = bigger number
    texgrps <- unique(texgrps[texorder])        #remove redundant texture names (e.g GR-SL and SL show up as SL SL)
    idx <- which(texgrps == rvgrp)    #find index of RV texture in the unique set of texture names
    texgrps[idx] <- paste0(texgrps[idx],"*") #add an asterisk as RV indicator
    txtzz <- toupper(paste0(texgrps,collapse=" ")) #collapse all texture names into a single string, one per horizon
    #make a summary data frame row for this horizon
    data.frame(depth=paste0(as.character(c(hz$hzdept_r[1], hz$hzdepb_r[1])),collapse=" to "), textures=txtzz, clay=paste0(as.character(c(hz$claytotal_l[1], as.character(round(as.numeric(hz$claytotal_r[[1]]),1)), hz$claytotal_h[1])), collapse="  "), frags=paste0(as.character(c(hz$fragvoltot_l[1], hz$fragvoltot_r[1], hz$fragvoltot_h[1])), collapse="  ")) 
  })
  return(rez)
}

getAWC <- function(tdep, bdep, texcl, total_frags_pct) {
 awc_base <- list("c" = 0.16, "cl" = 0.20, "cos" = 0.04, "cosl" = 0.12, "fs" = 0.09, "fsl" = 0.14, "l" = 0.17, "lcos" = 0.06, "lfs" = 0.09, "ls" = 0.07, "lvfs" = 0.09, "s" = 0.06, "sc" = 0.16, "scl" = 0.15, "si" = 0.18, "sic" = 0.16, "sicl" = 0.20, "sil" = 0.20, "sl" = 0.12, "vfs" = 0.09, "vfsl" = 0.16, "hpm" = 0.40, "mpm" = 0.50, "spm" = 0.60)
 foo <- which(tolower(getBaseTextureClassName(texcl)) == names(awc_base))
 if(length(foo) > 0)  
   return(as.numeric(awc_base[foo[1]]) * (1 - (total_frags_pct / 100)))
 return(NA)
}

getHorizonAWC <- function(horizon) {#NB uses PEDON HORIZON field names
  return(getAWC(horizon$hzdept, horizon$hzdepb, horizon$texture_class, horizon$total_frags_pct_cal))
}

getComponentHorizonAWCRV <- function(horizon) {
  #get lowest water storage textural class and highest fragments and determine awc
  fnew <- fetchNASISComponentTexture(horizon$coiid) 
  df <- merge(horizon, fnew, by.x="chiid", by.y="chiidref") #1 row per texture group
  idx <- which(df$rvindicator == 1)[1]
  if(length(idx) == 1) { 
    df <- df[idx,]
    return(as.numeric(getAWC(df$hzdept_r, df$hzdepb_r, as.character(uncode(data.frame(texcl=df$texcl))[1,]), df$fragvoltot_r)))
  } else if (length(idx) > 1) {
    print(paste0("Only one RV texture allowed! Horizon: ",horizon$chiid))
  } else return(NA)
}

getComponentHorizonAWCLow <- function(horizon) {
  #get lowest water storage textural class and highest fragments and determine awc
  fnew <- fetchNASISComponentTexture(horizon$coiid)
  df <- merge(horizon, fnew, by.x="chiid", by.y="chiidref") #1 row per texture group
  rez <- ddply(df, .variables="chtiid", .drop=F, .fun = function(hz) { return(getAWC(hz$hzdept_r, hz$hzdepb_r, as.character(uncode(data.frame(texcl=hz$texcl))[1,]), hz$fragvoltot_h)) })
  return(min(rez$V1))
}

getComponentHorizonAWCHigh <- function(horizon) {
  fnew <- fetchNASISComponentTexture(horizon$coiid)
  df <- merge(horizon, fnew, by.x="chiid", by.y="chiidref") #1 row per texture group
  rez <- ddply(df, .variables="chtiid", .drop=F, .fun = function(hz) { 
    return(getAWC(hz$hzdept_r, hz$hzdepb_r, as.character(uncode(data.frame(texcl=hz$texcl))[1,]), hz$fragvoltot_l)) 
  })
  return(max(rez$V1))
}

getBaseTextureClassName <- function(strtexture) {
  foo <- strsplit(as.character(unlist(strtexture)[1]),"-")
  if(length(foo[[1]])>1) 
    return(foo[[1]][length(foo[[1]])])
  foo2 <- strsplit(as.character(unlist(strtexture)[1])," ")
  return(foo2[[1]][length(foo2[[1]])])
}

checkCHorizonFineEarthLimits <- function(horizon, skipNA=TRUE) {
  fnew <- fetchNASISComponentTexture(horizon$coiid) ##TODO: make query use coiid to prefilter
  df <- merge(horizon, fnew, by.x="chiid", by.y="chiidref") #1 row per texture group
  if(all(!is.na(df$texcl))) {
    df$claylo <- lapply(df$texcl, getClayLow)
    df$clayhi <- lapply(df$texcl, getClayHigh)
    df$sandlo <- lapply(df$texcl, getSandLow)
    df$sandhi <- lapply(df$texcl, getSandHigh)
    df$siltlo <- lapply(df$texcl, getSiltLow)
    df$silthi <- lapply(df$texcl, getSiltHigh)
    
    idx <- which(df$chiid %in% horizon$chiid)
    if(length(idx) > 0) {
      cclaylo <- min(as.numeric(df[idx,]$claylo)) 
      cclayhi <- max(as.numeric(df[idx,]$clayhi)) 
      csandlo <- min(as.numeric(df[idx,]$sandlo)) 
      csandhi <- max(as.numeric(df[idx,]$sandhi)) 
      csiltlo <- min(as.numeric(df[idx,]$siltlo)) 
      csilthi <- max(as.numeric(df[idx,]$silthi)) 
      baservtexture <- getBaseTextureClassName(horizon$texture)
      crvtext <- getFineEarthLimitsFromTextureClassName(baservtexture)
      if(TRUE) { #todo handle missing RVs; note does not check modifiers
        if(!is.between(horizon$sandtotal_r,crvtext[1,1],crvtext[1,2])) {
          print(paste0("Component horizon (",horizon$chiid,") RV sand content (",horizon$sandtotal_r,"%) is outside range (",crvtext[1,1],"-",crvtext[1,2],"%) of RV textural class (",baservtexture,")"))
        }
        if(!is.between(horizon$silttotal_r,crvtext[2,1],crvtext[2,2])) {
          print(paste0("Component horizon (",horizon$chiid,") RV silt content (",horizon$silttotal_r,"%) is outside range (",crvtext[2,1],"-",crvtext[2,2],"%) of RV textural class (",baservtexture,")"))
        }
        if(!is.between(horizon$claytotal_r,crvtext[3,1],crvtext[3,2])) {
          print(paste0("Component horizon (",horizon$chiid,") RV clay content (",horizon$claytotal_r,"%) is outside range (",crvtext[3,1],"-",crvtext[3,2],"%) of RV textural class (",baservtexture,")"))
        }
      }
      
      if(TRUE)#Check ranges #TODO hadnle missing ranges
        if(!is.between(horizon$sandtotal_l,csandlo,csandhi) | !is.between(horizon$sandtotal_h, csandlo,csandhi))
          print(paste0("Component horizon (",horizon$chiid,") sand range (",horizon$sandtotal_l,"-",horizon$sandtotal_h,"%) is outside range (", csandlo,"-",csandhi,"%) of horizon texture groups (",paste0(uncode(df[idx,]$texcl),collapse=","),")"))
        
      if(TRUE)
         if(!is.between(horizon$silttotal_l,csiltlo,csilthi) | !is.between(horizon$silttotal_h,csiltlo,csilthi))
          print(paste0("Component horizon (",horizon$chiid,") silt range (",horizon$silttotal_l,"-",horizon$silttotal_h,"%) is outside range (",csiltlo,"-",csilthi,"%) of horizon texture groups (",paste0(uncode(df[idx,]$texcl),collapse=","),")"))
       
      if(TRUE) 
        if(!is.between(horizon$claytotal_l,cclaylo,cclayhi) | !is.between(horizon$claytotal_h, cclaylo,cclayhi))
          print(paste0("Component horizon (",horizon$chiid,") clay range (",horizon$claytotal_l,"-",horizon$claytotal_h,"%) is outside range (", cclaylo,"-",cclayhi,"%) of horizon texture groups (",paste0(uncode(df[idx,]$texcl),collapse=","),")"))
    }
  }
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
  if((texcl >= 1 & texcl <= 12) | any(texcl == 16 | texcl == 19 | texcl == 21)) return(0)
  else if(texcl == 17) return(15)
  else if(texcl == 13) return(28)
  else if(any(texcl == 20 | texcl == 18)) return(40)
  else if(texcl == 14) return(50)
  else if(texcl == 15) return(80)
  return(NA)
}

getSandHigh <- function(texcl) {
  if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(100)
  else if(texcl >= 5 & texcl <= 8) return(90)
  else if(texcl >= 9 & texcl <= 12) return(85)
  else if(texcl == 16) return(80)
  else if(texcl == 19) return(65)
  else if(texcl == 13) return(52)
  else if(texcl == 14) return(50)
  else if(any(texcl == 17 | texcl == 21)) return(45)
  else if(any(texcl == 15 | texcl == 18 | texcl == 20)) return(20)
  return(NA)
}

getSandLow <- function(texcl) {
  if(any(texcl == 21 | texcl == 20 | texcl == 18 | texcl == 14 | texcl == 15)) return(0)
  else if(texcl == 17) return(20)
  else if(texcl == 13) return(23)
  else if(texcl >= 9 & texcl <= 12) return(43)
  else if(any(texcl == 16 | texcl == 19)) return(45)
  else if(texcl >= 5 & texcl <= 8) return(70)
  else if(texcl >= 1 & texcl <= 4) return(85)
  return(NA)
}