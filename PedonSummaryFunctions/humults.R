library(soilDB)
library(aqp)
library(rgdal)
library(raster)

source('pedon_summary_functions.R')

f <- fetchKSSL(series = 'Sites')#mlra = '22A')
coordinates(f) <- ~ x + y
proj4string(f) <- "+proj=longlat +datum=WGS84"

quantile(horizons(f)[grepl(f$hzn_desgn, pattern='A'),]$db_13b, na.rm=T)
quantile(horizons(f)[grepl(f$hzn_desgn, pattern='B'),]$db_13b, na.rm=T)
quantile(horizons(f)[grepl(f$hzn_desgn, pattern='C'),]$db_13b, na.rm=T)

hasHumultCriteria <- function(x) {
  ## 1) check argillic bounds
  x$hzname <- x$hzn_desgn
  x$hzdept <- x$hzn_top
  x$hzdepb <- x$hzn_bot
  b <- getArgillicBounds(x)
  
  ## 2) intersect horizon(s) in argillic upper 15cm, AND ultisol BS depth
  uarg.idx <- intersectLabHorizon(x, b$ubound + 15) #upper argillic
  larg.idx <- intersectLabHorizon(x, min(b$ubound + 125, 180, estimateSoilDepth(x)-1))
  
  ## Criterion #0: its an ultisol
  criterion_0 <- (horizons(x)[x$labsampnum %in% larg.idx,]$bs82 < 35)
  if(!length(criterion_0))
    criterion_0 <- NA  
  
  ## Criterion #1: organic carbon in upper argillic
  ## 3) if >0.9% OC then Humult; 
  criterion_1 <- (horizons(x)[x$labsampnum %in% uarg.idx,]$estimated_oc > 0.9)
  if(!length(criterion_1))
    criterion_1 <- NA
  
  #Criterion #2: organic carbon (kg/m^2) in upper meter of mineral soil
  carbon <- x$estimated_oc / 100 # kg OC/kg soil
  db <- x$db_13b * 1000 # kg soil/m^3 soil
  thicknz <- (x$hzn_bot - x$hzn_top) / 100 # meters (thickness)
  thickcm <- cumsum(thicknz)
  too.deep.idx <- which(thickcm >= 1)[1]
  
  too.shallow <- 0
  if(is.na(too.deep.idx)) {
    too.shallow <- 1-max(thickcm)
    too.deep.idx <- length(thickcm)+1
  }  
  
  if(too.shallow == 0) {
    thicknz[too.deep.idx] <- thicknz[too.deep.idx]-(thickcm[too.deep.idx] - 1)
    if(length(thickcm) > too.deep.idx)
      thicknz[(too.deep.idx+1):length(thickcm)] <- 0
  }
  
  df <- (data.frame(hz = x$hzn_desgn, oc = carbon, dz=thicknz, db=db))
  if(any(is.na(df$db))) { #if bulk density data are not measured, use horizon designation
    df$db[grepl(df$hz, pattern="A") & is.na(df$db)] <- 1115
    df$db[grepl(df$hz, pattern="B") & is.na(df$db)] <- 1320
    df$db[grepl(df$hz, pattern="C") & is.na(df$db)] <- 1445
  }
  #print(df)
  df <- na.omit(df)
  if(nrow(df)) {
    criterion_2 <- (sum(df$oc*df$db*df$dz) > 12)
  } else {
    criterion_2 <- NA
  }
  status <- "unevaluated"
  if(is.finite(b$ubound) & is.finite(b$lbound))
    status <- "Alfisol"
  if(!is.na(criterion_0)) {
    if(criterion_0) 
      status <- "Ultisol"
  } else return("No argillic or no data")
  if(criterion_0) {
    if(!is.na(criterion_1)) {
      if(criterion_1) 
        status <- "Humult Type 1"
    } else return("Humult criterion 1 not evaluated")
    if(!is.na(criterion_2)) {
      if(criterion_2)
        status <- "Humult Type 2"
      if(criterion_0 & criterion_1 & criterion_2)
        status <- "Humult Type 1 & 2"
    } else return("Humult criterion 2 not evaluated")
  }
  return(status)
}
f$rez <- profileApply(f, FUN = hasHumultCriteria)
summary(factor(as.character(f$rez)))

not.evaluated.idx <- which(f$rez == 'No argillic or no data')
f.sub <- f[-not.evaluated.idx,]
who.idx <- which(f.sub$rez == "Ultisol")
humult.idx <- which(grepl(f.sub$rez, pattern="Humult"))

ca630_b <- readOGR(dsn="L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/final/FG_CA630_GIS_2018.gdb", layer="ca630_b")
ca630_b_t <- spTransform(ca630_b, CRS(proj4string(f.sub)))

plot(f.sub[-c(who.idx,humult.idx),]@sp, pch=19, main="Ultisols in MLRA 22A - USDA-NCSS KSSL Lab data")
points(f[not.evaluated.idx,]@sp, pch = 46)
plot(ca630_b_t, add=T)
points(f.sub[who.idx,]@sp, pch=19, col="RED")
points(f.sub[humult.idx,]@sp, pch=19, col="BLUE")
legend(x="bottomleft",legend=c(paste0("Humults (",nrow(site(f.sub)[humult.idx,]),")"), paste0("Ultisols (",nrow(site(f.sub)[who.idx,]),")"), paste0("Alfisols (",nrow(site(f.sub)[-c(who.idx,humult.idx),]),")"), paste0("N/A or N.D. (",nrow(site(f)[not.evaluated.idx,]),")")), pch=c(19,19,19,46), col=c("BLUE","RED","BLACK","BLACK"))

writeOGR(as(f.sub,'SpatialPointsDataFrame'), dsn = ".", layer="mlra22a_ultisols", driver="ESRI Shapefile", overwrite=T)

precip <- raster("L:/NRCS/MLRAShared/Geodata/project_data/MUSum_PRISM/final_MAP_mm_800m.tif")
f.sub$precip_mm <- extract(precip, as(f.sub,'SpatialPointsDataFrame'))

plot((f.sub$precip_mm ~ factor(f.sub$rez)), xlab="Taxonomic Criterion", ylab="Mean Annual Precipitation, mm")

humultz <- f.sub[humult.idx,]
quantile(horizons(humultz)[grepl(humultz$hzn_desgn, pattern='Bt1'),]$estimated_om, na.rm=T, p=c(0.05,0.95))
