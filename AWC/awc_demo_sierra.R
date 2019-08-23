# Region2 AWC lookup table demo

library(aqp)
library(soilDB)

# read RO2 AWC lookup table
awc_lut <- read.csv("S:/NRCS/Archive_Andrew_Brown/Scripts/AWC/RO2_awc_lut.csv")
rf_lut <- read.csv("S:/NRCS/Archive_Andrew_Brown/Scripts/AWC/RO2_rf_lut.csv")

# read pedon data from NASIS - includes TUD pedons and all supporting observations
f <- fetchNASIS()
hz <- horizons(f)

textures <- toupper(hz$texcl)

#optional: include in-lieu textures
inlieu.idx <- which(is.na(hz$texcl) & !is.na(hz$lieutex))
textures[inlieu.idx] <- toupper(hz$lieutex[inlieu.idx])

#optional: exclude bedrock in-lie textures
textures[textures == "BR"] <- NA

om <- rep(NA, length(textures))
om[grepl(hz$hzname, pattern="^O")] <- 1
om[grepl(hz$hzname, pattern="^A")] <- 2
om[grepl(hz$hzname, pattern="^B")] <- 2
om[grepl(hz$hzname, pattern="^[2-9]")] <- 3 # lithologic discontinuities
om[grepl(hz$hzname, pattern="^C")] <- 3

frags <- hz$total_frags_pct

getAWC <- function(texcl, omcl, gravel=NULL, ec=NULL, FUN=mean, precision = 2, ...) {
  awc_lut <- read.csv("S:/NRCS/Archive_Andrew_Brown/Scripts/AWC/RO2_awc_lut.csv")
  
  if(length(texcl) != length(omcl))
    stop("Error: Mismatch in length of input vectors.")
  
  split.lut <- split(awc_lut, f=awc_lut$texcl)
  out <- rep(NA, length(texcl))

  for(i in 1:length(texcl)) {
    res <- lapply(split.lut, function(x) {
      if(grepl(pattern=paste0("^",texcl[i],"$"), x$texcl, ignore.case = TRUE)) {
        idx.hi <- 1 + omcl[i]*2
        idx.lo <- idx.hi - 1
        if(!is.na(idx.hi) & !is.na(idx.lo)) {
          return(x[,idx.lo:idx.hi])
        } else return(NA)
      }
    })
    out[i] <- FUN(unlist(res))
  }
  return(round(out, precision))
}

# apply rock fragment or salt correction  
# by default, percent passing #10 sieve is used (total_rf)
#   if gravel fraction is specified separately, slighltly more limiting AWCs are applied for fragments dominated by gravel
#   the most limiting rock fragment correction is used

# if ec (electrical conductivity) is specified and present (ec >= 2 mmhos/cm), then correction is applied again for salts, 
# relative to the _fragment-corrected_ AWC.

correct_AWC <- function(awc_, total_rf_, gravel_ = NULL, ec_ = NULL) {
  rf_lut <- read.csv("S:/NRCS/Archive_Andrew_Brown/Scripts/AWC/RO2_rf_lut.csv")
  
  if(length(awc_) != length(total_rf_))
    stop("Error: Mismatch in length of input vectors.")
  
  use_salt_correction = FALSE
  
  lut.row.idx <- get_rf_lut_row(rf_lut, total = total_rf_, gravel = gravel_)
  lut.col.idx <- get_rf_lut_column(rf_lut, awc_)
  
  awc_c <- numeric(length(lut.row.idx))
  awc_c[is.na(lut.col.idx)] <- NA
  
  if(length(lut.row.idx) != length(lut.col.idx)) {
    stop("critical mismatch")
  }
  
  for(i in 1:length(lut.row.idx)) {
    if(!is.na(lut.col.idx[i])) {
      awc_c[i] <- rf_lut[lut.row.idx[i], lut.col.idx[i]]
    } else { 
      awc_c[i] <- NA
    }
  }
  
  if(!is.null(ec_) & length(ec_) == length(awc_) & any(ec_ >= 2)) 
    use_salt_correction = TRUE
  
  if(use_salt_correction) {
    # use EC to identify degree of reduction
    lut.row.idx2 <- get_rf_lut_row(rf_lut, ec = ec_)
    
    # use RF-reduced AWC for salt correction
    lut.col.idx2 <- get_rf_lut_column(rf_lut, awc_c)
    
    awc_c2 <- numeric(length(lut.row.idx2))
    awc_c2[is.na(lut.col.idx2)] <- NA
    # use lookup table and return result
    for(i in 1:length(lut.row.idx2)) {
      if(!is.na(lut.col.idx2[i])) {
        awc_c2[i] <- rf_lut[lut.row.idx2[i], lut.col.idx2[!is.na(lut.col.idx2)]]
      } else { 
        awc_c2[i] <- NA
      }
    }
    return(unlist(awc_c2))
  }
  return(unlist(awc_c))
}

# returns most limiting RF-reduction based on total RF, gravel or electrical conductivity
get_rf_lut_row <- function(rf_lut, total=NULL, gravel=NULL, ec=NULL) {
  n <- max(length(total), length(gravel), length(ec))
  buf <- numeric(n)
  for(i in 1:n) {
    idx1 <- idx2 <- idx3 <- -2
    if(!is.null(total[i])) 
      idx1 <- which(rf_lut$rf_byvol_l <= total[i] & rf_lut$rf_byvol_h > total[i])
    if(!is.null(gravel[i])) 
      idx2 <- which(rf_lut$gravel_l <= gravel[i] & rf_lut$gravel_h > gravel[i])
    if(!is.null(ec[i])) 
      idx3 <- which(rf_lut$salts_mmhos_l <= ec[i] & rf_lut$salts_mmhos_h > ec[i])
    buf[i] <- max(idx1, idx2, idx3) + 1
  }
  return(buf)
}

get_rf_lut_column <- function(rf_lut, awc_) {
  # TODO: relies on 1:1 match of AWC to column in LUT - in future, use ranges
  return(match(awc_, rf_lut[1,7:ncol(rf_lut)], nomatch = NA) + 7)
}

depth.weighted.mean <- function(spc, tdepth, bdepth, attr, na.rm = FALSE) {
  #expand `attr` in formula
  custom.formula <- formula(paste0(tdepth,":",bdepth," ~ ", paste0(attr, collapse=" + ")))
  # calculate a depth-weighted mean using aqp::slice()'d attr
  return(mean(slice(spc, custom.formula, just.the.data=TRUE)[[attr]], na.rm = na.rm))
}

depth.weighted.sum <- function(spc, tdepth, bdepth, attr, na.rm = FALSE) {
  #expand `attr` in formula
  custom.formula <- formula(paste0(tdepth,":",bdepth," ~ ", paste0(attr, collapse=" + ")))
  # calculate a depth-weighted sum using aqp::slice()'d attr
  return(sum(slice(spc, custom.formula, just.the.data=TRUE)[[attr]], na.rm = na.rm))
}

f$awc_r <- getAWC(textures, om)
f$awc_rc <- correct_AWC(f$awc_r, total_rf_=frags)

f$awc_l <- correct_AWC(getAWC(textures, om, FUN=min), total_rf_=frags)
f$awc_h <- correct_AWC(getAWC(textures, om, FUN=max), total_rf_=frags)

test <- data.frame(textures, om, f$awc_r, f$awc_rc, frags)
if(nrow(test[!complete.cases(test) & !is.na(test[,1]) & !is.na(test[,2]),])) {
  stop("some horizons have texture and om populated but no AWC calculated")
}


plot(f, color="awc_r")

f$awc_0to50 <- profileApply(f, depth.weighted.sum, tdepth=0, bdepth=50, attr="awc_r")
f$awc_0to50_l <- profileApply(f, depth.weighted.sum, tdepth=0, bdepth=50, attr="awc_l")
f$awc_0to50_h <- profileApply(f, depth.weighted.sum, tdepth=0, bdepth=50, attr="awc_h")

f$awc_0to50_l[!is.finite(f$awc_0to50_l)] <- NA
f$awc_0to50_h[!is.finite(f$awc_0to50_h)] <- NA

quantile(f$awc_0to50, na.rm = TRUE)

plot(ecdf(f$awc_0to50), main = "MLRA 18 - Sierra series EVAL - Pedon Avail. Water Capacity\nEmpirical CDF",
     xlab="cm of Available Water (0 to 50 cm)", xlim=c(5,11))
plot(ecdf(f$awc_0to50_l), col="RED", add=T)
plot(ecdf(f$awc_0to50_h), col="BLUE", add=T)
abline(v=7, lty=2, lwd=2, col="GREEN")
legend("topleft",legend=c("RV", "LOW","HIGH","Low AWC"),
                  col=c("BLACK","RED","BLUE","GREEN"), 
                  pch=c(19,19,19, NA), lwd=c(1,1,1,2),
                  lty=c(1,1,1,2))

plot(density(f$awc_0to50, na.rm = TRUE), xlim=c(5,11), ylim=c(0,1), 
     main = "MLRA 18 - Sierra series EVAL - Pedon Avail. Water Capacity\nDensity Plot")
lines(density(f$awc_0to50_l, na.rm = TRUE), col="RED")
lines(density(f$awc_0to50_h, na.rm = TRUE), col="BLUE")
lines(density(c(f$awc_0to50,f$awc_0to50_l,f$awc_0to50_h), na.rm = TRUE),lwd=2,lty=2,col="PURPLE")
abline(v=7)

full.set <- c(f$awc_0to50, f$awc_0to50_l, f$awc_0to50_h)
knitr::kable(quantile(full.set, probs=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1), na.rm=TRUE))

k <- fetchKSSL(series="Sierra")
k$om_0to50 <- profileApply(k, depth.weighted.mean, tdepth=0, bdepth=50, attr="oc") / 0.58
plot(density(k$om_0to50, na.rm = TRUE), "Sierra KSSL Pedons - Organic Matter %\n(0 to 50cm Weighted-average)")
plot(density(k$oc[grepl(k$hzn_desgn, pattern="^A")] / 0.58, na.rm = TRUE), "Sierra KSSL Pedons - Organic Matter %\n(A horizons only)")

k$a_hz_thickness <- profileApply(k, getSurfaceHorizonDepth, pattern="^A|^O", hzdesgn = "hzn_desgn")
f$a_hz_thickness <- profileApply(f, getSurfaceHorizonDepth, pattern="^A|^O")
plot(density(f$a_hz_thickness, na.rm = TRUE, from = 0), col="BLUE", "Sierra Pedons - O+A horizon thickness")
lines(density(k$a_hz_thickness, na.rm = TRUE, from = 0), col="RED")
abline(v=max(f$a_hz_thickness), lty=2, lwd=3, col="BLUE")
abline(v=max(k$a_hz_thickness), lty=2, lwd=2, col="RED")
legend('topright',legend=c("NASIS","KSSL","Max NASIS","Max KSSL"), lwd=c(1,1,3,2), lty=c(1,1,2,2), col=c("BLUE","RED","BLUE","RED"))
quantile(f$a_hz_thickness, probs=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1), na.rm=TRUE)

f$issurfacehz <-  grepl(f$hzname, pattern="^A|^O")
f$issurfacehz[f$issurfacehz] <- "#000000"
f$issurfacehz[f$issurfacehz == "FALSE"] <- "#FFFFFF"
f$age <- grepl(f$pedon_id, pattern="CA630|C0|AMS|BJM|PDM|RJV|SMM|BAH|MJE|CA109")
groupedProfilePlot(f, color='issurfacehz', groups='age', label="pedon_id")

f$awc_surfacehz_r <- profileApply(f, function(p) {
  return(depth.weighted.mean(p, 0, 5, attr = 'awc_r'))
})
plot(density(f$awc_surfacehz_r, na.rm=TRUE))
quantile(f$awc_surfacehz_r, na.rm=T)

f$solum_thickness <- profileApply(f, estimateSoilDepth, p = "^C|R")
plot(density(f$solum_thickness, na.rm=TRUE, from=min(f$solum_thickness, na.rm=TRUE)), xlim=c(0,250), "Sierra NASIS Pedons\nSolum (O+A+B horizon) Thickness")

f$surface_to_solum <- f$a_hz_thickness / f$solum_thickness
plot(density(f$surface_to_solum, na.rm=T, from=0), "Sierra NASIS Pedons\nSurface (O+A) to Solum (O+A+B) Thickness Ratio")

f.sub <- f[f$obs_date == "2019-07-01 PDT" | f$obs_date == '1959-06-01 PDT']
knitr::kable(data.frame(USERPEDONID=f.sub$pedon_id, 
           AWC_Oto50_L=f.sub$awc_0to50_l, 
           AWC_Oto50_R=f.sub$awc_0to50, 
           AWC_Oto50_H=f.sub$awc_0to50_h, 
           SURFACE_HZ_THK=f.sub$a_hz_thickness, 
           SOLUM_THK=f.sub$solum_thickness, 
           SURFACE_SOLUM_RATIO=f.sub$surface_to_solum))

comp <- fetchNASIS_components()

chz <- horizons(comp)
ctextures <- toupper(chz$texture)
cinlieu.idx <- which(is.na(chz$texture))
ctextures[cinlieu.idx] <- toupper(chz$lieutex[cinlieu.idx])

com <- rep(NA, length(ctextures))
com[grepl(chz$hzname, pattern="^O")] <- 1
com[grepl(chz$hzname, pattern="^A")] <- 2
com[grepl(chz$hzname, pattern="^B")] <- 2
com[grepl(chz$hzname, pattern="^C")] <- 3

cfrags <- chz$fragvoltot_r
comp$awc_r_C <- getAWC(ctextures, com, cfrags, awc_lut)
comp$awc_l_C <- getAWC(ctextures, com, cfrags, awc_lut, FUN=min)
comp$awc_h_C <- getAWC(ctextures, com, cfrags, awc_lut, FUN=max)

comp$awc_0to50 <- profileApply(comp, depth.weighted.sum, tdepth=0, bdepth=50, attr="awc_r")
comp$awc_0to50_C <- profileApply(comp, depth.weighted.sum, tdepth=0, bdepth=50, attr="awc_r_C")

plot(comp$awc_0to50 ~ comp$awc_0to50_C, xlim = c(5,10), ylim = c(5,10),
     main="Comparison of 0-50cm Available Water - Components versus TUDs")
abline(0,1)
abline(v=7, h=7, col="RED")
plot(comp, color="awc_r_C", label="dmudesc")

comp$mlramu <- c("18XI","18XI, deep","15XF","18XC")
comp$pedon_id <- rep("original",length(comp))
comp$awc_compare <- comp$awc_r
comp2 <- comp
comp2$pedon_id <- rep("RO2 AWC guide", length(comp2))
profile_id(comp2) <- paste0(profile_id(comp),"_calc")
comp2$awc_compare <- comp$awc_r_C
tuds <- f[f$pedon_id %in% c("1966CA019003","1967CA089005","1968CA017002","1975CA057003","1975CA061003","S1959CA005015")]
tuds$mlramu <- c("18XI","15XF", "18XI, deep","18XI, deep","18XI","18XC")
tuds$awc_compare <- tuds$awc_r
compspc <- aqp::union(list(comp,comp2,tuds))
compspc$awc_0to50_all <- paste0(round(profileApply(compspc, depth.weighted.sum, tdepth=0, bdepth=50, attr="awc_compare"), 2)," / 50")
groupedProfilePlot(compspc, color="awc_compare", groups="mlramu", label="pedon_id", alt.label="awc_0to50_all", alt.label.col="WHITE")
abline(h=50,lwd=2,lty=2,col="BLUE")

