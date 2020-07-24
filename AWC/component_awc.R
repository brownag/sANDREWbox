library(aqp)
library(soilDB)
library(magrittr)

getAWC <- function(lut, texcl, omcl, gravel=NULL, ec=NULL, FUN=mean, precision = 2, ...) {
  #awc_lut, loaded from regional lookup table
  awc_lut <- read.csv("E:\\workspace\\sANDREWbox\\AWC\\RO2_awc_lut.csv")
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
    out[i] <- FUN(unlist(res), ...)
  }
  return(round(out, precision))
}

# apply rock fragment or salt correction  
# by default, percent passing #10 sieve is used (total_rf)
#   if gravel fraction is specified separately, slighltly more limiting AWCs are applied for fragments dominated by gravel
#   the most limiting rock fragment correction is used

# if ec (electrical conductivity) is specified and present (ec >= 2 mmhos/cm), then correction is applied again for salts, 
# relative to the _fragment-corrected_ AWC.

correct_AWC <- function(lut, awc_, total_rf_, gravel_ = NULL, ec_ = NULL) {
  # rf_lut loaded from regional lookup table
  rf_lut <- read.csv("E:\\workspace\\sANDREWbox\\AWC\\RO2_rf_lut.csv")
  
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
  # TODO: relies on 1:1 match of AWC to column in LUT - in future, use ranges and interpolate
  return(match(awc_, rf_lut[1,7:ncol(rf_lut)], nomatch = NA) + 7)
}

f <- fetchNASIS(from="components")

f.sub <- f %>% filter(compname="Vistarobles")#dmuiid == "859770", compname=="Gardellones")

# this assigns a organic matter range/class limits based on first character in hzdesgn
om <- rep(NA, nrow(f.sub))
# om[grepl(f.sub$hzname, pattern="^O")] <- 1
# om[grepl(f.sub$hzname, pattern="^A")] <- 2
# om[grepl(f.sub$hzname, pattern="^B")] <- 2
# om[grepl(f.sub$hzname, pattern="^[2-9]")] <- 3 # lithologic discontinuities
# om[grepl(f.sub$hzname, pattern="^C")] <- 3
om[f.sub$om_r >= 3] <- 1
om[f.sub$om_r >= 1 & f.sub$om_r < 3] <- 2
om[f.sub$om_r < 1] <- 3
om

pattern <- "(.*)\\-(.*)"
as.character(lapply(f.sub$texture, function(s) lapply(regmatches(s, gregexpr(pattern, s)),
       function(e) regmatches(e, regexec(pattern, e)))))

newtextures <- as.character(lapply(lapply(regmatches(f.sub$texture, gregexpr(pattern, f.sub$texture)),
       function(e) unlist(regmatches(e, regexec(pattern, e)))), function(d) if(!is.null(d)) d[3]))
textures <- f.sub$texture
textures[which(newtextures != "NULL")] <- newtextures[which(newtextures != "NULL")]

f.sub$awc_r <- correct_AWC(rf_lut, 
                           getAWC(awc_lut,texcl = textures, omcl = om), #rv awc, fine earth fraction
                           total_rf_=f.sub$fragvoltot_r) # corrected for rv frags

f.sub$awc_l <- correct_AWC(rf_lut, 
                           getAWC(awc_lut, texcl = textures, omcl = om, FUN=min, na.rm=T), # low awc from FEF
                           total_rf_=f.sub$fragvoltot_h) # corrected for high frags

f.sub$awc_h <- correct_AWC(rf_lut, 
                           getAWC(awc_lut, texcl = textures, omcl = om, FUN=max, na.rm=T), # high awc from FEF
                           total_rf_=f.sub$fragvoltot_l) # corrected for low frags

plotSPC(f.sub, max.depth=100, color="awc_r")
f.sub$hcompname <- denormalize(f.sub, 'compname')
horizons(f.sub)[,c('hcompname', hzdesgnname(f.sub), horizonDepths(f.sub), 'awc_l','awc_r','awc_h')]
