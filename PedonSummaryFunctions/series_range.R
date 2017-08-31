# component / kssl lab data activity class checker

library(soilDB)
setwd("PedonSummaryFunctions")
source("pedon_summary_functions.R")

component.name <- 'Auberry'

n <- fetchNASIS(rmHzErrors = F, nullFragsAreZero = T)
f <- fetchKSSL(component.name)
f2 <- fetchKSSL("Sierra")
f <- rbind(f,f2)

site(f) <- merge(site(f), site(n), by="pedon_id", all.x = T)
horizons(f) <- merge(horizons(f), horizons(n)[,c("labsampnum","peiid","phiid")], by="labsampnum", all.x=T)

#alias fetchKSSL type SPC to fetchNASIS type SPC
horizons(f)$hzname <- horizons(f)$hzn_desgn
horizons(f)$hzdept <- horizons(f)$hzn_top
horizons(f)$hzdepb <- horizons(f)$hzn_bot

#calculate horizon CEC/clay ratiosn

#cec7 to clay is horizons(f)$cec7_cly

## these aren't diagnostic:
# horizons(f)$cec82_cly <- horizons(f)$cec82/horizons(f)$clay
# horizons(f)$scats_cly <- horizons(f)$base_sum/horizons(f)$clay

pscs_list <- profileApply(f, FUN=estimatePSCS, simplify = F)

intersectLabHorizon <- function(pedon, z1, z2=NULL) {
  hz <- horizons(pedon)
  if(!missing(z2)) { # if a top and bottom depth are specified, we may intersect multiple horizons
    foo <- numeric(0)
    for(h in 1:nrow(hz)) {
      hh <- hz[h,]
      if(is.between(hh$hzdept, z1, z2) | is.between(hh$hzdepb, z1, z2)) 
        foo <- c(foo, hh$labsampnum) # if one or both horizon boundaries falls between z1, z2 add pedon horizon to list
    }
    return(foo)
  } else { # if just z1 is specified, we will return 1 horizon ID using default "within" logic (less than or equal to) for tie breaking
    for(h in 1:nrow(hz)) {
      hh <- hz[h,]
      if(is.between(z1, hh$hzdept, hh$hzdepb)) 
        return(hh$labsampnum)
    }
  }
}

depth.weighted.lab.average <- function(p, attr=NA, ubound, lbound) {
  if(!is.na(attr) | !(attr %in% names(horizons(p)))) {
    hz <- horizons(p)
    
    #get horizons overlapping depth range of interest
    hz_overlay <- hz[(hz$labsampnum %in% intersectLabHorizon(p, ubound, lbound)), ]
    hz_overlay <- hz_overlay[order(hz_overlay$hzdept, decreasing=F), ]
    hz_overlay$weights = (hz_overlay$hzdepb - hz_overlay$hzdept) #calculate weightings (assuming no partial horizons)
    if(nrow(hz_overlay) > 0) {
      #first and last horizons may only partially contribute to the average
      for(h in 1:nrow(hz_overlay)) {
        cur <- hz_overlay[h, ]
        if(cur$hzdept < ubound) hz_overlay$weights[h] <- (cur$hzdepb - ubound) # portion of the first horizon below the upper bound
        if(cur$hzdepb > lbound) hz_overlay$weights[h] <- (lbound - cur$hzdept) # portion of the last horizon above the lower bound
      }
      print(data.frame(property=hz_overlay[[attr]], weight=hz_overlay$weights))
      return(weighted.mean(hz_overlay[[attr]], w=hz_overlay$weights))
    } else {
      return(NA)
    }
  } else {
    stop(paste0("No attribute specified to calculate depth-weighted average or attribute ",attr,"does not exist!"))
  }
}

foo1 <- lapply(1:length(pscs_list), FUN=function(i) {
  labiid <- names(pscs_list)[[i]]
  top <- pscs_list[[i]][1]
  bot <- pscs_list[[i]][2]
  foo <- paste0("pedon_key == ", labiid)
  pedon <- subsetProfiles(f, s=foo)
  print(paste0(" ---------- Lab ID:", labiid, " ---------- "))
  print("CEC7/Clay ratio")
  ratz7 <- depth.weighted.lab.average(pedon, top, bot, attr="cec7_cly")
  print(paste0("Weighted average over PSCS (", top,"-",bot,"cm): ", ratz7))
  ratz7
})

outframe <- data.frame(pedon=site(f)$pedon_id[(names(pscs_list) %in% site(f)$pedon_key)], taxon=toupper(site(f)$taxonname), sampled_as=toupper(site(f)$sampled_as), cec7_pscs = as.numeric(foo1))

foo2 <- lapply(1:length(pscs_list), FUN=function(i) {
  labiid <- names(pscs_list)[[i]]
  top <- pscs_list[[i]][1]
  bot <- pscs_list[[i]][2]
  foo <- paste0("pedon_key == ", labiid)
  pedon <- subsetProfiles(f, s=foo)
  print(paste0(" ---------- Lab ID:", labiid, " ---------- "))
  print("CS+VCS")
  cs <- depth.weighted.lab.average(pedon, top, bot, attr="cs")
  vcs <- depth.weighted.lab.average(pedon, top, bot, attr="vcs")
  csvcs <- cs+vcs
  print(paste0("Weighted average Coarse and Very Coarse Sand (", top,"-",bot,"cm): ", csvcs))
  # print("CS+VCS/Clay")
  # csvcscly <- cs+vcs/depth.weighted.lab.average(pedon,top,bot,"clay")
  # print(paste0("Weighted average Coarse and Very Coarse Sand ratio to Clay (", top,"-",bot,"cm): ", csvcscly))
  csvcs
})
outframe <- cbind(outframe, data.frame(cvcs_pscs=as.numeric(foo2)))

foo3 <- lapply(1:length(pscs_list), FUN=function(i) {
  labiid <- names(pscs_list)[[i]]
  top <- pscs_list[[i]][1]
  bot <- pscs_list[[i]][2]
  foo <- paste0("pedon_key == ", labiid)
  pedon <- subsetProfiles(f, s=foo)
  print(paste0(" ---------- Lab ID:", labiid, " ---------- "))
  bs82 <- depth.weighted.lab.average(pedon, top, bot, attr="bs82")
  print("Base Saturation pH 8.2")
  print(paste0("Weighted average Base Saturation Sum Cations (pH 8.2) (", top,"-",bot,"cm): ", bs82))
  bs82
})
outframe <- cbind(outframe, data.frame(bs82_pscs=as.numeric(foo3)))

foo4 <- lapply(1:length(pscs_list), FUN=function(i) {
  labiid <- names(pscs_list)[[i]]
  top <- pscs_list[[i]][1]
  bot <- pscs_list[[i]][2]
  foo <- paste0("pedon_key == ", labiid)
  pedon <- subsetProfiles(f, s=foo)
  print(paste0(" ---------- Lab ID:", labiid, " ---------- "))
  print("Base Saturation pH 7")
  bs7 <- depth.weighted.lab.average(pedon, top, bot, attr="bs7")
  print(paste0("Weighted average Base Saturation (pH 7) (", top,"-",bot,"cm): ", bs7))
  bs7
})
outframe <- cbind(outframe, data.frame(bs7_pscs=as.numeric(foo4)))

outframe

getCationExchangeActivityClass <- function(cec7cly_ratio) {
  if(!is.na(cec7cly_ratio)){
    if(cec7cly_ratio > 0.6) {
      return("SUPERACTIVE")
    } else if(cec7cly_ratio > 0.4 & cec7cly_ratio <= 0.6) {
      return("ACTIVE")
    } else if(cec7cly_ratio > 0.24 & cec7cly_ratio <= 0.4) {
      return("SEMIACTIVE")
    } else if(cec7cly_ratio <= 0.24) {
      return("SUBACTIVE")
    }
  }  
  return(NA)
}

pedonHasMollisolBaseSat<- function(pedon) {
  hz <- horizons(pedon)
  argbound <- getArgillicBounds(pedon)
  soildepth <- estimateSoilDepth(pedon)
  
  bottomdepths <- c(argbound$ubound+125, 180, soildepth)
  bottomdepth <- bottomdepths[which(bottomdepths == min(bottomdepths))]
  
  topdepth <- getMineralSoilSurfaceDepth(pedon)
  if(argbound$ubound > topdepth) {
    #ubound is -Inf if no argillic
    topdepth <- argbound$ubound
  }
  hz_overlay <- hz[(hz$labsampnum %in% intersectLabHorizon(pedon, topdepth, bottomdepth)),]
  return(min(hz_overlay$bs7) >= 50) #if the lowest base saturatiuon within the range of interest is greater than 50%, then we meet BS criteria for mollisol
}

pedonHasUlticSubgroupBaseSat <- function(pedon) {
  hz <- horizons(pedon)
  argbound <- getArgillicBounds(pedon)
  soildepth <- estimateSoilDepth(pedon)
  
  bottomdepths <- c(argbound$ubound+75, soildepth)
  bottomdepth <- bottomdepths[which(bottomdepths == min(bottomdepths))]
  
  topdepth <- getMineralSoilSurfaceDepth(pedon)
  if(argbound$ubound > -Inf) {
    #ubound is -Inf if no argillic
    topdepth <- argbound$ubound
    hz_overlay <- hz[(hz$labsampnum %in% intersectLabHorizon(pedon, topdepth, bottomdepth)),]
    isultic <- any(hz_overlay$bs82 < 75)
    isulticsurrogate <- any(hz_overlay$bs7 < 92.30891) 
    #source: 
    # bsmodel <- lm(formula = bs7 ~ bs82, data = horizons(f)) #aptly named variable
    # predict(bsmodel, data.frame(bs82=75)) #92.30891
    # 
    # plot(bs7 ~ bs82, data = horizons(f)) #graphical view, using the Sierra and Auberry data where both CEC7 and 8.2 were measured
    # abline(bsmodel)
    # abline(v=75)
    # abline(h=92.30891)
    #
    return(isultic | isulticsurrogate) #if any hz has base saturatiuon by sum of cations < 75% (<92.3% at CEC7) in upper 75cm of argillic, then we are ultic
  }
  return(FALSE) #return NA? this typically occurs when there is no argillic horizon detectable via clay increase
}

# lab pedon summary functions
outframe$activity_class <- lapply(outframe$cec7_pscs, FUN=getCationExchangeActivityClass)
outframe$mollisol_bs <- profileApply(f, FUN=pedonHasMollisolBaseSat)
outframe$ultic_subgroup <- profileApply(f, FUN=pedonHasUlticSubgroupBaseSat)
outframe$has_mollic <- profileApply(f, FUN=hasDarkMineralSurface)
outframe$has_dark_surface <- profileApply(f, FUN=hasDarkMineralSurface, require_chroma=FALSE, val_dry=8) #checking only moist value < 3
outframe$has_argillic <- profileApply(f, FUN=function(p) { return(getArgillicBounds(p)$ubound != -Inf) })

# taxonomic "decisions", using summary funtions
outframe$is_mollisol <- (outframe$mollisol_bs & outframe$has_mollic)
outframe$is_argixeroll <- (outframe$is_mollisol & outframe$has_argillic)
#currently not checking pachic argixerolls. 
outframe$is_ultic_argixeroll <- (outframe$is_argixeroll & outframe$ultic_subgroup)
outframe$is_typic_argixeroll <- (outframe$is_argixeroll & !outframe$ultic_subgroup)
outframe$is_ultic_haploxeralf <- (!outframe$is_mollisol & outframe$ultic_subgroup)
outframe$is_mollic_haploxeralf <- (!outframe$is_ultic_haploxeralf & outframe$has_dark_surface)
outframe$is_typic_haploxeralf <- (!outframe$is_mollic_haploxeralf & outframe$has_argillic)
outframe

outframe[which(!outframe$has_argillic),]$pedon

write.table(data.frame(outframe),file = "sierra_auberry_taxonomy.csv", sep = ",")

#how many of each taxon?
colSums(outframe[,c("is_mollisol","is_ultic_haploxeralf","is_mollic_haploxeralf","is_typic_haploxeralf")],na.rm = T)

#how many could not be decided?
colSums(is.na(outframe[,c("is_mollisol","is_ultic_haploxeralf","is_mollic_haploxeralf","is_typic_haploxeralf")]),na.rm = T)

#sampled as rescue?
# pscs_list[names(pscs_list) %in% site(subsetProfiles(f, s="pedon_id == '73-CA-05-066x'"))$pedon_key]
# setwd("..")