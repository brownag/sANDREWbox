#component series range checker 
# compares representative values from component to matching series concept range in characteristics 
# this identifies components that should *potentially* be taxadjuncts, series ranges to expand or notes to make in the final correlation document

source("PedonSummaryFunctions\\pedon_summary_functions.R")
source("PedonSummaryFunctions\\fine_earth_fractions.R")

reaction_classes <- list( #compares RV pH values with the low and high limits from the OSD for surface and PSCS/argillic
  'ultra acid' = data.frame(lo=1.8,hi=3.4),
  'extremely acid' = data.frame(lo=3.5,hi=4.4),
  'very strongly acid' = data.frame(lo=4.5,hi=5.0),
  'strongly acid' = data.frame(lo=5.1,hi=5.5),
  'moderately acid' = data.frame(lo=5.6,hi=6.0),
  'slightly acid' = data.frame(lo=6.1,hi=6.5),
  'neutral' = data.frame(lo=6.6,hi=7.3),
  'slightly alkaline' = data.frame(lo=7.4,hi=7.8),
  'moderately alkaline' = data.frame(lo=7.9,hi=8.4),
  'strongly alkaline' = data.frame(lo=8.5,hi=9),
  'very strongly alkaline' = data.frame(lo=9.1,hi=11), 
  'any' = data.frame(lo=1,hi=14) #if class is any, allow any pH (within reason)
)

psc.limits <- list( #pretty rudimentary checking of PSC clay against nominal ranges 
  'loamy' = data.frame(lo=0,hi=35),
  'loamy-skeletal' = data.frame(lo=0,hi=35),
  'coarse-loamy' = data.frame(lo=0,hi=18),
  'fine-loamy' = data.frame(lo=18,hi=35),
  'fine' = data.frame(lo=35,hi=100)
)

library(soilDB)

series.defs <- read.csv('PedonSummaryFunctions/607X_series_concepts.csv', stringsAsFactors = F)

c <- fetchNASIS_component_data()
c$ph1to1h2o_r <- round(c$ph1to1h2o_r, digits = 2)

for(s in 1:length(series.defs$series)) { #loop through each series concept
  sdef <- series.defs[s,]
  c.s <- c[grepl(sdef$series,c$compname,ignore.case = T),] #create a subset with components matching series name
  
  if(nrow(c.s)) { 
    #check that restriction is within range
    #TODO: cross reference to restrictions table to check that ranges match class
    soildepthz = profileApply(c.s, estimateSoilDepth, top="hzdept_r", bot="hzdepb_r", no.contact.depth=150)
    if(any(which(soildepthz > 150))) #don't check depth for components that are VD
     soildepthz <- soildepthz[-which(soildepthz > 150)]
    restr_check <- lapply(soildepthz, FUN=is.between, sdef$restr_top, sdef$restr_bottom)
    
    
    if(any(!unlist(restr_check))) { #if any have restrictions outside the range
      checkcomps <- !unlist(restr_check)
      print(paste0("Component ",paste0(c.s[which(checkcomps),]$dmudesc,c.s[which(checkcomps),]$compname)," restriction at ",soildepthz[checkcomps],"cm is outside of series (",sdef$series,") range."))
    }
  
    
    #HACK: for now, create dummy fields to prevent having to alias component and pedon summary functions
    #TODO: create proper wrapper functions for pedons versus components, and a suitable generic for unusual cases
    c.s$hzdept <- c.s$hzdept_r
    c.s$hzdepb <- c.s$hzdepb_r
    c.s$clay <- c.s$claytotal_r
    c.s$tax_order <- c.s$taxorder
    c.s$phiid <- c.s$chiid #this is scary
    
    #estimate PSCS, calculate PSCS clay content, compare to series
    cs.pscs <- lapply(profileApply(c.s, estimatePSCS, simplify = F), FUN=function(x) return(data.frame(pscs_lo=x[1], pscs_hi=x[2])))
    cs.pscs.clay <- profileApply(c.s, FUN=function(x) { #calculate component control section clay content
      dfpscs <- cs.pscs[[as.character(site(x)$coiid)]]
      hid <- intersectHorizon(x, dfpscs$pscs_lo, dfpscs$pscs_hi)
      hz <- horizons(c.s)
      hz_overlay <- hz[(hz$chiid %in% hid),]
      hz_overlay <- hz_overlay[order(hz_overlay$hzdept,decreasing=F),]
      hz_overlay$weights <- (hz_overlay$hzdepb - hz_overlay$hzdept)
      return(weighted.mean(hz_overlay$clay, hz_overlay$weights,na.rm = T))
    })
    
    psc.limit <- psc.limits[[which(names(psc.limits) == sdef$pscs_class)]]
    psc.clay_check <- lapply(cs.pscs.clay, FUN=is.between, psc.limit$lo, psc.limit$hi)
    if(any(!unlist(psc.clay_check))) { #if any have restrictions outside the range
      checkcomps <- !unlist((psc.clay_check))
      print(paste0("Component ",paste0(c.s[which(checkcomps),]$dmudesc,c.s[which(checkcomps),]$compname)," depth-weighted average clay content (",round(cs.pscs.clay[checkcomps],2),"%) in PSCS is outside of series (",sdef$series,":",sdef$pscs_class,") range."))
    }
    
    #compare RV surface texture and pH with "allowed" (OSD) classes
    surf.texture <- profileApply(c.s, FUN=function(x) {
      h <- horizons(x)[which(horizons(x)$phiid %in% intersectHorizon(x,getMineralSoilSurfaceDepth(x)+1)),]$texture
    })
    surf.texture_check <- surf.texture%in% strsplit(sdef$surface_txt_class,",")[[1]]
    if(any(!surf.texture_check)) {
      checkcomps <- !unlist(surf.texture_check)
      print(paste0("Component ",paste0(c.s[which(checkcomps),]$dmudesc,c.s[which(checkcomps),]$compname)," RV surface texture class (",surf.texture[checkcomps],") does not match allowed textures (",sdef$surface_txt_class,") for the series (",sdef$series,")"))
    }
    
    surf.ph <- profileApply(c.s, FUN=function(x) {
       h <- round(horizons(x)[which(horizons(x)$phiid %in% intersectHorizon(x,getMineralSoilSurfaceDepth(x)+1)),]$ph1to1h2o_r,1)
    })
    surf.ph_check_lo <- lapply(surf.ph,FUN=function(x) { x >= reaction_classes[[sdef$surf_pH_lo]]$lo } ) #check that hz pH are greater than low class low limit
    surf.ph_check_hi <- lapply(surf.ph,FUN=function(x) { x <= reaction_classes[[sdef$surf_pH_hi]]$hi } ) #check that hz pH are less than high class high limit
    if(any(!unlist(surf.ph_check_lo))) {
       checkcomps <- !unlist(surf.ph_check_lo)
       aliaz <- paste0(c.s[which(checkcomps),]$dmudesc,c.s[which(checkcomps),]$compname)
       print(paste0("Component ",aliaz," surface horizon RV pH (",round(surf.ph[which(checkcomps)],2),") outside low class range (",sdef$surf_pH_lo,":",reaction_classes[[sdef$surf_pH_lo]]$lo,") for the series (",sdef$series,")"))
    }
    if(any(!unlist(surf.ph_check_hi))) {
     checkcomps <- !unlist(surf.ph_check_hi)
     aliaz <- paste0(c.s[which(checkcomps),]$dmudesc,c.s[which(checkcomps),]$compname)
     print(paste0("Component ",aliaz," surface horizon RV pH (",round(surf.ph[which(checkcomps)],2),") outside high class range (",sdef$surf_pH_hi,":",reaction_classes[[sdef$surf_pH_hi]]$hi,") for the series (",sdef$series,")"))
    }
    
    #compare RV argillic/PSCS horizon textures and pH with "allowed" (OSD) classes; 
    # if no argillic, just estimate the PSCS and get all texture classes within it
    arg.texture <- profileApply(c.s, FUN=function(x) {
      bz <- getArgillicBounds(x)
      if(!is.finite(bz$ubound)) {
        foo <- estimatePSCS(x) 
        bz$ubound <- foo[1]
        bz$lbound <- foo[2]
      }
      h <- horizons(x)[which(horizons(x)$phiid %in% intersectHorizon(x,bz$ubound+1,bz$lbound-1)),]$texture #get RV texture classes for all horizons in slice
    },simplify = F)
    
    arg.ph <- profileApply(c.s, FUN=function(x) { #get pH values from all horizons either within the argillic or within the control section (if no argillic)
      bz <- getArgillicBounds(x)
      if(!is.finite(bz$ubound)) {
        foo <- estimatePSCS(x)
        bz$ubound <- foo[1]
        bz$lbound <- foo[2]
      }
      h <- round(horizons(x)[which(horizons(x)$phiid %in% intersectHorizon(x, bz$ubound+1, bz$lbound-1)),]$ph1to1h2o_r, 1) #get RV pH
    },simplify = F)

    #check textures
    arg.texture_check <- lapply(arg.texture,FUN=function(x) { x %in% strsplit(sdef$arg_txt_class,",")[[1]]} )
    comp.arg.texture_check <- lapply(arg.texture_check, all)
    if(any(!unlist(comp.arg.texture_check))) {
      checkcomps <- !unlist(comp.arg.texture_check)
      for(cc in names(checkcomps)) {
        aliaz <- paste0(c.s[which(checkcomps),]$dmudesc,c.s[which(checkcomps),]$compname)
        comptxt <- unlist(arg.texture[checkcomps])[!unlist(arg.texture[checkcomps]) %in% strsplit(sdef$arg_txt_class,",")[[1]]]
      }
      print(paste0("Component ",aliaz," subsurface texture class (",comptxt,") does not match allowed textures (",sdef$arg_txt_class,") for the series (",sdef$series,")"))
    }
    
    #check pH RV is between the low value for the low pH class and the high value for the high pH class
    arg.ph_check_lo <- lapply(arg.ph,FUN=function(x) {
        x[is.na(x)] <- 999 #ignore NA
        return(x >= reaction_classes[[sdef$arg_pH_lo]]$lo)
    } ) #check that hz pH are greater than low class low limit
    arg.ph_check_hi <- lapply(arg.ph,FUN=function(x) { 
         x[is.na(x)] = -1 #ignore NA
         return(x <= reaction_classes[[sdef$arg_pH_hi]]$hi) 
    } ) #check that hz pH are less than high class high limit
    comp.arg.ph_check_lo <- lapply(arg.ph_check_lo, all)
    comp.arg.ph_check_hi <- lapply(arg.ph_check_hi, all)
    if(any(!unlist(comp.arg.ph_check_lo))) { #if any components have RVs below low range value
      checkcomps <- which(!unlist(comp.arg.ph_check_lo)) #get their index in the list
      for(j in 1:length(checkcomps)) { #loop through each component that has one or more horizons with offending values
         hzn <- horizons(c.s[j,])[which(arg.ph_check_lo[[j]]),]
         aliaz <- paste0(c.s[j,]$dmudesc,c.s[j,]$compname,":",hzn$hzname)
         print(paste0("Component ",aliaz," RV pH (",round(hzn$ph1to1h2o_r,2),") outside low class range (",sdef$arg_pH_lo,":",reaction_classes[[sdef$arg_pH_lo]]$lo,"-",reaction_classes[[sdef$arg_pH_lo]]$hi,") for the series (",sdef$series,")"))
       }
    }
    # if(any(!unlist(comp.arg.ph_check_hi))) { #if any components have RVs below low range value
    #   checkcomps <- which(!unlist(comp.arg.ph_check_hi)) #get their index in the list
    #   for(c in 1:length(checkcomps)) { #loop through each component that has one or more horizons with offending values
    #     hzn <- horizons(c.s[c,])[which(arg.ph_check_hi[[c]]),]
    #     aliaz <- paste0(compset[c,]$dmudesc,compset[c,]$compname,":",hzn$hzname)
    #     print(paste0("Component ",aliaz," RV pH (",hzn$ph1to1h2o_r,") outside high class range (",sdef$arg_pH_hi,":",reaction_classes[[sdef$arg_pH_hi]]$lo,"-",reaction_classes[[sdef$arg_pH_hi]]$hi,") for the series (",sdef$series,")"))
    #   }
    # }
  }
}