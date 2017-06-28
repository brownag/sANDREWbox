#interpretation related pedon-summary functions

#' isOrganicHorizon()
#'
#' @param hzname - Horizon designation
#'
#' @return TRUE/FALSE if horizon designation contains the master horizon "O"
#' @export
#'
#' @examples
#' isOrganicHorizon('Oe') #returns TRUE
#' isOrganicHorizon('C') #returns FALSE
isOrganicHorizon <- function(hzname) {
  return(grepl(x=hzname,"O")) #TODO: can use OSM definition if organic matter % is known from lab data
}

#' getClayIncrease
#'
#' @param p - "profile" element of SoilProfileCollection
#'
#' @return \code{getClayIncrease()} returns a vector of values reflecting vertical differences in clay content between mineral horizons.
#' @export
#'
#' @examples
getClayIncrease = function(p) {
  phz=horizons(p)
  if(nrow(phz) > 1) {
    #print(nrow(phz))
    foo = rep(NA,nrow(phz))
    for(h in 2:nrow(phz)) {
      if(!isOrganicHorizon(phz[h-1,]$hzname))
        foo[h] = phz[h,]$clay - phz[h-1,]$clay
      else
        foo[h] = NA
    }
    return(foo)
  }
  return(NA)
}

#' getClayIncreaseDepth
#'
#' @param p - "profile" element of SoilProfileCollection
#' @param thresh - threshold value for clay increase
#'
#' @return \code{getClayIncreaseDepth()} returns the depth at which the clay content change is greater than the user-specified threshold *thresh*.
#' @export
#'
#' @examples
#' \dontrun{getClayIncreaseDepth()}
getClayIncreaseDepth = function(p, thresh) {
  foo <- getClayIncrease(p)
  phz <- horizons(p)
  if(!is.na(foo)) {
    atcdepths = which(foo > thresh)
    if(length(atcdepths) > 0 ) 
      return(phz[atcdepths[1],]$hzdept)
  } 
  return(-1)
}

#' estimateRootingDepth()
#'
#' @param p - "profile" element of SoilProfileCollection
#'
#' @return \code{estimateRootingDepth()} returns the depth (if present) to a "clay pan" (here defined as absolute clay increase of 20%). This does not mirror the exact definition of Abrupt Textural Change, as that may be more restrictive than required. 
#' @export
#'
#' @examples
#' \dontrun{estimateRootingDepth(p)}
estimateRootingDepth = function(p) {
  thresh=20 #(abrupt text change)
  default=estimateSoilDepth(p)
  cid = getClayIncreaseDepth(p, thresh)
  if(cid != -1) return(cid)
  return(default)
}


#' getMineralSoilSurfaceDepth
#'
#' @param p - "profile" element of SoilProfileCollection
#'
#' @return \code{getMineralSoilSurfaceDepth()} returns the thickness of organic soil materials at the soil surface.
#' @export
#'
#' @examples
#' \dontrun{getMineralSoilSurfaceDepth(p)}
getMineralSoilSurfaceDepth <-  function(p) { 
  #assumes OSM is given O designation;
  #TODO: add support for lab-sampled organic measurements
  phz = horizons(p)
  default_t = 0
  if(nrow(phz) > 1) { 
    for(h in 2:nrow(phz))
      if(!grepl(x=phz[h-1,]$hzname,"O")) {
        default_t = phz[h-1,]$hzdept 
        return(default_t)
      }
  }
  return(0)
}

#' surfaceHorizonThickness()
#'
#' @param p - "profile" element of SoilProfileCollection
#' @param hzdesgn - Regular expression pattern for matching horizon designation
#'
#' @return
#' @export
#'
#' @examples
surfaceHorizonThickness <- function(p, hzdesgn) {
  phz <- horizons(p)
  match_desgn <- grepl(phz$hzname,pattern=hzdesgn)
  flag=F; m <- 0 
  if(match_desgn[1]) m=1
  while(!flag & m > 0) {
    if(!match_desgn[m] | m >= length(match_desgn)) flag=T
    m <- m + 1
  }
  if(m > 0) return(phz[m,]$hzdepb)
  else return(-Inf)
}

#' getPlowLayerDepth
#'
#' @param p - "profile" element of SoilProfileCollection
#'
#' @return \code{getPlowLayerDepth()} returns the thickness of Ap horizon(s) at the soil surface. Ignores buried Ap.
#' @export
#'
#' @examples
#' \dontrun{getMineralSoilSurfaceDepth(p)}
getPlowLayerDepth <- function(p) {
  phz=horizons(p)
  if(nrow(phz) >= 1) {
    hasap = grepl(phz$hzname,pattern="Ap[^b]")
    if(sum(hasap) > 0) { #if there are one or more non-buried Ap horizons
      return(max(phz[hasap,]$hzdepb))
    }
  }
  return(-Inf)
}

getClayReqForArgillic <- function(eluvial_clay_content) {
  if(eluvial_clay_content < 15) {
    return(eluvial_clay_content + 3)
  } else if (eluvial_clay_content >= 40) {
    return(eluvial_clay_content + 8)
  } else {
    return(1.2*eluvial_clay_content)
  }
}

getArgillicBounds <- function(p) {
  phz = horizons(p)
  ci <- getClayIncrease(p)
  bounds = c(-Inf,Inf)
  for(h in 2:length(ci)) {
    #add a check for thin (<30cm) transitional horizons where clay increase might be met in non-contiguous hzns? 
    #   does this need to account for boundary distinctness?
    if(!is.finite(bounds[1])) {
      if(!is.na(ci[h])) {
        thresh=getClayReqForArgillic(phz[h-1,]$clay) 
        #add a check for truncated argillics due to erosion or mixing of upper boundary by plowing
        if(phz[h-1,]$clay+ci[h] >= thresh) {
          #TODO: check for "evidence of illuviation" - horizon designation?
          if(grepl(phz[h,]$hzname,pattern="t"))
            bounds[1]=phz[h,]$hzdept
        }
      }
    }
    if(is.finite(bounds[1])) { #we're iterating within the argillic, lets find the bottom
      if(!grepl(phz[h,]$hzname,pattern="t"))
        bounds[2]=phz[h-1,]$hzdepb
    }
  }
  restrictdep <- estimateSoilDepth(p)
  if(is.finite(bounds[1]))
    if(!is.finite(bounds[2]) | bounds[2] > restrictdep) 
      bounds[2] = restrictdep
  
  return(data.frame(ubound=bounds[1],lbound=bounds[2]))
}

estimatePSCS = function(p) {
  soildepth <- estimateSoilDepth(p)
  
  #Parts D (argillic starts >100cm  depth) and F (all other mineral soils)
  default_t = 25
  default_b = 100
  
  #Key part A (soils with restrictio in shallow depth)
  if(soildepth <= 36) {
    default_t = 0
    default_b = soildepth
    return(c(default_t,default_b))
  }
  
  #Key part B (Andisols)
  if(!is.na(p$tax_order))
    if(p$tax_order == "Andisols") {
      default_t = 0
      #nb eliminate organic horizons without andic soil properties
      default_b = 100
    }  
  
  #Adjust PSCS range downward if organic soil material is present at surface (i.e. mineral soil surface depth > 0)
  odepth=getMineralSoilSurfaceDepth(p) 
  print(odepth)
  if(odepth > 0) {
    default_t = default_t + odepth
    if(default_b != soildepth)
      default_b = default_b + odepth
  }
  
  #Key parts C and E (has argillic/kandic/natric WITHIN 100CM)
  argillic_bounds = getArgillicBounds(p)
  if(is.finite(argillic_bounds$ubound)) { 
    default_t=argillic_bounds$ubound
    if(argillic_bounds$ubound <= 100) {
      #Part C - argillic near surface
      #TODO: check arenic and grossarenic subgroups, fragipan depths, strongly contrasting PSCs... should work fine for CA630 though
      if(argillic_bounds$lbound-argillic_bounds$ubound <= 50)
        default_b = argillic_bounds$lbound
      else
        default_b = argillic_bounds$ubound + 50 
    } else if(argillic_bounds$lbound <= 25) {
      default_b = 100
    } 
  }  
  
  #Adjust PSCS top depth to bottom of plow layer (if appropriate)
  plow_layer_depth = getPlowLayerDepth(p)
  if(is.finite(plow_layer_depth))
    if(plow_layer_depth >= 25+odepth) 
      default_t = plow_layer_depth
  
  #Adjust PSCS bottom depth to restriction depth, if appropriate
  if(soildepth < default_b) #truncate to restriction
    default_b = soildepth

  return(c(default_t,default_b))
}



library(soilDB)
pedons = fetchNASIS()
#site(pedons) = cbind(site(pedons),getSoilDepthClass(pedons))

c1=profileApply(pedons, getPlowLayerDepth)
# c2=profileApply(pedons, estimateRootingDepth)

pscsraw = profileApply(pedons,estimatePSCS)
lo = as.logical(1:length(pscsraw) %% 2)
pscs_top = pscsraw[lo]
pscs_bot = pscsraw[!lo]

write.csv(transform(data.frame(pedons$site_id,pedons$taxonname,pedons$psctopdepth,pedons$pscbotdepth,esttop=pscs_top,estbot=pscs_bot, tmatch=(pedons$psctopdepth == pscs_top), bmatch=(pedons$pscbotdepth == pscs_bot)),match=(tmatch==bmatch & tmatch == T)),"CA630_pscs_check.csv")
