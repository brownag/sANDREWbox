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
    foo <- rep(NA,nrow(phz))
    for(h in 2:nrow(phz)) {
      if(!isOrganicHorizon(phz[h-1,]$hzname))
        foo[h] <- phz[h,]$clay - phz[h-1,]$clay
      else
        foo[h] <- NA
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
getMineralSoilSurfaceDepth <-  function(p, hzdesgn='hzname', botdepth='hzdepb') { 
  #assumes OSM is given O designation;
  #TODO: add support for lab-sampled organic measurements
  #      keep organic horizons with andic soil properties
  phz = horizons(p)
  default_t = 0
  if(nrow(phz) > 1) { 
    for(h in 2:nrow(phz)) {
      match.hz <- grepl(x=phz[h-1, hzdesgn], ".*O.*")
      if(length(match.hz)) {
        if(match.hz) {
          default_t <- phz[h-1, botdepth]
        }
      }
    }
    return(default_t)
  }
  return(0)
}

getLabMineralSoilSurfaceDepth <- function(p) {
  return(getMineralSoilSurfaceDepth(p, hzdesgn='hz_desgn', botdepth="hzn_bot"))
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
    if(!is.finite(bounds[1])) { #if we havent found upper bound
      if(!is.na(ci[h])) { #and we are between two mineral horizons (i.e. change in clay content is defined)
        thresh=getClayReqForArgillic(phz[h-1,]$clay) #determine the threshold
        #TODO : check for truncated argillics due to erosion or mixing of upper boundary by plowing
        #        e.g. t subscript auto-starts argillic regardless of clay? that won't quite work because it will break cases where have clay films and limited clay increase relative to the eluvial
        #           t subscript immediately under p subscript => autostart argillic, assuming that unmixed there would be a clay increase and the Ap can't be a part of argillic?
        #           t subscript ON SURFACE HORIZON (implies erosion) => autostart argillic
        #       These types of logic could create an element of unpredictability with these calculations by loosening logic for natural soils in an (incomplete) attempt to deal with human-modified edge cases. Technically,the upper bound of argilic is where clay increase is met. Period. But there are cases where an argillic is recognized in a soil that no longer has an eluvial horizon in the profile. I *think* most soils in a semi-natural state will have a non-argillic surface horizon, and ideally one that would "calculate" as an eluvial (even if it did not actually contribute the clay to the underlying illuvial hz) if the argillic starts immediately beneath, but human-modification and recently disturbed sites will still be problematic.
        #       
        #       If this is implemented as a check against the diagnostic table, there will be always opportunity to except "wierd" cases
        #        might be best to just let fn predict argillic "wrong" or "absent" in eroded/mixed surface case and have \
        #        soil scientist verify that diagnostic table is correct (since basic morphologic data e.g. clay content
        #        cannot be used to delineate boundaries of diagnostic hz in the human-altered cases, requires site context)
        if(phz[h-1,]$clay+ci[h] >= thresh) {
          if(grepl(phz[h,]$hzname,pattern="t"))#TODO: check for P&V table for "evidence of illuviation" ?
            bounds[1]=phz[h,]$hzdept
        }
      }
    }
    
    if(is.finite(bounds[1])) { #we're iterating within the argillic, lets take the bottom depth
      if(!grepl(phz[h,]$hzname, pattern="t")) { #if the next horizon lacks a "t" subscript 
        bounds[2] <- phz[h, ]$hzdept
        break;
      }
      if(h == length(ci)) # or if we are in last horizon in pedon
        bounds[2] <- phz[h, ]$hzdepb
    }  
  }
  restrictdep <- estimateSoilDepth(p)
  if(is.finite(bounds[1]) & !is.finite(bounds[2]) | bounds[2] > restrictdep) 
    bounds[2] = restrictdep #catches cases e.g. Crt where argillic bdepth could go deeper than the "soil"
  return(data.frame(ubound=bounds[1],lbound=bounds[2]))
}

estimatePSCS = function(p, tax_order_field="tax_order") {
  soildepth <- estimateSoilDepth(p)
  
  #Parts D (argillic starts >100cm  depth) and F (all other mineral soils)
  default_t = 25
  default_b = 100

  
  #Key part A (soils with restrictio in shallow depth)
  if(soildepth <= 36) {
    default_t = 0
    default_b = soildepth
  }
  
  #Key part B (Andisols)
  if(!is.na(site(p)[tax_order_field]))
    if(site(p)[tax_order_field] == "andisols") {
      default_t = 0
      default_b = 100
    }  
  
  #Adjust PSCS range downward if organic soil material is present at surface (i.e. mineral soil surface depth > 0)
  odepth <- getMineralSoilSurfaceDepth(p) 
  if(odepth > 0) {
    default_t = default_t + odepth
    if(default_b != soildepth)
      default_b = default_b + odepth
  }
  
  #Key parts C and E (has argillic/kandic/natric WITHIN 100CM)
  if(is.na(site(p)[tax_order_field]) | site(p)[tax_order_field] != "andisols") {
    argillic_bounds = getArgillicBounds(p)
    if(is.finite(argillic_bounds$ubound)) { 
      if(argillic_bounds$ubound<100) {
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
    }  
  }
  
  #Adjust PSCS top depth to bottom of plow layer (if appropriate)
  plow_layer_depth = getPlowLayerDepth(p)
  if(is.finite(plow_layer_depth))
    if(plow_layer_depth >= 25+odepth) 
      default_t = plow_layer_depth
  
  #Adjust PSCS bottom depth to restriction depth, if appropriate
  if(soildepth < default_b) {#truncate to restriction
    default_b = soildepth
  }
  
  return(as.numeric(c(default_t,default_b)))
}

get_raw_colors_from_NASIS_db <- function ()  {
  if (!requireNamespace("RODBC")) 
    stop("please install the `RODBC` package", call. = FALSE)
  q <- "SELECT peiid, phiid, ms.ChoiceLabel AS colormoistst, colorpct as pct, mh.ChoiceName AS colorhue, mv.ChoiceName AS colorvalue, mc.ChoiceName AS colorchroma\nFROM\n  pedon_View_1 INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref\n  INNER JOIN phcolor_View_1 ON phorizon_View_1.phiid = phcolor_View_1.phiidref\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1242) AS mh ON phcolor_View_1.colorhue = mh.ChoiceValue\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1244) AS mv ON phcolor_View_1.colorvalue = mv.ChoiceValue\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1241) AS mc ON phcolor_View_1.colorchroma = mc.ChoiceValue\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1243) AS ms ON phcolor_View_1.colormoistst = ms.ChoiceValue\n  ORDER BY phiid, colormoistst;"
  channel <- RODBC::odbcDriverConnect(connection = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  #d.final <- simplifyColorData(d)
  return(d)
}

get_dominant_raw_colors_from_NASIS_db <- function() {
  hz_color_nasis <- get_raw_colors_from_NASIS_db() #need the "unmixed" colors from NASIS
  ll <- lapply(split(hz_color_nasis,f=hz_color_nasis$phiid), function(df) {

    idx.moist <- which(df$colormoistst == "Moist")
    idx.dry <- which(df$colormoistst == "Dry")
    if(length(idx.moist) > 0 | length(idx.dry) > 0) {
      hzpeiid <- df$peiid[1]
      hzphiid <- df$phiid[1]
      
      moistcol <- df[idx.moist[order(df$pct,decreasing=F)][1],]
      drycol <- df[idx.dry[order(df$pct,decreasing=F)][1],]
      
      outdf <- data.frame(peiid=hzpeiid, phiid=hzphiid, rd_hue=drycol$colorhue, 
                          rd_value=drycol$colorvalue, rd_chroma=drycol$colorchroma, rm_hue=moistcol$colorhue, 
                          rm_value=moistcol$colorvalue, rm_chroma=moistcol$colorchroma) #make template dataframe
      return(outdf)
    }
  })
  outdf <- data.frame(peiid=numeric(0), phiid=numeric(0), rd_hue=character(0), 
                      rd_value=numeric(0), rd_chroma=numeric(0), rm_hue=character(0), 
                      rm_value=numeric(0), rm_chroma=numeric(0))
  for(i in 1:length(ll))
    outdf <- rbind(outdf, ll[[i]])
  return(outdf)
}

hasDarkColors <- function(dry_val, moi_val, moi_chr, val_dry=5, val_moist=3, chr_moist=3, require_chroma=TRUE,  remove.na=TRUE) {
  f.dry_val <- TRUE
  f.moi_val <- TRUE
  f.moi_chr <- TRUE
  
  #print(c(dry_val, moi_val, moi_chr))
  if(length(dry_val)) { #if have a record for dry value
    if(!is.na(dry_val)) { # and that record is not empty
      if(dry_val > val_dry) # if value greater than threshold, not a dark color
        f.dry_val <- FALSE
    } else if(remove.na) f.dry_val <- TRUE
  }
  
  if(length(moi_val)) {
    if(!is.na(moi_val)) {
      if(moi_val > val_moist)
        f.moi_val <- FALSE    
    } else if(remove.na) f.dry_val <- TRUE

  } 
  
  if(length(moi_chr))
    if(!is.na(moi_chr)) {
      if(moi_chr > chr_moist)
        f.moi_chr <- FALSE
        if(!require_chroma)
          f.moi_chr <- TRUE
    } else if(remove.na) f.moi_chr <- TRUE
  
  if(any(!c(f.dry_val, f.moi_val, f.moi_chr))) {
    return(FALSE)
  }
  return(TRUE)
}

hasDarkMineralSurface <- function(p, bounds=FALSE, val_dry=5, val_moist=3, chr_moist=3, require_chroma=TRUE, remove.na=TRUE) {
  min_surface <- getMineralSoilSurfaceDepth(p) 
  minimum_thickness <- 18 # upper 18cm of mineral are diagnostic for colors; but thickness varies depending on below criteria
  #TODO: minimum thickness requirements
  # - default: 18
  # - maximum: 25
  # - if shallower than 18cm, not sandy, and dark surface overlies contact: 10 (and color averaged over whole thickness)
  # - otherwise: 1/3 of distance to: 
  #     shallowest of (upper boundary of) 2ndary carbonates, calcic, petrocalcic, duripan, fragipan
  #     deepest of (lower boundary of) argillic, cambic, natric, oxic or spodic horizon
  diag_depth <- min_surface+minimum_thickness #is mixing always over upper 18? or do you need to mix over 25 if applicable?
  soil_depth <- estimateSoilDepth(p)
  dark_lbound <- min_surface #default is no epipedon
  
  if(soil_depth < diag_depth) #soils shallower than 18cm -> use the whole thing
    diag_depth <- soil_depth
  
  hz_color_nasis <- get_dominant_raw_colors_from_NASIS_db() #need the "unmixed" colors from NASIS, one each for moist and dry if available
  hz <- horizons(p)
  hz <- merge(hz, hz_color_nasis, by ="phiid")
  hz <- hz[order(hz$hzdept,decreasing=F),]
  if(length(hz)) {
    hid <- intersectHorizon(p, min_surface, diag_depth) #start by checking whether the 18cm mixed reqs are met, if these aren't met then no need to look deeper
    hz_overlay <- hz[(hz$phiid %in% hid),]
    hz_overlay <- hz_overlay[order(hz_overlay$hzdept,decreasing=F),]
    hz_overlay$weights = (hz_overlay$hzdepb - hz_overlay$hzdept)
    cuml_weights <- cumsum(hz_overlay$weights)
    outside_diag <- which(cuml_weights > 18)
    partial = outside_diag[1] #take the first element where the cumulative sum exceeds the minimum diagnostic thickness
    if(length(partial) > 0 & !is.na(partial)) {
      if(partial > 1) {
        hz_overlay$weights[outside_diag] <- 0 # set all subsequent weights to zero (really only should be one value in here if intersect works)
        hz_overlay$weights[partial] = (18-cuml_weights[partial-1]) # augment the weight for the bottom-most (partial) horizon to reflect the portion that is within boundaries
      }
    }
    
    #calculate mixed (depth-weighted average) values and chroma
    if(length(hz_overlay$rd_value) > 1) {
      dry_val <- weighted.mean(hz_overlay$rd_value, hz_overlay$weights, na.rm = remove.na)
    } else dry_val <- hz_overlay$rd_value
    if(length(hz_overlay$rm_value) > 1) {
      moi_val <- weighted.mean(hz_overlay$rm_value, hz_overlay$weights, na.rm = remove.na)
    } else moi_val <- hz_overlay$rm_value
    if(length(hz_overlay$rm_chroma) > 1) {
      moi_chr <- weighted.mean(hz_overlay$rm_chroma, hz_overlay$weights, na.rm = remove.na)
    } else moi_chr <- hz_overlay$rm_chroma
    
    #check whether the mixed 18cm surface meets base requirements for epipedon
    mixed_dark_surface <- hasDarkColors(dry_val, moi_val, moi_chr, val_dry=val_dry, val_moist=val_moist, chr_moist=chr_moist)
    
    if(!bounds) #if bounds is FALSE, don't return the boundaries, just TRUE/FALSE for dark surface
      return(mixed_dark_surface)
    
    #if bounds=TRUE, then we return the upper and lower depth of the dark surface
    # weighted averaging no longer matters, just need to take the max depth that meets color reqs, and truncate to argillic upper bound
    bound_subset <- intersectHorizon(p, min_surface, soil_depth) #get all mineral horizons
    argillic_ubound <- getArgillicBounds(p)$ubound
    
    ldarkcol <-lapply(split(hz, f = hz$phiid), FUN=function(df) {
      hasDarkColors(df$rd_value, df$rm_value, df$rm_chroma, val_dry=val_dry, val_moist=val_moist, chr_moist=chr_moist)
    })
  
    # has_dark_colors retains the depth ordering, regardless of the numeric value of phiid
    hz$has_dark_colors <- unlist(ldarkcol)[order(hz$phiid)]
    
    if(!is.null(hz$has_dark_colors)) {
      idx <- which(hz$has_dark_colors & (hz$phiid %in% bound_subset)) #look at mineral horizons with dark colors
      if(length(idx)) { #at least one hz with dark colors
        discont <- which(idx >= idx[c(1,diff(idx)) > 1][1])
        if(length(discont))
           idx <- idx[-discont] #drop discontinuous dark horizons (assumes for now that uppermost hz is ok) TODO: test logic
        if(length(idx)) {
          dark_hz <- hz[idx,]
          if(dark_hz[1,]$hzdept == min_surface) { # now check that first mineral is dark!
            if(nrow(dark_hz) > 1) { # dark layer includes more than one dark horizon
              for(d in 2:nrow(dark_hz)) {
                mizz <- is.na(dark_hz[d,]$hzdept) | is.na(dark_hz[d-1,]$hzdepb)
                if(mizz) 
                  dark_hz <- dark_hz[-d, ] #remove horizons without necessary depths
                else if(dark_hz[d,]$hzdept != dark_hz[d-1,]$hzdepb) #if the next horizon meeting color req is not continuous with the previous
                  dark_hz <- dark_hz[-d, ] #remove it
              }
              dark_thickness <- sum((dark_hz$hzdepb-dark_hz$hzdept)) #calculate thickness of continuous dark horizons
              if(dark_thickness >= minimum_thickness) { #if thickness of dark horizons is > minimum
                dark_lbound <- max(dark_hz$hzdepb) #take the max bottom depth as the lower boundary
              } else if(dark_thickness < minimum_thickness) { # whole dark horizon thicknesses sum to less than 18cm, still might meet dark surface 
                if(dark_thickness > 10 & soil_depth < minimum_thickness) { #check for shallow soils (omit really thin epipedons)
                  #TODO: hardcoded 10cm minimum for shallow case requires non-sandy textures
                  if(max(dark_hz$hzdepb) == soil_depth) dark_lbound <- soil_depth
                }
                if((dark_thickness < minimum_thickness) & mixed_dark_surface) { #if the weighted averaging of a darkened and less dark hz got us below threshold
                  dark_lbound <- diag_depth # use mineral surface + "minimum" thickness as lower bound (TODO: criteria for minimum thickness of epipedon)
                }
              }
            } else { #one dark horizon, lower bound of potential epipedon is bottom depth
              dark_lbound <- max(dark_hz$hzdepb) 
              if(dark_lbound - min_surface < minimum_thickness) {
                dark_lbound <- min_surface #if we have a dark surface horizon, but it is not thick enough, no dark surface
                if(mixed_dark_surface) {
                  #BUT if we made it by weighted averaging, use depth we averaged over
                  dark_lbound <- diag_depth
                }
              }
            }
          } else dark_lbound <- min_surface # first mineral horizon isnt dark, no dark surface
        } else dark_lbound <- min_surface # no continuous horizons with dark surface
      } else dark_lbound <- min_surface # no horizons with dark colors, no dark surface
      
      if(length(argillic_ubound))
        if(argillic_ubound != -Inf) {
          if(dark_lbound > argillic_ubound)
            dark_lbound <- argillic_ubound
            if(mixed_dark_surface) {
              #BUT if we made it by weighted averaging, use depth we averaged over
              dark_lbound <- diag_depth
            }
        }
      
      if(dark_lbound > soil_depth) 
        dark_lbound <- soil_depth
    }
  } else {
    #no color data at all
    print(paste0("Pedon (", hz$peiid, ") lacks all color data; impossible to determine boundaries. Returning NA."))
    dark_lbound <- min_surface
  }
  if(min_surface == dark_lbound) {
    #no dark surface, figure out the boundaries for ochric
    ochric_top=0 #includes organic soil materials too thin for histic/folistic
    ochric_bot=18
    if(argillic_ubound != -Inf)
      ochric_bot = argillic_ubound
    return(data.frame(dsubound=ochric_top, dslbound=ochric_bot, is_ochric=T))
  }
    
  return(data.frame(dsubound=min_surface, dslbound=dark_lbound, is_ochric=F))
}

is.between <- function(x, a, b) { 
  x <- as.numeric(as.character(x)) #ensure that we will be able to evaluate, coerce to numeric
  if(all(!is.null(a),!is.null(b)))
    if(all(!is.na(x),!is.na(a),!is.na(b),length(x)>0,length(a)==1,length(b)==1))
      if(as.numeric(x) <= b & as.numeric(x) >= a) 
        return(TRUE)
  return(FALSE)
}

intersectPedonHorizon <- function(pedon, z1, z2=NULL) {
  #alias function; default arguments work with pedons (NASIS)
  return(intersectHorizon(pedon, z1, z2)) #returns list of pedon horizon ids (phiid)
}


intersectLabHorizon <- function(pedon, z1, z2=NULL) {
  #alias function for lab pedons (KSSL)
  return(intersectHorizon(pedon, z1, z2, topdepth='hzn_top', botdepth='hzn_bot', hzid='labsampnum')) #returns list of lab sample #'s
}


intersectComponentHorizon <- function(pedon, z1, z2=NULL) {
  #alias function for components (NASIS)
  return(intersectHorizon(pedon, z1, z2, hzid='chiid')) #returns list of component horizon ids
}

intersectHorizon <- function(pedon, z1, z2=NULL, topdepth='hzdept', botdepth='hzdepb', hzid='phiid') {
  hz <- horizons(pedon)
  if(!is.null(z2)) { # if a top and bottom depth are specified, we may intersect multiple horizons
    foo <- numeric(0)
    for(h in 1:nrow(hz)) {
      hh <- hz[h,]
      if(is.between(hh[, topdepth], z1, z2) | is.between(hh[, botdepth], z1, z2)) 
        foo <- c(foo, hh[, hzid]) # if one or both horizon boundaries falls between z1, z2 add pedon horizon to list
    }
    return(foo)
  } else { # if just z1 is specified, we will return 1 horizon ID using default "within" logic (less than or equal to) for tie breaking
    for(h in 1:nrow(hz)) {
      hh <- hz[h,]
      if(is.between(z1, hh[,topdepth], hh[,botdepth])) 
        return(hh[, hzid])
    }
  }
}

getHorizonAt50cm <- function(p) {
  hz <- horizons(p)
  hid <- intersectHorizon(p, 50)
  return(hz[(hz$phiid %in% hid),])
}

getHorizons50to100cm <- function(p) {
  hz <- horizons(p)
  hid <- intersectHorizon(p, 50, 100)
  return(hz[(hz$phiid %in% hid),])
}



