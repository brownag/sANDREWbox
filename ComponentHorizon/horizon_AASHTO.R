## component weighted average aashto explorer
get_component_AASHTO_table <- function() {
  if (!requireNamespace("RODBC")) 
    stop("please install the `RODBC` package", call. = FALSE)
  q.cha <- "SELECT chiidref, chaashtoiid, aashtocl, rvindicator FROM chaashto cha;" #ORDER BY coiidref, hzdept_r ASC
  channel <- RODBC::odbcDriverConnect(connection = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d.cha <- RODBC::sqlQuery(channel, q.cha, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  return(d.cha)
}

get_component_engineering_ranges <- function() {
  if (!requireNamespace("RODBC")) 
    stop("please install the `RODBC` package", call. = FALSE)
  q.cha <- "SELECT chiid, sieveno200_l, sieveno200_r, sieveno200_h, lep_l, lep_r, lep_h, ll_l, ll_r, ll_h, pi_l, pi_r, pi_h, aashind_l, aashind_r, aashind_h FROM chorizon ch;" #ORDER BY coiidref, hzdept_r ASC
  channel <- RODBC::odbcDriverConnect(connection = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  d.cha <- RODBC::sqlQuery(channel, q.cha, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  return(d.cha)
}

is.between <- function(x, a, b) { 
  x <- as.numeric(as.character(x)) #ensure that we will be able to evaluate, coerce to numeric
  if(all(!is.na(x),!is.na(a),!is.na(b),!is.null(a),!is.null(b),length(x)>0,length(a)==1,length(b)==1))
    if(as.numeric(x) <= b & as.numeric(x) >= a) 
      return(TRUE)
  return(FALSE)
}

intersectComponentHorizon <- function(comp, z1, z2=NULL) {
  hz <- horizons(comp)
  if(!missing(z2)) { # if a top and bottom depth are specified, we may intersect multiple horizons
    foo <- numeric(0)
    for(h in 1:nrow(hz)) {
      hh <- hz[h,]
      if(is.between(hh$hzdept_r, z1, z2) | is.between(hh$hzdepb_r, z1, z2)) 
        foo <- c(foo, hh$chiid) # if one or both horizon boundaries falls between z1, z2 add pedon horizon to list
    }
    return(foo)
  } else { # if just z1 is specified, we will return 1 horizon ID using default "within" logic (less than or equal to) for tie breaking
    for(h in 1:nrow(hz)) {
      hh <- hz[h,]
      if(is.between(z1, hh$hzdept_r, hh$hzdepb_r)) 
        return(hh$chiid)
    }
  }
}

depth.weighted.average <- function(p, attr=NA, ubound, lbound) {
  if(!is.na(attr) | !(attr %in% names(horizons(p)))) {
    hz <- horizons(p)
    
    #get horizons overlapping depth range of interest
    hz_overlay <- hz[(hz$chiid %in% intersectComponentHorizon(p, ubound, lbound)), ]
    hz_overlay <- hz_overlay[order(hz_overlay$hzdept, decreasing=F), ]
    hz_overlay$weights = (hz_overlay$hzdepb - hz_overlay$hzdept) #calculate weightings (assuming no partial horizons)
    
    #first and last horizons may only partially contribute to the average
    for(h in 1:nrow(hz_overlay)) {
      cur <- hz_overlay[h, ]
      if(cur$hzdept_r < ubound) hz_overlay$weights[h] <- (cur$hzdepb_r - ubound) # portion of the first horizon below the upper bound
      if(cur$hzdepb_r > lbound) hz_overlay$weights[h] <- (lbound - cur$hzdept_r) # portion of the last horizon above the lower bound
    }
    
    return(weighted.mean(hz_overlay[[attr]], w=hz_overlay$weights))
  } else {
    stop(paste0("No attribute specified to calculate depth-weighted average or attribute ",attr,"does not exist!"))
  }
}

weightedAverageAASHTO <- function(comp, top, bottom) {
  l <- depth.weighted.average(comp, attr='aashind_l', top, bottom)
  r <- depth.weighted.average(comp, attr='aashind_r', top, bottom)
  h <- depth.weighted.average(comp, attr='aashind_h', top, bottom)
  return(data.frame(low=l, rv=r, hi=h))
}

#Code for working with the AASHTO table and RV classes
# #aashto_table <- get_component_AASHTO_table() #get AASHTO table
# 
# aashto_table <- aashto_table[which(aashto_table$rvindicator == 1),] # filter to RVs; ideal result in a 1:1 chorizon to chaashto *crosses fingers*
# 
# #sanity check #1
# aashto_per_hz <- aggregate(aashto_table$aashtocl, by=list(aashto_table$chiidref), FUN=function(x) return(length(levels(factor(x)))))
# names(aashto_per_hz) <-  c("phiid","number of RV AASHTO records")
# aashto_per_hz[which(aashto_per_hz[,2] >= 2),] #none of my dmus have multiple RVs at least ;)
# 
# #do the join (assuming you have made a 1:1 relatuion)
# f.bak <- f
# horizons(f) <- merge(horizons(f), aashto_table, by.x = "chiid", by.y="chiidref", all.x = T)
# 
# #sanity check #2
# nrow(horizons(f.bak)) == nrow(horizons(f)) #if this is FALSE then the join is not  1:1; 
##number of merged horizons can never be less than you started with, but may get extra (overlapping) records if you have multiple RVs (not allowed!)
