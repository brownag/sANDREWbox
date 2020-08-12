#Appendix B. - Guide for Placing Soils in Capability Subclasses in California - Guides A and B
#(Only soils in capability classes 2 through 7 are assigned to a subclass.)

#criteria in guide A and B are the same, but subclass by slope % varies

#NOTES (AGB):
# Somewhat poorly drained soils that lack a slow/v.slow permeability might fall through; be sure the logic is sound or generate a warning

#I: depth class (moderately deep or deeper)
  #1: moderately slowly to very rapidly permeable
      #1.1: moderately well to excessively well drained
        #1.1a: surface texture "fine or very fine"
        #1.1b: surface texture "moderately fine"
        #1.1c: surface texture "medium"
        #1.1d: surface texture "moderately coarse"
        #1.1e: surface texture "coarse or very coarse"
          #1.1e.i: has textural B horizon
          #1.1e.ii: lacks textural B horizon
  #2: slowly or very slowly permeable
    #2.1: well or moderately well drained
    #2.2: somewhat poorly drained
      #2.2a: not a Histosol 

#II: wet soils (poorly and very poorly drained; also SPD if "coarse textured" or "deep organic" i.e. II.1.2 & II.1.3)
  #II.1.1: surface texture "moderately coarse to fine"
  #II.1.2: coarse textured, lacks textural B
  #II.1.3: depth class (moderately deep or deeper)
    #II.1.3 organic soils 

#III: depth class (shallow or very shallow)
  #III.1 Excessively to moderately well drained
    #III.1.1: very shallow
    #III.1.2: shallow
  #III.2 Somewhat poorly drained (gets 's' subclass)

#IV: saline/sodic ("moderate to severe salinity and sodicity")

#V: >35% rock fragments in surface layers (very or extremely modifier)

#VI: soils subject to damaging overflow (landform contains "flood plain"?)
#
source('PedonSummaryFunctions/pedon_summary_functions.R')
library(aqp)
library(soilDB)

m_sc <- read.csv("LCC/subclass_matrix.csv")

c <- fetchNASIS("components")

#hacky aliasing
c$hzdept <- c$hzdept_r
c$hzdepb <- c$hzdepb_r
c$clay <- c$claytotal_r

estimateMostLimitingKsat <-  function(x) {
  chz.idx <- which(x$ksat_l == min(x$ksat_l))
  if(length(chz.idx))
     return(x$ksat_l[chz.idx[1]])
  else return(NA)
}

hasTexturalB <- function(x) {
  x.arg <- getArgillicBounds(x)
  if(all(is.finite(as.numeric(x.arg))))
    return(TRUE)
  return(FALSE)
}

getSubclass <- function(cc) {
  row.name <- getSubclassSoilPropertyCriterion(cc)
  crit.ids <- c('1a','1b','1b2','1c','1c2','1d','1d2','1e','1f','2a','2b','3a','3b','3c','4a','4b','5','6','7')
  row.idx <- which(crit.ids %in% row.name)
  col.idx <- getSubclassSlopeCriterion(cc)
  return(as.character(m_sc[row.idx, col.idx]))
}

getSubclassSlopeCriterion <- function(cc) {
  slope_high <- cc$slope_h
  if(slope_high <= 2) {
    return(1)
  } else if(slope_high <= 9) {
    return(2)
  } else if(slope_high <= 15) {
    return(3)
  } else if(slope_high <= 50) {
    return(4)
  } else {
    return(5)
  }
}

getSurfaceProperty <- function(cc, prop) {
  surface.prop <- glom(cc, getMineralSoilSurfaceDepth(cc)+1, df = TRUE)[, prop]
    #horizons(cc)[cc$chiid %in% intersectComponentHorizon(cc, getMineralSoilSurfaceDepth(cc)+1), prop]
}

getSubclassSoilPropertyCriterion <- function(cc) {
  ###PULL OUT THE ODDBALLS FIRST
  
  #VI: soils subject to damaging overflow (landform contains "flood plain"? -- should flood-plain steps be included?) or use flooding freq?
  if(grepl(cc$landform_string, pattern="flood.plain")) {
    #7 soils with damaging overflow
    return("7")
  }
  
  #V: >35% rock fragments in surface layers (very or extremely modifier)
  if(cc$surface.rf.high > 35) {
    #6 high fragment volume in surface horizon
    return("6")
  }
  
  #IV: saline/sodic ("moderate to severe salinity and sodicity")
  if(any(na.omit(cc$ec_r) > 16) | any(na.omit(cc$sar_r) > 25)) { 
    #5 salt affected
    return("5")
  }
  
  if(cc$restr_depth > 50) {
    if(cc$restr_ksat > 1) { # moderately high Ksat ~= moderately slow permeability (Field Book pp 2-84)
      #1a: surface texture "fine or very fine"
      if(cc$surface.txclay.high > 35)
        return("1a")
      buf <- ""
      #1b: surface texture "moderately fine"
      if(cc$surface.txclay.high > 27)
        buf = "1b"
      #1c: surface texture "medium"
      if(cc$surface.txclay.high > 18)
        buf = "1c"
      #1d: surface texture "moderately coarse"
      if(cc$surface.txclay.high > 10)
        buf = "1d"
      if(cc$slope_h <=2 &cc$restr_depth > 100 & buf != "") {
        #mod fine to mod coarse soils >100cm deep on slopes less than 2% often come out class 1 irrigated
        #give them a "c" subclass dryland (validate later if they are class 1 irrigated)
        buf <- paste0(buf, "2")
      }
      if(buf != "") return(buf)
      #1e&f: surface texture "coarse or very coarse" has/lacks textural B horizon
      if(cc$has_textural_B) {
        return("1e")
      } else {
        return("1f")
      }
    } else { #very slow or slow perm
      if(cc$drainagecl == "well" | cc$drainagecl == "moderately well") {
        #2a WD & MWD
        return("2a")
      } else if (cc$drainagecl == "somewhat poorly") {
        if(cc$taxorder != "histosols") {
          #2B SPD
          return("2b")
        }
      }
    }
  }
  
  if(cc$drainagecl == "poorly" | cc$drainagecl == "very poorly" | (cc$drainagecl == "somewhat poorly" & 
     ((cc$taxorder == "histosols" & cc$restr_depth > 50) | (!cc$has_textural_B & cc$pscs_clay <= 18)))) {
    if(!cc$has_textural_B & cc$pscs_clay <= 18) {
      #3b coarse textured soils
      return("3b")
    }
    if(cc$taxorder == "histosols" & cc$restr_depth > 50) {
      #3c deep organic soils (is the restriction depth required? taxord=histosols requires "deep" OSM by definition)
      return("3c")
    }
    return("3a")
  }
  
  if(cc$restr_depth <= 50 | cc$drainagecl == "somewhat poorly") {
    #4a shallow soils
    if(cc$restr_depth <= 25) {
      #4b very shallow soils
      return("4b")
      #footnote: SPD soils included here
    }
    return("4a")
  }
}

get_legend_from_NASIS_db <- function() {
  
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT nationalmusym, dmuiid, dmudesc, coiid, musym, muname, mustatus, muacres FROM legend AS l
          INNER JOIN lmapunit AS lmu ON lmu.liidref = l.liid
          INNER JOIN mapunit AS mu ON mu.muiid = lmu.muiidref
          INNER JOIN correlation AS cor ON cor.muiidref = mu.muiid
          INNER JOIN datamapunit AS dmu ON dmu.dmuiid = cor.dmuiidref
          INNER JOIN component AS co ON co.dmuiidref = dmu.dmuiid 
        WHERE cor.repdmu = 1 AND lmu.mustatus != 4"
  
  channel <- soilDB:::.openNASISchannel()
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  RODBC::odbcClose(channel)
  return(d)
}
 
getCapabilityUnit <- function(cc) {
  if(any(grepl(horizons(cc)$texture, pattern="PT|MK"))) {
    return(10)
  }
  if(cc$drainagecl == 'somewhat poorly' | cc$drainagecl == 'poorly') { #also check flooding and ponding
    return(2)
  } 
  if(any(na.omit(cc$ec_r) > 16) | any(na.omit(cc$sar_r) > 25)) {
    return(6)
  }
  if(grepl(cc$taxclname,pattern="magnesic,") | all(cc$ph1to1h2o_h < 5.5)) {#pull out magnesic and soils with acid reaction class 
    return(9)
  }
  if(any(grepl(cc$hzname,pattern = "Cr|R|.*qm")) & cc$restr_depth < 150) { #could use "strongly or more cemented" rather than hzdesgn
    return(8) #NB: need to remove hard unweathered bedrock and hardpan before doing ksat
  }
  if(cc$restr_ksat < 0.1) { #assume that Ksat mod. low or lower == permeability slow/v. slow
    return(3)
  }
  if(cc$pscs_clay < 10 & cc$pscs_frags > 15) {
    return(4)
  }
  if(cc$pscs_clay > 35) {
    return(5)
  }
  if(cc$surface.rf.high > 15) {
    return(7)
  }
  n.hz <- nrow(horizons(cc))
  if(n.hz > 1) {
    substrata.hz <- na.omit(horizons(cc)[(n.hz-1):n.hz,])
    if(any(substrata.hz$texture == "S" | substrata.hz$texture == "COS") | any(substrata.hz$fragvoltot_r >= 35)) {
      return(11)
    }
  }  
  return(1)
}

#Calculate various soil properties for all components
c$restr_depth <- profileApply(c, estimateSoilDepth, top="hzdept_r", bottom="hzdepb_r", p="Cr|R|Cd|.*qm")
c$restr_ksat <- profileApply(c,  estimateMostLimitingKsat)
c$has_textural_B <- profileApply(c, hasTexturalB)
c$pscs_clay <- profileApply(c, getPSCSclay, dtype="component")
c$pscs_frags <- profileApply(c, getPSCS, attr="fragvoltot_h", dtype="component")
c$surface.rf.high <- profileApply(c, getSurfaceProperty, prop='fragvoltot_h')
c$surface.txclay.high <- profileApply(c, getSurfaceProperty, prop='claytotal_h')

#Calculate LC subclass
c$subclass_prop_crit <- profileApply(c, getSubclassSoilPropertyCriterion)
c$subclass_slope_crit <- profileApply(c, getSubclassSlopeCriterion)
c$subclass_calc <- profileApply(c, getSubclass)

#Calculate LC unit
c$unit_calc <- profileApply(c, getCapabilityUnit)

plot(factor(c$subclass_calc[which(as.numeric(c$nirrcapcl) > 1 & as.numeric(c$nirrcapcl) < 8)]), 
     main="Land Capability Subclasses", sub="for soils in LCC classes 2 through 7")

plot(factor(c$unit_calc[which(as.numeric(c$nirrcapcl) > 1 & as.numeric(c$nirrcapcl) < 5)]),
     main="Land Capability Units", sub="for soils in LCC 2, 3 or 4")

plot(factor(c$nirrcapscl, levels = c("c","e","s","w")), 
     main="LC Subclass Breakdown (LCC 2 to 7)", sub="NASIS values")

plot(factor(c$subclass_calc, levels = c("c","e","s","w")), ylim=c(0,350), sub="Calculated values")

plot(factor(c$nirrcapunit[which(as.numeric(c$nirrcapcl) > 1 & as.numeric(c$nirrcapcl) < 5)]),
     main="Land Capability Unit Breakdown (LCC 2 to 4)", sub="NASIS values")

plot(factor(c$unit_calc[which(as.numeric(c$nirrcapcl) > 1 & as.numeric(c$nirrcapcl) < 5)]), 
     sub="Calculated")

c[which(c$unit_calc == 3), ]$compname

df.out <- merge(site(c), get_legend_from_NASIS_db(), all.x=T)
df.out.sub <- df.out[,c('musym','dmudesc','compname','comppct_r', 'compkind', 'drainagecl', 'slope_h', 'nirrcapcl', 'nirrcapscl', 'nirrcapunit', 'irrcapcl', 'irrcapscl', 'irrcapunit', 'landform_string','restr_depth','has_textural_B',"pscs_clay",'surface.rf.high','surface.txclay.high', 'subclass_prop_crit', 'subclass_slope_crit', 'subclass_calc', 'unit_calc')]

dsof <- df.out.sub
dsof$nirrcapcl <- as.numeric(dsof$nirrcapcl)
dsof$irrcapcl <- as.numeric(dsof$irrcapcl)
dsof$nirrcapscl <- as.character(dsof$nirrcapscl)
dsof$nirrcapunit <- as.character(dsof$nirrcapunit)
dsof$irrcapscl <- as.character(dsof$irrcapscl)
dsof$irrcapunit <- as.character(dsof$irrcapunit)

dsof[which(dsof$nirrcapcl == 1 | dsof$nirrcapcl == 8),]$subclass_calc <- "NA"
dsof[which(dsof$nirrcapcl == 1 | dsof$nirrcapcl >= 5),]$unit_calc <- "NA"
dsof[which(dsof$irrcapcl == 1 | dsof$irrcapcl == 8),]$subclass_calc <- "NA"
dsof[which(dsof$irrcapcl == 1 | dsof$irrcapcl >= 5),]$unit_calc <- "NA"

write.csv(dsof[order(dsof$musym, dsof$comppct_r, dsof$compname),], file="lcc_subclass_check.csv")
