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
source('S:/NRCS/Archive_Andrew_Brown/Scripts/PedonSummaryFunctions/pedon_summary_functions.R')
library(soilDB)
c <- fetchNASIS_components()

#hacky aliasing
c$hzdept <- c$hzdept_r
c$hzdepb <- c$hzdepb_r
c$clay <- c$claytotal_r

estimateMostLimitingKsat <-  function(x) {
  chz.idx <- which(x$ksat_l == min(x$ksat_l))
  if(length(chz.idx))
     return(x$ksat_l[chz.idx])
  else return(NA)
}

hasTexturalB <- function(x) {
  x.arg <- getArgillicBounds(x)
  if(all(is.finite(as.numeric(x.arg))))
    return(TRUE)
  return(FALSE)
}
 
c$restr_depth <- profileApply(c, estimateSoilDepth, top="hzdept_r", bottom="hzdepb_r")
c$restr_ksat <- profileApply(c,  estimateMostLimitingKsat)
c$has_textural_B <- profileApply(c, hasTexturalB)
c$pscs_clay <- profileApply(c, getPSCSclay, dtype="component")
cc <- c[1,]

if(cc$restr_depth > 50) {
  if(cc$restr_ksat > 1) { # moderately high Ksat ~= moderately slow permeability (Field Book pp 2-84)
    #1.1a: surface texture "fine or very fine"
    #1.1b: surface texture "moderately fine"
    #1.1c: surface texture "medium"
    #1.1d: surface texture "moderately coarse"
    #1.1e: surface texture "coarse or very coarse"
    #1.1e.i: has textural B horizon
    #1.1e.ii: lacks textural B horizon
  } else { #very slow or slow perm
    if(cc$drainagecl == "well" | cc$drainagecl == "moderately well") {
      
    } else if (cc$drainagecl == "somewhat poorly") {
      if(cc$taxorder != "histosols") {
        
      }
    }
  }
}

if(cc$drainagecl == "poorly" | cc$drainagecl == "very poorly" | (cc$drainagecl == "somewhat poorly" & ((cc$taxorder == "histosols" & cc$restr_depth > 50) | (!cc$has_textural_B & cc$pscs_clay <= 18)))) {
#get PD and VPD, plus SPD from deep histosols and coarse textured soils lacking a textural B
}

#II: wet soils (poorly and very poorly drained; also SPD if "coarse textured" or "deep organic" i.e. II.1.2 & II.1.3)
#II.1.1: surface texture "moderately coarse to fine"
#II.1.2: coarse textured, lacks textural B
#II.1.3: depth class (moderately deep or deeper)
#II.1.3: organic soils 

#III: depth class (shallow or very shallow)
#III.1 Excessively to moderately well drained
#III.1.1: very shallow
#III.1.2: shallowx
#III.2 Somewhat poorly drained (gets 's' subclass)

#IV: saline/sodic ("moderate to severe salinity and sodicity")

#V: >35% rock fragments in surface layers (very or extremely modifier)

#VI: soils subject to damaging overflow (landform contains "flood plain"?)
