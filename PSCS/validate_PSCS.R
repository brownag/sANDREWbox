# NASIS Pedon Particle Size Control Section (PSCS) Validation script
# @author: andrew brown
# @last revised: 2020/02/24; using aqp 1.19.1

# load aqp library - SoilProfileCollections, PSCS estimator and profile plots
library(aqp)

# load soilDB library - NASIS database access
library(soilDB)

message("Validating particle size control section (PSCS) for NASIS selected set...")

psc.key.partC <- function(pscs_clay, pscs_frag, pscs_sand, all_sand, bedrckdepth) {
  sandy <- all_sand
  skeletal <- pscs_frag >= 35
  colo <- pscs_clay < 18
  filo <- pscs_clay >= 18 & pscs_clay < 35
  clayey <- pscs_clay >= 35
  
  res <- rep(NA, length(pscs_clay))
  
  res[sandy] <- "sandy"
  res[sandy & skeletal] <- "sandy-skeletal"
  
  res[!sandy & colo] <- "coarse-loamy"
  res[filo] <- "fine-loamy"
  res[filo] <- "fine-loamy"
  
  # this is somewhat more restrictive than the key, but captures the intent
  res[!sandy & colo & (pscs_frag + pscs_sand) < 15] <- "coarse-silty"
  res[filo & (pscs_frag + pscs_sand) < 15] <- "fine-silty"
  
  res[!sandy & (colo | filo) & bedrckdepth <= 50] <- "loamy"
  res[!sandy & (colo | filo) & pscs_frag >= 35] <- "loamy-skeletal"
  
  res[clayey] <- "fine"
  res[clayey & bedrckdepth <= 50] <- "clayey"
  res[clayey & pscs_frag >= 35] <- "clayey-skeletal"
  
  return(res)
}

# load site/pedons from NASIS selected set 
f <- fetchNASIS()
f$bedrckdepth <- profileApply(f, estimateSoilDepth, p = "Cr|R|qm|Cd")

message(paste0("Loaded ", length(f), " pedons."))

# calculate a list with top and bottom depth of PSCS for each profile in SPC
l.calc.pscs <- profileApply(f, function(p) {
  df <- cbind(profile_id(p), 
              as.data.frame(t(as.numeric(estimatePSCS(p, bottom.pattern = "Cr|R|Cd|qm")))))
  names(df) <- c(idname(f), "calc_pscstop", "calc_pscsbot")
  return(df)
}, frameify = TRUE)
 
message(paste0("PSCS boundaries calculated."))

# merge data.frame with calculated PSCS bounds into site table
site(f) <- l.calc.pscs

# create subset of mismatching (calc versus stored) top or bottom depths
res <- filter(f, calc_pscstop != psctopdepth | calc_pscsbot != pscbotdepth)

# print/plot result
if (length(res)) { 
  
  # make a plot of non-matching pedons. add annotations to show stored v.s. calculated
  par(mar = c(1,1,1,1))
  plotSPC(res, label = "pedon_id")
  
  calc.df <- site(res)[,c(idname(res), "calc_pscstop", "calc_pscsbot")]
  names(calc.df) <- c(idname(res),"top", "bottom")
  pop.df <- site(res)[,c(idname(res), "psctopdepth", "pscbotdepth")]
  names(pop.df) <- c(idname(res),"top", "bottom")
  
  addBracket(x = calc.df, col = "BLUE", tick.length = 0, lwd = 1, offset = -0.07)
  addBracket(x = pop.df, col = "YELLOW", tick.length = 0, lwd = 1, offset = -0.075)
  
  message(paste0(length(res)," of ",length(f)," PSCS boundaries do not match!"))
  
  # print out stored versus calculated for non-matching pedons
  prnt <- profileApply(res, function(p) {
    message(paste0("\t", p$pedon_id, 
                   "\tStored: ",p$psctopdepth," - ",p$pscbotdepth,
                   "cm  \t=> Calculated: ",p$calc_pscstop," - ",p$calc_pscsbot,
                   "cm  \t (",idname(p), ":", profile_id(p),")"))
  })
  
} else {
  message("All PSCS depths match!")
}

message("Comparing stored v.s. calculated particle size class...")

f.new <- mutate_profile(glomApply(f, function(p) return(c(p$psctopdepth, p$pscbotdepth))), 
                             pscs_clay = weighted.mean(clay, hzdepb - hzdept, na.rm = TRUE),
                             pscs_sand = weighted.mean(sand, hzdepb - hzdept, na.rm = TRUE),
                             pscs_frag = weighted.mean(total_frags_pct_nopf, hzdepb - hzdept, na.rm = TRUE),
                             all_sand = all(grepl("S$", texcl) & !grepl("LFS|LVFS", texcl)))
  
res2 <- data.frame(pedon_id = f.new$pedon_id, 
                     pscs_clay = f.new$pscs_clay, 
                     pscs_frag = f.new$pscs_frag, 
                     pscs_sand = f.new$pscs_sand,
                     pscstop = f.new$psctopdepth,
                     pscsbot = f.new$pscbotdepth,
                     sandy_textures = f.new$all_sand, 
                     bedrockdepth = f.new$bedrckdepth,
                     calculated = psc.key.partC(f.new$pscs_clay, f.new$pscs_frag, 
                                    f.new$pscs_sand, f.new$all_sand, 
                                    f.new$bedrckdepth), 
                     stored = f.new$taxpartsize)
  
foo <- is.na(res2$calculated) | res2$calculated != res2$stored
  
if (any(foo)) {
  res2[foo,]
} else {
  message("All PS classes match!")
}


