# NASIS Pedon Particle Size Control Section (PSCS) Validation script
# @author: andrew brown
# @last revised: 2020/01/14; using aqp 1.18.4

# load aqp library - SoilProfileCollections, PSCS estimator and profile plots
library(aqp)

# load soilDB library - NASIS database access
library(soilDB)

# load site/pedons from NASIS selected set 
f <- fetchNASIS()

# define function for merging estimatePSCS result from profileApply
frameify <- function(l, colnames=NULL) {
  res <- as.data.frame(cbind(names(l), do.call('rbind', l)), row.names = FALSE)
  colnames(res) <- colnames
  return(res)
}

# calculate a list with top and bottom depth of PSCS for each profile in SPC
l.calc.pscs <- profileApply(f, estimatePSCS, bottom.pattern = "Cr|R|Cd|qm", simplify = FALSE)

# merge data.frame with calculated PSCS bounds into site table
site(f) <- frameify(l.calc.pscs, colnames = c(idname(f), "calc_pscstop", "calc_pscsbot"))

# create subset of mismatching (calc versus stored) top depths
res_top <- subsetProfiles(f, s = 'calc_pscstop != psctopdepth')

# print/plot result
if(length(res)) { 
  par(mar=c(1,1,1,1))
  plotSPC(res, label="pedon_id")
  message(paste0(length(res)," of ",length(f)," PSCS top depths do not match!"))
} else {
  message("All PSCS top depths match!")
}

# create subset of mismatching (calc versus stored) bottom depths
res_bot <- subsetProfiles(f, s = 'calc_pscsbot != pscbotdepth')

# print/plot result
if(length(res_bot)) {
  par(mar=c(1,1,1,1))
  plotSPC(res, label="pedon_id")
  message(paste0(length(res)," of ",length(f)," PSCS bottom depths do not match!"))
} else {
  message("All PSCS bottom depths match!")
} 

