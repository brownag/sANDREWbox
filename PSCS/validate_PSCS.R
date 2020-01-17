# NASIS Pedon Particle Size Control Section (PSCS) Validation script
# @author: andrew brown
# @last revised: 2020/01/15; using aqp 1.18.4

# load aqp library - SoilProfileCollections, PSCS estimator and profile plots
library(aqp)

# load soilDB library - NASIS database access
library(soilDB)

message("Validating particle size control section (PSCS) for NASIS selected set...")

# load site/pedons from NASIS selected set 
f <- fetchNASIS()

message(paste0("Loaded ",length(f), " pedons."))

# define function for merging estimatePSCS result from profileApply
frameify <- function(l, colnames=NULL) {
  res <- as.data.frame(cbind(names(l), do.call('rbind', l)), row.names = FALSE)
  colnames(res) <- colnames
  return(res)
}

# calculate a list with top and bottom depth of PSCS for each profile in SPC
l.calc.pscs <- profileApply(f, estimatePSCS, bottom.pattern = "Cr|R|Cd|qm", simplify = FALSE)
message(paste0("PSCS boundaries calculated."))

# merge data.frame with calculated PSCS bounds into site table
site(f) <- frameify(l.calc.pscs, colnames = c(idname(f), "calc_pscstop", "calc_pscsbot"))

# create subset of mismatching (calc versus stored) top depths
res <- subsetProfiles(f, s = 'calc_pscstop != psctopdepth | calc_pscsbot != pscbotdepth')

# print/plot result
if(length(res)) { 
  
  # make a plot of non-matching pedons. add annotations to show stored v.s. calculated
  par(mar=c(1,1,1,1))
  plotSPC(res, label="pedon_id")
  
  calc.df <- site(res)[,c(idname(res), "calc_pscstop", "calc_pscsbot")]
  names(calc.df) <- c()
  
  addBracket(x <- site(res)[,c(idname(res), "calc_pscstop", "calc_pscsbot")],
                       tick.length = 0, lwd=7, offset=-0.075,
                       col=rgb(red=0, green=0, blue=1, alpha=0.50))
  addBracket(x <- site(res)[,c(idname(res), "psctopdepth", "pscbotdepth")], col="YELLOW", 
                       tick.length = 0, lwd=1, offset=-0.075)
  
  message(paste0(length(res)," of ",length(f)," PSCS top depths do not match!"))
  # print out stored versus calculated for non-matching pedons
  profileApply(res, function(p) {
    message(paste0(p$pedon_id, " Stored: ",p$psctopdepth,"-",p$pscbotdepth,"cm; Calculated: ",p$calc_pscstop,"-",p$calc_pscsbot,"cm; (",idname(p), ":", profile_id(p),")"))
  })
} else {
  message("All PSCS depths match!")
}

