#pedon texturizer

source("PedonSummaryFunctions\\pedon_summary_functions.R") 
source("PedonSummaryFunctions\\fine_earth_fractions.R")
library(soilDB)
pedons <- fetchNASIS()
f <- profileApply(pedons, FUN=checkProfileFineEarthLimits)
