#pedon texturizer

source("Texturizer\\fine_earth_fractions.R")
library(soilDB)
pedons <- fetchNASIS()
profileApply(pedons,FUN=checkProfileFineEarthLimits)
