source("pedon_summary_functions.R")

#get the data
library(soilDB)
pedons = fetchNASIS()

#checks
#c1=profileApply(pedons, getPlowLayerDepth)
#c2=profileApply(pedons, estimateRootingDepth)

##get pscs bounds
pscsraw = profileApply(pedons, estimatePSCS) 

#make output
lo = as.logical(1:length(pscsraw) %% 2)
pscs_top = pscsraw[lo]
pscs_bot = pscsraw[!lo]
write.csv(transform(data.frame(pedons$site_id,pedons$taxonname,pedons$psctopdepth,pedons$pscbotdepth,esttop=pscs_top,estbot=pscs_bot, tmatch=(pedons$psctopdepth == pscs_top), bmatch=(pedons$pscbotdepth == pscs_bot)),match=(tmatch==bmatch & tmatch == T)), "CA630_pscs_check.csv")