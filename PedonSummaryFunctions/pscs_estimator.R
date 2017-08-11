source("pedon_summary_functions.R")
library(soilDB)
get_extended_data_from_NASIS_db
pedons = fetchNASIS()

#c1=profileApply(pedons, getPlowLayerDepth)
#c2=profileApply(pedons, estimateRootingDepth)

pscsraw = profileApply(pedons, estimatePSCS) #get pscs bounds

#make output
lo = as.logical(1:length(pscsraw) %% 2)
pscs_top = pscsraw[lo]
pscs_bot = pscsraw[!lo]
write.csv(transform(data.frame(pedons$site_id,pedons$taxonname,pedons$psctopdepth,pedons$pscbotdepth,esttop=pscs_top,estbot=pscs_bot, tmatch=(pedons$psctopdepth == pscs_top), bmatch=(pedons$pscbotdepth == pscs_bot)),match=(tmatch==bmatch & tmatch == T)),"CA630_pscs_check.csv")
