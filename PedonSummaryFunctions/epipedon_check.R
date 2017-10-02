source("PedonSummaryFunctions\\pedon_summary_functions.R")

#get the data
library(soilDB)
f <- fetchNASIS()
foo <- diagnostic_hz(f)

#calc thickness
foo$thickness <- foo$featdepb-foo$featdept

#filter to the dark (or nondark) epipedons for Pedon Diagnostic Horizon table
foo2 <- foo[grepl(foo$diag_kind,pat="(ochric|mollic|umbric) epipedon"),]
foo3 <- data.frame(peiid=foo2$peiid, epipedon_thickness=foo2$thickness)
site(f) <- merge(site(f), foo3, by="peiid")

#use hasDarkMineralSurface() to check whether the diagnostics table match the data in the horizon table
has_epipedon <- do.call(rbind,profileApply(f,FUN=hasDarkMineralSurface, bounds=T, remove.na=T, simplify=F))

#merge and check for matches of top and bottom depth
has_epipedon$peiid <- rownames(has_epipedon)
foo4 <- merge(foo2, has_epipedon, by="peiid")
foo4$pedon_id <- merge(foo2, site(f), by="peiid")$pedon_id
foo4$tmatch <- foo4$featdept == foo4$dsubound
foo4$bmatch <- foo4$featdepb == foo4$dslbound
foo4$match <-  foo4$tmatch & foo4$bmatch

#write output
write.csv(foo4,file="PedonSummaryFunctions\\dark_epipedon_check.csv")
