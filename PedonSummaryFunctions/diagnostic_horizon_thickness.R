source("PedonSummaryFunctions\\pedon_summary_functions.R")

f <- fetchNASIS()
foo <- diagnostic_hz(f)
#calc thickness
foo$thickness <- foo$featdepb-foo$featdept
#filter to desired diagnostic
foo2 <- foo[grepl(foo$diag_kind,pat="(mollic|umbric) epipedon"),]
foo3 <- data.frame(peiid=foo2$peiid,mollic_thickness=foo2$thickness)
site(f) <- merge(site(f), foo3, by="peiid")

has_umbric_data <- do.call(rbind,profileApply(f,FUN=hasDarkMineralSurface, bounds=T, remove.na=T, simplify=F))

has_umbric_data$peiid <- rownames(has_umbric_data)
foo4 <- merge(foo2, has_umbric_data, by="peiid")
foo4$tmatch <- foo4$featdept == foo4$dsubound
foo4$bmatch <- foo4$featdepb == foo4$dslbound
foo4$match <-  foo4$tmatch & foo4$bmatch

write.csv(foo4,file="PedonSummaryFunctions\\dark_epipedon_check.csv")

p<-f[which(site(f)$peiid==1193379),]
