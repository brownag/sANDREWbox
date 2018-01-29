library(soilDB)
source('PedonSummaryFunctions/pedon_summary_functions.R')

component.name <- 'Auberry'
labdata <- fetchKSSL(series = component.name)

n <- fetchNASIS(rmHzErrors = F, nullFragsAreZero = T)
f <- fetchKSSL(component.name)

site(f) <- merge(site(f), site(n), by="pedon_id", all.x = T)
horizons(f) <- merge(horizons(f), horizons(n)[, c("labsampnum","peiid","phiid")], by="labsampnum", all.x=T)

#alias fetchKSSL type SPC to fetchNASIS type SPC
horizons(f)$hzname <- horizons(f)$hzn_desgn
horizons(f)$hzdept <- horizons(f)$hzn_top
horizons(f)$hzdepb <- horizons(f)$hzn_bot

current.ranges <- list('^A.*' = list(lo=1.5, rv=3, hi=5.5), '^B.*' = list(lo=1.5, rv=3, hi=5.5), '^C.*' = list(lo=0.0, rv=0.25, hi=0.5))

data.frame(f[1,]$hzn_desgn, f[1,]$c_tot*1.72)

arg.bounds <- getArgillicBounds(f[1,])
above_argillic <- intersectHorizon(f[1, ], 0, arg.bounds$ubound)
upper_argillic <- intersectHorizon(f[1, ], arg.bounds$ubound, (arg.bounds$lbound - arg.bounds$ubound) / 2)
lower_argillic <- intersectHorizon(f[1, ], (arg.bounds$lbound - arg.bounds$ubound) / 2, arg.bounds$lbound)

