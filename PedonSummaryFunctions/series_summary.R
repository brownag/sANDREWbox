library(soilDB)
source('PedonSummaryFunctions/pedon_summary_functions.R')

component.names <- c('Flanly','Auberry','Sierra','Blasingame','Orose')
labdata <- fetchKSSL(series=component.names[1])
for(c in component.names[2:length(component.names)])
  labdata <- rbind(labdata, fetchKSSL(series = c))

n <- fetchNASIS(rmHzErrors = F, nullFragsAreZero = T)
f <- labdata

site(f) <- merge(site(f), site(n), by="pedon_id", all.x = T)
horizons(f) <- merge(horizons(f), horizons(n)[, c("labsampnum","peiid","phiid")], by="labsampnum", all.x=T)

#alias fetchKSSL type SPC to fetchNASIS type SPC
horizons(f)$hzname <- horizons(f)$hzn_desgn
horizons(f)$hzdept <- horizons(f)$hzn_top
horizons(f)$hzdepb <- horizons(f)$hzn_bot

horizons(f)$c_tot[is.na(horizons(f)$c_tot)] <- horizons(f)$oc[is.na(horizons(f)$c_tot)]
horizons(f)$om_tot <- horizons(f)$c_tot*1.78

current.ranges <- list('^A.*' = list(lo=1.5, rv=3, hi=5.5), '^B.*' = list(lo=0.5, rv=1, hi=1.5), '^C.*' = list(lo=0.0, rv=0.25, hi=0.5))

hz.above.argillic <- data.frame()
hz.upper.argillic <- data.frame()
hz.lower.argillic <- data.frame()

for(labpedon in 1:length(site(f))) { 
  arg.bounds <- getArgillicBounds(f[labpedon,])
  above_argillic <- intersectLabHorizon(f[labpedon, ], 0, arg.bounds$ubound-1)
  upper_argillic <- intersectLabHorizon(f[labpedon, ], arg.bounds$ubound+1, (arg.bounds$lbound - arg.bounds$ubound) / 2)
  lower_argillic <- intersectLabHorizon(f[labpedon, ], ((arg.bounds$lbound - arg.bounds$ubound) / 2)+1, arg.bounds$lbound-1)
  
  hz.above.argillic <- rbind(hz.above.argillic,horizons(f[labpedon,])[horizons(f[labpedon,])$labsampnum %in% above_argillic, ])
  hz.upper.argillic <- rbind(hz.upper.argillic,horizons(f[labpedon,])[horizons(f[labpedon,])$labsampnum %in% upper_argillic, ])
  hz.lower.argillic <- rbind(hz.lower.argillic,horizons(f[labpedon,])[horizons(f[labpedon,])$labsampnum %in% lower_argillic, ])
}

om.above.argillic <- na.omit(hz.above.argillic$om_tot)
om.upper.argillic <- na.omit(hz.upper.argillic$om_tot)
om.lower.argillic <- na.omit(hz.lower.argillic$om_tot)
plot(density(om.above.argillic), xlim=c(0,5), ylim=c(0,1))
lines(density(om.upper.argillic), xlim=c(0,5), ylim=c(0,1), col="blue")
lines(density(om.lower.argillic), xlim=c(0,5), ylim=c(0,1), col="red")

summary.om <- rbind('above'=quantile(om.above.argillic, probs=c(0,0.01,0.05,0.1,0.25, 0.5, 0.75, 0.9, 0.95 ,0.99,1)),
      'upper argillic'=quantile(om.upper.argillic, probs=c(0,0.01,0.05,0.1,0.25, 0.5, 0.75, 0.9, 0.95 ,0.99,1)),
      'lower argillic'=quantile(om.lower.argillic, probs=c(0,0.01,0.05,0.1,0.25, 0.5, 0.75, 0.9, 0.95 ,0.99,1)))

summary.om[,c("5%","50%","95%")]
