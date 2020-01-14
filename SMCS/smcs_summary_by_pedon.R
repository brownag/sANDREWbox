library(soilDB)
jnsm_data <- read.csv('S:/NRCS/430 SOI Soil Survey/430-05 Soil Survey Area Case Files/CA630/jNSM/CA630_all_jNSM.csv')
pedons <- fetchNASIS_pedons()

pjnsm <- merge(site(pedons), jnsm_data, by.x='site_id', by.y='stnName', all.x=T)

c <- fetchNASIS_components()
taxa <- sort(unique(c$compname[c$compkind == 'series']))
buf <-  cbind(data.frame(taxon=character(0)), n=numeric(0), t(quantile(c(0), p=c(0,0.05,0.25,0.5,0.75,0.95,1)))[0,])
for(t in taxa) {
  subse <- pjnsm[which(pjnsm$taxonname == t),]
  dry_cumulative <- subse$yrdryCum#[which(subse$yrdryCum > 0 & !is.na(subse$yrdryCum))]
  mdry_cumulative <- subse$yrmdCum#[which(subse$yrmdCum > 0 & !is.na(subse$yrmdCum))]
  dry_days_cumulative <- quantile(dry_cumulative+mdry_cumulative, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)
  buf <- rbind(buf,cbind(data.frame(taxon=t), n=length(na.omit(dry_cumulative+mdry_cumulative)), t(dry_days_cumulative)))
  # moist_cumulative <- subse$yrmstCum[which(subse$yrmstCum > 0 & !is.na(subse$yrmstCum))]
  # quantile(365-moist_cumulative, p=c(0,0.05,0.25,0.5,0.75,0.95,1))
}
write.csv(buf,file="smcs_dry_ca630.csv")
