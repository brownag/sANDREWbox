library(soilDB)
library(sharpshootR)
library(aqp)

f <- fetchNASIS_pedons()
f.sub <- f[which(f$pedonpurpose == 'laboratory sampling site'), ]

k <- fetchKSSL(pedlabsampnum = f.sub$pedlabsampnum[1])
for(i in f.sub$pedlabsampnum[2:nrow(site(f.sub))]) 
  k <- rbind(k, fetchKSSL(pedlabsampnum = i))

genhzpatterns <- list('A'="A", 'Bw'="Bt[23]", '2Bt'="3Bt", '2BCt'="BC")

idx=4

f.sub <- f[grepl(f$hzname, pattern=genhzpatterns[idx]), ]

k.sub <- horizons(k)[grepl(k$hzn_desgn, pattern=genhzpatterns[idx]),]

#al + 0.5fe
quantile(k.sub$al_ox + 0.5*k.sub$fe_ox, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)

#db0.33
quantile(k.sub$db_13b, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)

#w15l2
quantile(k.sub$w15l2, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)

#pNZ
quantile(k.sub$p_nz, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)

#om
quantile(k.sub$estimated_om, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)

 #bs7
quantile(k.sub$bs7, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)

#bs8.2
quantile(k.sub$bs82, p=c(0,0.05,0.25,0.5,0.75,0.95,1), na.rm=T)
