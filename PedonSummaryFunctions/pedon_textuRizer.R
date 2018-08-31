#pedon texturizer

source("PedonSummaryFunctions\\pedon_summary_functions.R") 
source("PedonSummaryFunctions\\fine_earth_fractions.R")

library(soilDB)
library(rgdal)
pedons <- fetchNASIS()

ca630_b <- readOGR(dsn="L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/FG_CA630_GIS_2018_0818_TKK.gdb",layer="ca630_b")

coordinates(pedons) <- ~ x_std + y_std
proj4string(pedons) <- '+proj=longlat +datum=WGS84'
pedons@sp <- spTransform(pedons@sp, CRS(proj4string(ca630_b)))

p.sub <- pedons[which(pedons$taxonname == 'Arpatutu' | pedons$taxonname == "Typic Haploxerults"),]
p.sub <- p.sub[-which(grepl(p.sub$pedon_id,pattern = '2017')),]

#f <- profileApply(p.sub, FUN=checkProfileFineEarthLimits, skipNA=FALSE)

p.sub$pscs_clay <- profileApply(p.sub, FUN=getPSCSclay)
p.sub$pscs_frag <-profileApply(p.sub, FUN=getPSCSfrags)

nonskeletal <- p.sub[which(p.sub$pscs_frag < 35), ]


aggregate(p.sub$pscs_clay, by=list(p.sub$taxonname), FUN=quantile, na.rm=T)

heavy.arpatutus.idx <- which(p.sub$pscs_clay > 18 & p.sub$taxonname == 'Arpatutu')

#combine "heavy arpatutus" and typic haploxerults
ultz <- p.sub[c(which(p.sub$taxonname == "Typic Haploxerults"), heavy.arpatutus.idx), ]

#look at just the wimpy arpatutus
alfz <- p.sub[-heavy.arpatutus.idx, ]
alfz <- alfz[which(alfz$taxonname == "Arpatutu"),]

plot(ultz)

plot(alfz)

aggregate(ultz$pscs_clay, by=list(ultz$taxonname), FUN=quantile, na.rm=T)
aggregate(alfz$pscs_clay, by=list(alfz$taxonname), FUN=quantile, na.rm=T)

plot(ca630_b)
points(alfz@sp, pch=3, col="red")
points(ultz@sp, pch=3, col="blue")

nrow(site(alfz))

nrow(site(ultz))

plot(density(alfz$phfield, na.rm=T))
plot(density(ultz$phfield, na.rm=T))

plot(density(alfz$clay, na.rm=T))
plot(density(ultz$clay, na.rm=T))

plot(density(p.sub$clay,na.rm=T))

quantile(p.sub$clay,na.rm=T)
quantile(alfz$clay,na.rm=T)
quantile(ultz$clay,na.rm=T)

