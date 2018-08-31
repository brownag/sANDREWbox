#humic subgroups
#
library(soilDB)
library(aqp)
library(rgdal)
library(raster)

source('PedonSummaryFunctions/pedon_summary_functions.R')

# f <- fetchKSSL(mlra = '18')
# coordinates(f) <- ~ x + y
# proj4string(f) <- "+proj=longlat +datum=WGS84"
# 
# quantile(horizons(f)[grepl(f$hzn_desgn, pattern='A'),]$db_13b, na.rm=T)
# quantile(horizons(f)[grepl(f$hzn_desgn, pattern='B'),]$db_13b, na.rm=T)
# quantile(horizons(f)[grepl(f$hzn_desgn, pattern='C'),]$db_13b, na.rm=T)
# 
# quantile(horizons(f)[grepl(f$hzn_desgn, pattern='A'),]$estimated_oc, na.rm=T)
# quantile(horizons(f)[grepl(f$hzn_desgn, pattern='B'),]$estimated_oc, na.rm=T)
# quantile(horizons(f)[grepl(f$hzn_desgn, pattern='C'),]$estimated_oc, na.rm=T)

n <- fetchNASIS_pedons()
n$is_humic_subgroup <- as.logical(profileApply(n, hasDarkMineralSurface, require_chroma=F))

df <- data.frame(taxonname=n$taxonname, site_id=n$site_id, taxonkind=n$taxonkind, is_humic_subgroup=n$is_humic_subgroup, taxpscs=n$taxpartsize, taxsubgr=n$taxsubgrp)[order(n$taxonname, n$taxonkind, n$site_id),]
write.csv(df,file = "metased_StoMD_humic.csv")

coordinates(n) <- ~ x_std + y_std
proj4string(n) <- "+proj=longlat +datum=WGS84"

ca630_b <- readOGR(dsn="L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/final/FG_CA630_GIS_2018.gdb", layer="ca630_b")
ca630_b_t <- spTransform(ca630_b, CRS(proj4string(n)))

nice.colors <- rep("BLUE", nrow(site(n)))
nice.colors[n$is_humic_subgroup] <- "GREEN"
plot(n@sp, pch=19, col=nice.colors, main="CA630 - Humic Colors (S to MD, L-SK, MSD)", sub="Includes soils correlated to Copperopolis, Priestgrade, Solambo and Moccasinhill.")
plot(ca630_b_t, add=T)
legend(x="bottomleft",legend=c(paste0("Humic Colors (",sum(n$is_humic_subgroup),")"), paste0("Lacks Humic Colors (",length(n$is_humic_subgroup),")")), pch=19, col=c("GREEN","BLUE"))
