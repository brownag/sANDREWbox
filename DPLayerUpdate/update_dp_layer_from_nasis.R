library(soilDB)
library(rgdal)
p <- fetchNASIS_pedons()
n <- site(p)
dp <- readOGR(dsn='L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/final/FG_CA630_GIS_2018.gdb', layer="ca630_dp")

#get the low hanging fruit. direct matches.
dp.new <- merge(dp, n, by.y='pedon_id', by.x='IDENT', all.x=T)

#try removing Ns
dp$IDENT_noN <- ""
dp$IDENT_noN <- gsub(dp$IDENT, pattern = "N", replacement = "")
note_taxa <- merge(dp[is.na(dp.new$taxonname), ], n, by.y='pedon_id', by.x='IDENT_noN', all.x=T)$taxonname
dp.new$taxonname[is.na(dp.new$taxonname)] <- note_taxa

length(dp.new[which(is.na(dp.new$taxonname)),]$IDENT)

writeOGR(dp.new[, c("IDENT","TYPE","LAT","LONG_","Y_PROJ","X_PROJ","COMMENT", "ALTITUDE", "FILENAME", "site_id", "taxonname", "taxonkind", "taxpartsize","ecositeid", "pedonpurpose", "pedontype")], dsn=".",layer="updated_CA630_dp",driver="ESRI Shapefile")
nrow(dp.new)
