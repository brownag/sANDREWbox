library(aqp)
library(soilDB)
library(rgdal)
f <- fetchNASIS()
n <- site(f)#get_site_data_from_NASIS_db()
setwd('S:/NRCS/Archive_Andrew_Brown/CA649/Points')
dp <- readOGR(dsn='.', layer="ca649_dp")
ident.var <- 'ident'
#get the low hanging fruit. direct matches.
dp.new <- merge(dp, n, by.y='pedon_id', by.x=ident.var, all.x=T)

#try removing Ns
dp$IDENT_noN <- ""
dp$IDENT_noN <- gsub(dp[[ident.var]], pattern = "N", replacement = "")
if(length(is.na(dp.new$taxonname))) {
  note_taxa <- merge(dp[is.na(dp.new$taxonname), ], n, by.y='pedon_id', by.x='IDENT_noN', all.x=T)$taxonname
  dp.new$taxonname[is.na(dp.new$taxonname)] <- note_taxa
}
nrow(dp.new)

writeOGR(dp.new[, c("ident", "type", "Latitude", "Longitude", "y_proj", "x_proj", "comment", 
                    "altitude", "filename", "site_id", "taxonname", "taxonkind", "taxpartsize","ecositeid", "pedonpurpose", "pedontype")], 
         dsn=".",
         layer="updated_ca649_dp",
         driver="ESRI Shapefile")
