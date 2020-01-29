library(aqp)
library(soilDB)
library(rgdal)

f <- fetchNASIS()
n <- site(f)

# merging can and will break down without this option
options(stringsAsFactors = FALSE)

setwd('S:/NRCS/Archive_Andrew_Brown/CA649/Points')
dp <- readOGR(dsn='.', layer="ca649_dp", stringsAsFactors = FALSE)
ident.var <- 'ident'

# direct matches, with pedon associated with site record
dp.new <- merge(dp[,c(ident.var, 'altitude')], n, by.y='pedon_id', by.x=ident.var, sort=F)

siterecs <- get_site_data_from_NASIS_db()[,c('siteiid','site_id')]
eco.history <- get_extended_data_from_NASIS_db()$ecositehistory
ecosite <- do.call('rbind', lapply(split(eco.history, f=eco.history$siteiid), soilDB:::.pickBestEcosite))

ecosite.lut <- as.character(ecosite$ecositeid)
names(ecosite.lut) <- merge(ecosite, siterecs, sort=F, all.x=T)$site_id

no.ecosite.idx <- which(is.na(dp.new$ecositeid))
dp.new[no.ecosite.idx, 'ecositeid']<- as.character(ecosite.lut[dp.new[no.ecosite.idx,]$ident])

writeOGR(dp.new, 
         dsn=".",
         layer="updated_ca649_dp",
         driver="ESRI Shapefile",
         overwrite_layer = TRUE)
