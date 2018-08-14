## legend change updater
## @author: andrew brown
## v0.1 06/22/2018

###
### LIBRARIES
###
library(rgdal)
library(raster)
###

###
### SETTINGS
###

#CA731
# target_layer <- 'ca731_a'
# original_gdb <- 'L:/NRCS/MLRAShared/CA731/Archived_CA731/CA731_Join_FY2018_0201_ra.gdb'
# new_gdb <- 'L:/NRCS/MLRAShared/CA731/CA731_Join_FY2018_0201_ra.gdb'
# output_shp <- 'CA630Legend/final_mlramu_extents/CA731_630_mapunits_update'
# ignore_symbols <- c()
 
#CA077
target_layer <- 'ca077_a'
original_gdb <- 'L:/NRCS/MLRAShared/CA077/CA077_Join_FY2018_ra.gdb'
new_gdb <- 'E:/workspace'#'L:/NRCS/MLRAShared/CA077/CA077_Join_FY2018_0809_TKK_JW.gdb'
output_shp <- 'CA630Legend/final_mlramu_extents/CA077_630_mapunits_update'
ignore_symbols <- c("W", "DAM")

# #CA632
# target_layer <- 'ca632_a'
# original_gdb <- 'L:/NRCS/MLRAShared/CA632/CA632_Join_FY2018_ra.gdb'
# new_gdb <- 'L:/NRCS/MLRAShared/CA632/CA632_Join_FY2018_0809_TKK_JW.gdb'
# output_shp <- 'CA630Legend/final_mlramu_extents/CA632_630_mapunits_update'
# ignore_symbols <- c("W", "DAM")

#CA644
# target_layer <- 'ca644_a'
# original_gdb <- 'L:/NRCS/MLRAShared/CA644/CA644_Join_FY2018_ra.gdb'
# new_gdb <- 'L:/NRCS/MLRAShared/CA644/CA644_Join_FY2018_0809_TKK.gdb'
# output_shp <- 'CA630Legend/final_mlramu_extents/CA644_630_mapunits_update'
# ignore_symbols <- c("301", "451", "W", "DAM") #no double dipping. these acreages already counted under a MLRA project for MUs brought into CA630
# 
#CA649
# target_layer <- 'ca649_a'
# original_gdb <- 'L:/NRCS/MLRAShared/CA649/original/CA649_Join_FY2018_ra.gdb'
# new_gdb <- 'L:/NRCS/MLRAShared/CA649/CA649_Join_FY2018_ra.gdb'
# output_shp <- 'CA630Legend/final_mlramu_extents/CA649_630_mapunits_update'
# ignore_symbols <- c("W") #no double dipping. these acreages already counted under a MLRA project for MUs brought into CA630

#CA648
# target_layer <- 'ca648_a'
# original_gdb <- 'L:/NRCS/MLRAShared/CA648/original/CA648_Join_FY2018_ra.gdb'
# new_gdb <- 'L:/NRCS/MLRAShared/CA648/CA648_Join_FY2018_0730_TKK.gdb'
# output_shp <- 'CA630Legend/final_mlramu_extents/CA648_630_mapunits_update'
# ignore_symbols <- c("W") #no double dipping. these acreages already counted under a MLRA project for MUs brought into CA630

lcb_dsn <- 'L:/NRCS/MLRAShared/Geodata/Land Category Breakdown'
lcb_layer <- 'SSR2_Land_Categories_ver2018_0523'
###

###
### LOAD DATA
###
# load original (i.e. current SSURGO) geodatabase
origdb <- readOGR(dsn=original_gdb, layer=target_layer)

# load new (i.e. edited) geodatabase
newdb <- readOGR(dsn=new_gdb, layer=target_layer)

# load land category breakdown layer
lcb <- readOGR(dsn=lcb_dsn, layer=lcb_layer)
###

###
### NEW MUS - find NEW symbols/mapunits being ADDED to legend
###
to_add <- sort(unique(as.character(newdb$MUSYM[!(newdb$MUSYM %in% origdb$MUSYM)]))) #what symbols were not in the original set?

###
### ADDITIONAL MUS - find symbols/mapunits being BEING MADE ADDITIONAL (i.e. have been completely removed/replaced on the spatial)
###
to_remove <- sort(unique(as.character(origdb$MUSYM[!(origdb$MUSYM %in% newdb$MUSYM)]))) #what original symbols are missing from new set?

###
### CALCULATE ACRES - for each of the new musyms and total
###
update_polys <- newdb[newdb$MUSYM %in% to_add,]
update_polys_ignore <- update_polys[0,]
ignored_acres <- 0
if(length(ignore_symbols)) {
  update_polys_ignore <- update_polys[(update_polys$MUSYM %in% ignore_symbols),]
  ignored_acres <- round(sum(update_polys_ignore$Shape_Area) / 4046.86)
}
ignore.idx <- !(update_polys$MUSYM %in% ignore_symbols)
if(length(ignore.idx)) { update_polys_t <- spTransform(update_polys[ignore.idx,], CRS(proj4string(lcb)))
} else update_polys_t <- spTransform(update_polys, CRS(proj4string(lcb)))
impact_acres <- aggregate(update_polys_t$Shape_Area / 4046.86, by=list(update_polys_t$MUSYM), FUN=function(x) { round(sum(x)) })
colnames(impact_acres) <- c("MUSYM", "Acres")
total_impact_acres <- sum(impact_acres$Acres) + ignored_acres

writeOGR(update_polys_t, dsn = getwd(), layer = output_shp, driver = 'ESRI Shapefile')
###

###
### OUTPUT
###
print(paste0("Added to legend: ", paste(to_add,collapse = ",")))
print(paste0("Made additional: ", paste(to_remove,collapse = ",")))
print(paste0("Acreage already accounted for (ignored MUSYMS; not added to shapefile): ", ignored_acres))

#table of acres by musym
impact_acres

print(paste0("Total update impact: ", total_impact_acres, " acres"))
print(paste0("Total update impact (ignored MUSYMs removed): ", total_impact_acres - ignored_acres, " acres"))
###

#this doesn't work.
# ###
# #calculate land category breakdown overall and for individual mapuntis
# update_polys_t <- spTransform(update_polys, CRS(proj4string(lcb)))
#lcb_crop <- crop(lcb, extent(update_polys_t))
# foo <- intersect(lcb_crop, update_polys_t)
# ###
# 
foo <- readOGR(dsn='S:/NRCS/430 SOI Soil Survey/430-15 Soil Correlation/CA630/Joins', layer='CA630_exactjoin_projects')
