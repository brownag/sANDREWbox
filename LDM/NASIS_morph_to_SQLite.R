# # convert morphologic data to sqlite
# 
# library(sf)
# 
# gdb.path <- "E:/Geodata/soils/NASIS_Morphological_20200116.gdb/NASIS_Morphological_20200116.gdb"
# # lays <- sf::st_layers(gdb.path)
# # 
# library(DBI)
# library(RSQLite)
# con <- dbConnect(RSQLite::SQLite(), "E:/Geodata/soils/NASIS_morphological_20200116.sqlite")
# # 
# # # who decided it was a good idea to use title case for domain values?!
# lowercols <- c("bounddistinct","boundtopo","texcl", "lieutex", "phfield", "effclass",
#                "rupresblkdry", "rupresblkmst", "rupresblkcem", "stickiness", "plasticity", "texture_class",
#                "colorphysst", "colormoistst","featkind")
# # 
# foo <- c("phorizon","phcolor","pediagfeatures")
# lapply(foo, function(x) {
#   .d <- sf::st_read(gdb.path, x, stringsAsFactors = FALSE)
#   .d[] <- lapply(colnames(.d), function(y) if(y %in% lowercols) return(tolower(.d[[y]])) else return(.d[[y]]))
#   try(DBI::dbWriteTable(con, x, soilDB::code(.d), overwrite=TRUE))
# })
# DBI::dbDisconnect(con)

library(soilDB)
# createStaticNASIS(c("MetadataDomainMaster",
#                     "MetadataTableColumn",
#                     "MetadataDomainDetail",
#                     "geomorfeat",
#                     "geomorfeattype"),
#                   SS = FALSE,
#                   output_path = "E:/Geodata/soils/NASIS_morphological_20200116.sqlite")

morphology_spc <- fetchNASIS(dsn = "E:/Geodata/soils/NASIS_morphological_20200116.sqlite", SS = FALSE)
save(morphology_spc, file = "E:/Geodata/soils/morphology_spc.rda")

# write.csv(horizons(morphology_spc), file = "E:/Geodata/soils/NASIS_morph_spc_horizons.csv")
write.csv(diagnostic_hz(morphology_spc), file = "E:/Geodata/soils/NASIS_morph_spc_diag.csv")
head(diagnostic_hz(morphology_spc))
# morphology_spc <- read.csv("E:/Geodata/soils/NASIS_morph_spc_horizons.csv")
