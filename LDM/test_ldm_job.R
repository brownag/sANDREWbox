
bigspc <- soilDB::fetchLDM(toupper(SoilTaxonomy::taxon_code_to_taxon(LETTERS[1:12])),
                          dsn = "E:/Geodata/soils/LDM-compact.sqlite",
                          what = "corr_taxorder", tables = c("lab_physical_properties",
                                                             "lab_chemical_properties"))
save(bigspc, file = "bigspc.rda")