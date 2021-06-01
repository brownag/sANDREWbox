library(soilDB)

res <- SDA_query("SELECT * FROM lab_combine_nasis_ncss 
                    LEFT JOIN lab_layer ON
                          lab_combine_nasis_ncss .pedon_key = lab_layer.pedon_key
                    LEFT JOIN lab_chemical_properties ON 
                          lab_layer.labsampnum = lab_chemical_properties.labsampnum
                  WHERE hzn_desgn LIKE 'A' AND 
                        (total_carbon_ncs > 12 OR organic_carbon_walkley_black > 12)")

sum(res$total_carbon_ncs > 12, na.rm=T)
sum(res$organic_carbon_walkley_black > 12, na.rm=T)

sum(res$total_carbon_ncs > 12 | res$organic_carbon_walkley_black > 12, na.rm=TRUE)

unique(c(res$total_carbon_ncs_method, res$oc_walkley_black_method))

library(sf)
res2 <- subset(res, !is.na(longitude_decimal_degrees))
nrow(unique(res2[,c("longitude_decimal_degrees","latitude_decimal_degrees")]))
res2 <- st_as_sf(res2, coords = c("longitude_decimal_degrees","latitude_decimal_degrees"))

maps::map("state")
plot(res2$geometry, add=T)
