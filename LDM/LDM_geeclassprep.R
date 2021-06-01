# training data for google earth engine class
library(aqp)
library(soilDB) # need ncss-tech/soilDB@fetchLDM branch
library(sf)

ssa_codes <- c("CA630","CA649","CA750","CA731","CA790")

# get and inspect the boundaries
bounds <- st_as_sf(fetchSDA_spatial(ssa_codes, geom.src = "sapolygon"))
plot(bounds$geometry)

# get the lab data
centralsierra_pedons <- fetchLDM(ssa_codes, what = "area_code")
centralsierra_pedons_sp <- subset(centralsierra_pedons, !is.na(latitude_decimal_degrees))

# promote to spatial
coordinates(centralsierra_pedons_sp) <- ~ longitude_decimal_degrees + latitude_decimal_degrees

# weighted averages over 0-50cm interval
csp50 <- site(trunc(centralsierra_pedons_sp, 0, 50) %>% 
  transform(thk = hzn_bot - hzn_top,
            carbon_total = ifelse(!is.na(total_carbon_ncs), 
                                  total_carbon_ncs, 
                                  organic_carbon_walkley_black)) %>% 
  mutate_profile(wtd_clay = sum(clay_total * (thk / sum(thk))),
                 wtd_oc = sum(carbon_total * (thk / sum(thk)))))[,c(idname(centralsierra_pedons_sp),
                                                                 "wtd_clay","wtd_oc")]
site(centralsierra_pedons_sp) <- csp50
centralsierra_pedons_sp <- subset(centralsierra_pedons_sp, !is.na(wtd_clay))

point_data <- st_as_sf(as(centralsierra_pedons_sp, 'SpatialPointsDataFrame'))

target_columns <- c("pedon_key","corr_name","corr_taxorder","wtd_clay","wtd_oc")
point_data_sub <- point_data[,target_columns]
point_data_sub <- point_data_sub[!is.na(point_data_sub$wtd_clay),]

plot(point_data_sub$geometry, add=TRUE, col=point_data_sub$wtd_clay)

st_write(point_data_sub,
         dsn = "E:/GTAC_GEE/MyData/CentralSierra_pedons.shp", append = FALSE)
st_write(st_union(bounds),
         dsn = "E:/GTAC_GEE/MyData/CentralSierra_bound.shp", append = FALSE)
