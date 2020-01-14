## Load packages
library(getSpatialData)
library(sf)

aoi <- rgdal::readOGR("E:/CA649/CA649_b.shp")

set_aoi(aoi)
time_range <-  c("2018-06-01", "2018-12-31")
login_USGS("br0wn.andrewg", password = "thisisaverylongpassword1")
set_archive("E:/CA649/LANDSAT")

product_names <- getLandsat_names()

## query for records for your AOI, time range and product
query <- getLandsat_query(time_range = time_range, name = product_names[4])

## preview a record
getLandsat_preview(query[3,])

#print available levels for a record
query[5,]

## download record 5 with level "l1" (will direct to AWS automaticaly)
files <- getLandsat_data(records = query[3,], level = "l1", source = "auto")
#espa-andrew.g.brown@ca.usda.gov-07022019-135831-039


espa_order(query[3,],)