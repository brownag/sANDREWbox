# basic demo of using arcgisbinding to read a shapefile and create an sf object

library(arcgisbinding)

arc.check_product()

res <- arc.open("E:/CA649/penonblanco/output/working_progress.shp")

res_sf <- arc.shape2sf(arc.shape(arc.select(object = res,
                                 fields = c("FID","Shape"),
                                 where_clause = "musym = '7076'")))
plot(res_sf)
