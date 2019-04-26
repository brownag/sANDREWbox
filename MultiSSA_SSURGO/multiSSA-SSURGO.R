# Multi Soil Survey Area SSURGO output to GeoPKG format
# @author: andrew brown
# 
# example: SSURGO data for all of Tuolumne county (IRWM extent)
# 
# SQLite output based on this Gist by Dylan Beaudette: 
#   https://gist.github.com/dylanbeaudette/dc34a374669c4289096c2b84001b9202

# The "FedData" package provides efficient accessors for a variety of federal data sources
# install.packages("FedData")
library(FedData)

# sf package for appending spatial to GeoPKG
library(sf)

# Tabular data will be stored in a SQLite database within the GeoPKG
# NOTE: MS Access formats that are currently the standard will be obsolete soon.
library(RSQLite)

# Read in template shapefile with RGDAL
library(rgdal)

# read in the IRWM boundary shapefile to clip SSURGO
irwm_boundary <- readOGR(dsn = "TS_IRWM_Boundary", 'TS_IRWM_Boundary')

# factors take up extra memory -- and where we are going we don't need them
options(stringsAsFactors = FALSE)

# use FedData to get the constituent survey areas and stitch them together 
# (note: if local RAW files exist it will not redownload)
#s <- get_ssurgo(template = irwm_boundary, label = "tuol-stan_IWRM", force.redo = TRUE)
#save(s, file="ca109.Rda")
load(file="ca109.Rda")

# NOTE: this won't work if QGIS or ArcMap recently opened the file
output.file <- 'testgdal.gpkg'
unlink(output.file, force = TRUE)

# force datatype of MUKEY in spatial data to match tabular 
s$spatial$MUKEY <- as.integer(s$spatial$MUKEY)
s$spatial$MUKEYchr <- as.character(s$spatial$MUKEY)

# save MU polygons in GeoPKG (readable by ArcMap 10.2.2+ and QGIS)
# write_sf(st_as_sf(s$spatial), output.file, layer = 'mu_poly')
# write_sf(st_as_sf(s$spatial), "test2.gpkg", layer = 'mu_poly')
rgdal::writeOGR(s$spatial, dsn = "testgdal.gpkg", layer = "mu_poly", driver = "GPKG", overwrite_layer = TRUE)

#rgdal::writeOGR(s$spatial,dsn=".", layer="mupoly_tuol-stan_IWRM", driver='ESRI Shapefile', overwrite_layer = T)

# save tabular data into same file, note 1:many relationship
# include row.names so that there is a unique ID for ArcMap

# s.tabular.clean <- lapply(s$tabular, FUN = function(d) {
#   d[is.na(d)] <- ""
#   return(d)
# })

#output.file <- "test.sqlite"
con <- dbConnect(RSQLite::SQLite(), output.file)
RSQLite::dbListTables(con)
for(i in 1:length(names(s$tabular))) {
  i <- 57
  df <- s.tabular.clean[[i]]
  
  if(nrow(df)){
    df <- cbind(data.frame(oid = 1:nrow(df), df))
    dbWriteTable(con, name = names(s$tabular)[i], value = df, overwrite=TRUE)
    #write.csv(df, file=paste0("tabular/",names(s$tabular)[i],".csv"))
  }  
}
dbDisconnect(con)

