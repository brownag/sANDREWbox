library(sf)
library(RSQLite)
library(USAboundaries)
library(FedData)

label <- "ca109"
raw.dir <- "RAW/SSURGO"
extraction.dir <- "EXTRACTIONS"

output.file <-  "ca109.gpkg"
contemporary <- us_counties(states = c("California"))
tuolumne <- contemporary[which(contemporary$countyfp == "109"),]
plot(st_geometry(tuolumne))

# if you need the source data/dont know the necessary SSURGOAreas use get_ssurgo()
#ca109 <- get_ssurgo(template = as(tuolumne, 'Spatial'), label = "ca109", force.redo = TRUE)

# get spatial data out of existing ZIP files downloaded from WSS
template <- as(tuolumne, "Spatial")
template.poly <- polygon_from_extent(template)

# skip the process of SDA query to determine necessary SSAs 
SSURGOAreas <- data.frame(areasymbol=c("CA731","CA630","CA790","CA729","CA649","CA740"), 
                          saverest=c("09/12/2018", "09/17/2018", "09/13/2018", "09/12/2018", "09/14/2018", "09/12/2018"))

# extract (download if needed)
SSURGOData <- lapply(1:nrow(SSURGOAreas), function(i) {
  message("Loading SSURGO data for survey area ", i, " of ", nrow(SSURGOAreas), ": ", as.character(SSURGOAreas$areasymbol[i]))
  get_ssurgo_study_area(template = template.poly, 
                        area = as.character(SSURGOAreas$areasymbol[i]), 
                        date = as.Date(SSURGOAreas$saverest[i], format = "%m/%d/%Y"), 
                        raw.dir = "./RAW/SSURGO/")
})

SSURGOPolys <- lapply(SSURGOData, "[[", "spatial")

message("Merging all SSURGO Map Unit polygons")
SSURGOPolys <- do.call("rbind", SSURGOPolys)

message("Cropping all SSURGO Map Unit polygons to template")
SSURGOPolys <- raster::crop(SSURGOPolys, y=spTransform(template, proj4string(SSURGOPolys)))

# inspect
#plot(template)
#plot(template.poly, add=T)
#plot(SSURGOPolys, col="RED", add=T)

# get tabular data
SSURGOTables <- lapply(SSURGOData, "[[", "tabular")
message("Merging all SSURGO data tables")
tableNames <- unique(unlist(sapply(SSURGOTables, names)))
tableNames <- tableNames[order(tableNames)]
SSURGOTables <- lapply(tableNames, function(name) {
  tables <- lapply(SSURGOTables, "[[", name)
  tables <- do.call("rbind", tables)
  tables <- unique(tables)
  return(tables)
})
names(SSURGOTables) <- tableNames
SSURGOTables <- extract_ssurgo_data(tables = SSURGOTables, mapunits = as.character(unique(SSURGOPolys$MUKEY)))
suppressWarnings(rgdal::writeOGR(SSURGOPolys, dsn = normalizePath(paste0(extraction.dir, "/.")), 
                                 layer = paste0(label, "_SSURGO_Mapunits"), driver = "ESRI Shapefile", 
                                 overwrite_layer = TRUE))
junk <- lapply(names(SSURGOTables), function(tab) {
  readr::write_csv(SSURGOTables[[tab]], path = paste(extraction.dir, 
                                                     "/", label, "_SSURGO_", tab, ".csv", sep = ""))
})

# create unique feature ID
SSURGOPolys$fid <- 1:nrow(SSURGOPolys)

# produce get_ssurgo-like list output
ca109 <- (list(spatial = SSURGOPolys, tabular = SSURGOTables))

# define geometry of geopkg, and write mu polygons to file
write_sf(st_as_sf(ca109$spatial), output.file, layer = 'mu_poly')

# inspect
st_layers("ca109.gpkg")

agg.idx <- which(names(ca109$tabular) == "muaggatt")

ca109$tabular[[agg.idx]]$mukeychr <- as.character(ca109$tabular[[agg.idx]]$mukey)

# connect to write sqlite tables (just muaggatt #56)
con <- dbConnect(RSQLite::SQLite(), dbname = "ca109.gpkg")
dbListObjects(con)
dbCreateTable(con, name = "muaggatt", fields = ca109$tabular[[agg.idx]], overwrite=TRUE)
dbWriteTable(con, name = "muaggatt", value = ca109$tabular[[agg.idx]], overwrite=TRUE)
dbDisconnect(con)
