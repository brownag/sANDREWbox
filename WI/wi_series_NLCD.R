library(aqp)
library(soilDB)
library(sf)
library(raster)
library(exactextractr)

soils <- c("Magnor","Seaton","NewGlarus","Kewaunee")

# read_sf is the {sf} analog of rgdal::readOGR 
# wi.boundary <- read_sf("WIBoundary/WI_Boundary.shp")

# instead of using the buggy boundary, use the us_states data in spData
data("us_states", package = "spData")

# us_states is an sf object
wi.boundary <- us_states[us_states$NAME == "Wisconsin",]

# download SoilWeb extents
extents <- lapply(soils, function(x) { 
  sf.obj <- st_as_sf(seriesExtent(x))
  
  # sf::st_transform is analog of sp::spTransform
  sf.obj <- st_transform(sf.obj, st_crs(wi.boundary))
  
  # st_intersection returns the portion of sf.obj within wi.boundary
  sf.obj.wi <- st_intersection(sf.obj, wi.boundary)
  
  return(sf.obj.wi)
})

# make a combined  (multi-multipolygon) sf object
all.extents <- do.call('rbind', extents)
# save the extents as a shp
write_sf(all.extents, "wi_series_extents.shp", delete_layer = TRUE)
# split back out into a list where each element is a single multipolygon
extents <- split(read_sf("wi_series_extents.shp"), f = 1:length(extents))

# load NLCD (or other categorical) raster
landcover <- raster("YOUR RASTER FILE HERE .tif")

# extract the data (assuming a single multipolygon geometry per extent)
rdata <- lapply(extents, function(x) {
    x <- st_transform(x, st_crs(landcover))
    exact_extract(landcover, x)[[1]]
  })
# there are many different built in summary operations you can use too! see ?exact_extract(fun)

# iterate over soils
res <- do.call('rbind', lapply(1:length(soils), function(i) {
  
  # we will just take raster cells that completely fall within extent polygon (no partial overlap)
  rvalues <- rdata[[i]]$value[rdata[[i]]$coverage_fraction == 1]
  
  # create a data.frame row, with tabulated raster values by category
  data.frame(cbind(soil = soils[i],
                   # coded values from raster need to be converted to human-readable names, percentages, and rounded
                   t(round(prop.table(table(factor(rvalues,
                                                   # scraped from mucomparison report categorical definitions config file
                                                   levels = c(0L, 11L, 12L, 21L, 22L, 23L, 24L, 31L, 
                                                              41L, 42L, 43L, 51L, 52L, 71L, 72L, 73L, 
                                                              74L, 81L, 82L, 90L, 95L), 
                                                   labels = c("nodata", "Open Water", "Perennial Ice/Snow", "Developed, Open Space", 
                                                              "Developed, Low Intensity", "Developed, Medium Intensity", 
                                                              "Developed, High Intensity", "Barren Land (Rock/Sand/Clay)", 
                                                              "Deciduous Forest", "Evergreen Forest", 
                                                              "Mixed Forest", "Dwarf Scrub", "Shrub/Scrub", "Grassland/Herbaceous", 
                                                              "Sedge/Herbaceous", "Lichens", "Moss", "Pasture/Hay", "Cultivated Crops", 
                                                              "Woody Wetlands", "Emergent Herbaceous Wetlands"),))) * 100, 1))))
}))

# for each row in res (series name) calculate a "top three" cover composition summary
res$top_three <- apply(res, 1, function(x) {
  paste0(names(x[-1][order(as.numeric(x[-1]), decreasing = TRUE)][1:3]), collapse = "/")
})

# inspect result
head(res)
