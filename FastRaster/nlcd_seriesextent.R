library(aqp)
library(soilDB)
library(sf)
library(raster)
library(exactextractr)

soils <- c("Peters","Auburn","Sierra","Musick")

# download SoilWeb extents
extents <- lapply(soils, function(x) st_as_sf(seriesExtent(x)))

# load NLCD (or other categorical) raster
landcover <- raster("C:/Geodata/project_data/nlcd_2011_cropped.tif")

# extract the data (assuming a single multipolygon geometry per extent)
rdata <- lapply(extents, function(x) exact_extract(landcover, x)[[1]])
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

# soil nodata Open.Water Perennial.Ice.Snow Developed..Open.Space Developed..Low.Intensity Developed..Medium.Intensity
# 1 Peters      0        1.1                  0                   3.2                      0.2                         0.1
# 2 Auburn      0        1.4                  0                     4                      1.8                         0.9
# 3 Sierra      0        1.1                  0                     3                      0.7                         0.3
# 4 Musick      0        0.7                  0                   3.5                      0.7                         0.2
# Developed..High.Intensity Barren.Land..Rock.Sand.Clay. Deciduous.Forest Evergreen.Forest Mixed.Forest Dwarf.Scrub
# 1                         0                          0.3              0.4                0            0           0
# 2                       0.2                          0.3             10.7             15.2          1.5           0
# 3                       0.2                            0             11.4             28.1          2.3           0
# 4                       0.1                            0              5.7               62          5.1           0
# Shrub.Scrub Grassland.Herbaceous Sedge.Herbaceous Lichens Moss Pasture.Hay Cultivated.Crops Woody.Wetlands
# 1         1.7                 79.2                0       0    0         5.3              7.6            0.7
# 2        12.8                   50                0       0    0         0.5              0.4            0.1
# 3        22.4                 29.1                0       0    0         1.2              0.3              0
# 4        15.8                  6.1                0       0    0           0                0              0
# Emergent.Herbaceous.Wetlands                                         top_three
# 1                            0 Grassland.Herbaceous/Cultivated.Crops/Pasture.Hay
# 2                            0 Grassland.Herbaceous/Evergreen.Forest/Shrub.Scrub
# 3                            0 Grassland.Herbaceous/Evergreen.Forest/Shrub.Scrub
# 4                          0.1 Evergreen.Forest/Shrub.Scrub/Grassland.Herbaceous
