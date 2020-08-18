# basic point-location REST requests to SoilGrids v2 API and wrangling of data into SoilProfileCollection object
# last update: 2020/08/18

library(httr)
library(jsonlite)
library(tidyverse)

# point id, latitude and longitude as inputs
your.points <- tribble(~id, ~lat, ~lon,
                       "A", 37.9, -120.3,
                       "B", 38.1, -121.5)

# iterate over points and download data from REST endpoint
res <- lapply(split(your.points, f = your.points$id), function(yd) {
  id <- yd$id
  lat <- yd$lat
  lon <- yd$lon
  
  response <- httr::GET(sprintf("https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lat=%s&lon=%s", lat, lon))
  r.content <- httr::content(response, as = "text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(r.content)
  
  extractSGLayerProperties <- function(x) {
    out <-  res$properties$layers[res$properties$layers$name == x,]$depths[[1]]
    
    # fix names and labels for downstream
    out <- out[,colnames(out)[grep("range", colnames(out), invert = TRUE)]]
    out <- data.frame(label = gsub("cm", "", out$label), values = out$values)
    colnames(out) <- gsub("\\.Q0\\.", "Q", colnames(out))
    colnames(out) <- gsub("Q5", "Q50", colnames(out))
    colnames(out) <- gsub("values", x, colnames(out))
    
    return(out)
  }
  
  soc <- extractSGLayerProperties("soc")
  bdod <- extractSGLayerProperties("bdod")
  phh2o <- extractSGLayerProperties("phh2o")
  clay <- extractSGLayerProperties("clay")
  cec <- extractSGLayerProperties("cec")
  
  # create new horizon data, merge in each property using depth label
  hz.data <- tibble(id = id, lat = lat, lon = lon, label = soc[,"label"])
  hz.data <- hz.data %>% 
    merge(soc, by = "label") %>%
    merge(bdod, by = "label") %>%
    merge(phh2o, by = "label") %>%
    merge(clay, by = "label") %>%
    merge(cec, by = "label")
  
  rownames(hz.data) <- NULL
  
  return(hz.data)
})

# combine horizon data together
hz.data.all <- do.call('rbind', res)

# calculate top and bottom depths from label
spc <- separate(hz.data.all, label, sep = "-", into = c("top", "bottom"))

###
### spc is a tibble with all your data in it
### 

# from here, you can do your analysis e.g. with aqp
# install if needed:
#  install.packages(aqp)
#  remotes::install_github("ncss-tech/aqp", dependencies=FALSE)

library(aqp)

# promote horizon data to SoilProfileCollection
depths(spc) <- id ~ top + bottom

# plot median/50th percentile SOC for spc truncated to [0,50]
plot(trunc(spc, 0, 50), color = "socQ50")

# plot low/5th percentile
plot(trunc(spc, 0, 50), color = "socQ05")

# plot high/95th percentile
plot(trunc(spc, 0, 50), color = "socQ95")
