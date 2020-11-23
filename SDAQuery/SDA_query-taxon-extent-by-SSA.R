library(aqp, warn.conflicts = FALSE)
library(soilDB)
library(sf)
library(ggplot2)
library(ggspatial)

### CONFIG: SOIL SURVEY AREA
soil_survey_area <- "CA630"

### CONFIG: TAXONOMIC SUBGROUP (or other grouping)
taxonomic_level <- "Ultic Haploxeralfs"
group_level <- "taxsubgrp"
majors_only <- TRUE

# get component data
component_data <- fetchSDA(sprintf("areasymbol = '%s'", soil_survey_area))

component_data <- subset(component_data, !is.na(component_data[[group_level]])) 

# get spatial data (based on nmusyms in component data)
spatial_data <- st_as_sf(fetchSDA_spatial(component_data$nationalmusym, by.col = "nmusym"))

# create named list of major components, split on taxsubgrp
if (majors_only) {
  major_component_data <- subset(component_data, majcompflag == "Yes")
} else major_component_data <- component_data

subgrps <- split(major_component_data, group_level)

#' Cross-reference national musym for SPC and spatial data to produce spatial subsets
#'
#' @param spc A SoilProfileCollection
#' @param spat A data.frame, Spatial (sp) or Simple Features (sf) object
#'
#' @return A data.frame, Spatial (sp) or Simple Features (sf) object
#' 
get_poly_from_SPC <- function(spc, spat) {
  spat[spat$nationalmusym %in% spc$nationalmusym,]
}
 
# make extent map of all Ultic Haploxeralfs
display_extent <- get_poly_from_SPC(subgrps[[taxonomic_level]], spatial_data)

## --- Example graphic 1 ---

# create a subset of the soils with temp regime mesic
mesic_ulha <- subset(subgrps[[taxonomic_level]], 
                     taxtempregime == "mesic") ## NOTE: taxtempregime hardcoded

# get extent of mesic subset of Ultic Haploxeralfs
mesic_display_extent <- get_poly_from_SPC(mesic_ulha, spatial_data)

# plot the full extent, and then overplot the mesic portion
ggplot() + 
  geom_sf(data = display_extent, aes(fill = "Thermic"), color=NA) +
  geom_sf(data = mesic_display_extent, aes(fill = "Mesic"), color=NA) +
  annotation_north_arrow(which_north = "true") +
  scale_fill_manual(name = "Ultic Haploxeralfs", values = c("Thermic" = "red", "Mesic" = "blue"))

## --- Example graphic 2 ---

# compare to full extent of ultic haploxeralfs derived from soil web
library(raster)
te_raster <- suppressWarnings(taxaExtent("Ultic Haploxeralfs", "subgroup"))
display_extent_extent <- st_union(st_transform(display_extent, crs(te_raster))$geometry)
par(mfrow = c(1,2), mar = c(1,1,1,2))
plot(display_extent_extent)
plot(te_raster, add = TRUE)
plot(display_extent_extent)
