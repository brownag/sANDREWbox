library(aqp, warn.conflicts = FALSE)
library(raster, warn.conflicts = FALSE)
library(soilDB)
library(sf)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(ggspatial)

### CONFIG: SOIL SURVEY AREA
soil_survey_area <- "CA630"

### CONFIG: TAXONOMIC SUBGROUP (or other grouping)
#  NOTE: that these arguments are customizable without changing source data
mapunit_level <- "nationalmusym"
group_level <- "taxsubgrp"              
taxonomic_level <- "Ultic Haploxeralfs"
majors_only <- TRUE

# save time and stress on SDA server by caching individual SSAs 
cache_file_name <- paste0(soil_survey_area, ".rda")
if (!file.exists(cache_file_name)) {
  # get component data
  component_data <- fetchSDA(sprintf("areasymbol = '%s'", soil_survey_area))
  component_data <- subset(component_data, !is.na(component_data[[group_level]])) 
  
  # get spatial data (based on nmusyms in component data)
  spatial_data <- st_as_sf(fetchSDA_spatial(component_data[[mapunit_level]], by.col = "nmusym"))
  
  save(component_data, spatial_data, file = cache_file_name)
} else {
  load(cache_file_name)
}

if (majors_only) {
  major_component_data <- subset(component_data, majcompflag == "Yes")
} else major_component_data <- component_data 

#' Calculate arbitrary dominant condition from an SDA SPC
#' 
#' @description The dominant condition is selected after aggregating \code{weight_field} to determine aggregate composition at the specified (e.g. taxonomic) level. The final results are filtered based on a specific value to make it easy to subset data based on dominant condition for arbitrary attributes.
#' 
#' @param spc A SoilProfileCollection
#' @param weight_field A weight field (e.g. \code{comppct_r})
#' @param condition_field A condition field (e.g. \code{taxsubgrp})
#' @param condition_value Optional: A condition value for filtering results (e.g. \code{"Ultic Haploxeralfs"})
#'
#' @return A table of national mapunit symbols, condition values, and summed weights; optionally filtered to a specific condition value.
#' 
spc2dominant_condition <-  function(spc,  
                                    weight_field = "comppct_r",
                                    condition_field,
                                    condition_value = NULL,
                                    mapunit_field = "nationalmusym") { 
  
  # NB: dplyr double {{embrace}} for NSE of arguments
  
  step1 <- site(spc)[,c(mapunit_field,condition_field,weight_field)] %>%
    group_by(across(c({{ mapunit_field }}, {{condition_field}}))) %>%
    summarize(across({{ weight_field }}, sum)) %>%
    slice(which({{ weight_field }} == max({{ weight_field }})))
  
   if (!is.null(condition_value))
    return(filter(step1, grepl(x = across(all_of({{ condition_field }})), 
                               pattern = {{ condition_value }})))
  
  return(step1)
}

#' Cross-reference national musym for SPC and spatial data to produce spatial subsets
#' @description Specify a SoilProfileCollection and associated spatial data object. The spatial object is subsetted based on the dominant condition field and value specified (\code{condition_field}, \code{condition_value}). 
#' 
#' @param spc A SoilProfileCollection
#' @param spat A data.frame, Spatial (sp) or Simple Features (sf) object
#'
#' @return A data.frame, Spatial (sp) or Simple Features (sf) object
#' 
get_poly_from_SPC <- function(spc, spat, 
                              weight_field = "comppct_r",
                              condition_field,
                              condition_value = NULL,
                              mapunit_field = "nationalmusym") { 
  spat.dominant <- spc2dominant_condition(spc, weight_field, condition_field, condition_value, mapunit_field)
  
  # this compares field levels between spatial and spc
  #  and then determines which of those correspond with a dominant condition
  spat.matches <- spat[[mapunit_field]] %in% spc[[mapunit_field]]
  spat.is_dominant <- spat[[mapunit_field]] %in% spat.dominant[[mapunit_field]]
  
  spat[spat.matches & spat.is_dominant,]
}


# where are lithic haploxeralfs the dominant condition?
# plot(get_poly_from_SPC(major_component_data, spatial_data, condition_value = "Lithic Haploxeralfs")$geometry)

# make extent map of all Ultic Haploxeralfs (or other target condition value)
display_extent <- get_poly_from_SPC(major_component_data, spatial_data,
                                    condition_field = group_level,
                                    condition_value = taxonomic_level)

# calculate the dominant condition for all nmusyms
dom_group <- spc2dominant_condition(major_component_data, condition_field = group_level)

# calculate those matching user input
dom_group_match <- filter(dom_group, across({{ group_level }}) == taxonomic_level)

# subset the SPC to get just the matching/target components
dominant_components <- subset(major_component_data, 
                              major_component_data[[mapunit_level]] %in% 
                                dom_group_match[[mapunit_level]])

### EXAMPLE CODE -- compare mesic versus thermic ultic haploxeralfs

# no constraint on condition value; get all the polygons with group_level
display_extent_all <- get_poly_from_SPC(major_component_data, spatial_data, 
                                        condition_field = group_level,
                                        condition_value = NULL)

display_extent_all <- merge(display_extent_all, dom_group)

## --- Example graphic 1 ---

ggplot() +
  geom_sf(data = display_extent_all, aes(fill = taxsubgrp), color = NA) +
  annotation_north_arrow(which_north = "true")

## --- Example graphic 2 ---

# create a subset of the soils with temp regime mesic
mesic_ulha <- subset(dominant_components, taxtempregime == "mesic") ## NOTE: taxtempregime hardcoded

# get extent of mesic subset of Ultic Haploxeralfs
mesic_display_extent <- get_poly_from_SPC(mesic_ulha, spatial_data, condition_field = group_level)

# create a subset of the soils with temp regime mesic
thermic_ulha <- subset(dominant_components, taxtempregime == "thermic") ## NOTE: taxtempregime hardcoded

# get extent of mesic subset of Ultic Haploxeralfs
thermic_display_extent <- get_poly_from_SPC(thermic_ulha, spatial_data, condition_field = group_level)

ggplot() + 
  geom_sf(data = thermic_display_extent, aes(fill = "Thermic"), color = NA) +
  geom_sf(data = mesic_display_extent, aes(fill = "Mesic"), color = NA) +
  annotation_north_arrow(which_north = "true") +
  scale_fill_manual(name = "Ultic Haploxeralfs", values = c("Thermic" = "red", "Mesic" = "blue"))

## --- Example graphic 3 ---

# compare to full extent of ultic haploxeralfs derived from the new 800m SoilWeb taxonomic grids
#   the scale bar is the area percentage of 800m square (~160 acres) occupied by that taxon

te_raster <- suppressWarnings(taxaExtent("Ultic Haploxeralfs", "subgroup"))
display_extent_extent <- st_union(st_transform(display_extent, crs(te_raster))$geometry)
mesic_display_extent_extent <- st_union(st_transform(mesic_display_extent, crs(te_raster))$geometry)
thermic_display_extent_extent <- st_union(st_transform(thermic_display_extent, crs(te_raster))$geometry)

chdisplay <- st_convex_hull(display_extent_extent)
te_crop <- crop(te_raster, extent(as_Spatial(display_extent_extent)))
par(mfrow = c(1,3), mar = c(2,2,2,2))

# pane 1
plot(chdisplay, main = "SoilWeb 800m Taxonomic Grid\n(Ultic Haploxeralfs)")
plot(te_crop, col = rev(viridis::viridis(100)), add = TRUE)

# pane 2
plot(te_crop, col = rev(viridis::viridis(100)))
plot(display_extent_extent, add = TRUE)
plot(chdisplay, add = TRUE)

# pane 3
plot(display_extent_extent, main = "Mesic/Thermic Convex Hull")
plot(st_convex_hull(mesic_display_extent_extent), lwd=2, col=rgb(0,0,1,0.5), add = TRUE)
plot(st_convex_hull(thermic_display_extent_extent), lwd=2, col=rgb(1,0,0,0.5), add = TRUE)
