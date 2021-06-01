library(aqp)
library(soilDB)
library(sf)
library(raster)

series <- "Musick"
clan_level <- "subgroup"

target <- seriesExtent(series)

clan <- siblings(series, cousins = TRUE)

all_series <- rbind(clan$sib, clan$cousins)

osd_list <- unique(c(series, all_series$sibling))
chunks <- makeChunks(osd_list)
osds_ex <- lapply(1:max(chunks), function(idx) fetchOSD(osd_list[chunks == idx], extended = TRUE))
osds <- aqp::combine(lapply(osds_ex, function(x) x$SPC))

# "top 6" Subgroups in clan
clan_leaders <- tail(sort(table(osds[[clan_level]])))

clan_rasters <- lapply(names(clan_leaders), function(x) suppressWarnings(taxaExtent(x, clan_level)))

clan_extent <- extent(st_bbox(do.call('c', sapply(clan_rasters, function(x) {
      st_as_sf(as(extent(x), 'SpatialPolygons'))
    }))))

# template for the clan_stack
clan_raster <- raster(ext = clan_extent, 
                      resolution = res(clan_rasters[[1]]), 
                      crs = crs(clan_rasters[[1]]))
                       
clan_stack <- stack(lapply(clan_rasters, function(x) suppressWarnings(projectRaster(x, clan_raster))))
clan_stack_true <- clan_stack
values(clan_stack_true) <- factor(values(clan_stack_true) > 0)
par(fg = "white", bg = "gray50")
plot(factor(clan_stack_true) > 0)
