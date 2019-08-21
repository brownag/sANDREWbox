# sampling and prediction scheme

# runs off product "s" of multi-project-vis.R

# prepare a set of sampling locations
s$pID <- 1:nrow(s)
s.utm <- sp::spTransform(s, sp::CRS("+proj=utm +zone=10 +units=m"))
foo <- sharpshootR::constantDensitySampling(s.utm, n.pts.per.ac = 1)
raster.list <- list(
  `maatC`='C:/Geodata/project_data/MUSum_PRISM/final_MAAT_800m.tif',
  `mapmm`='C:/Geodata/project_data/MUSum_PRISM/final_MAP_mm_800m.tif',
  `epptmm`='C:/Geodata/project_data/MUSum_PRISM/effective_precipitation_800m.tif',
  `ffd`='C:/Geodata/project_data/MUSum_PRISM/ffd_50_pct_800m.tif',
  `slope`='C:/Geodata/project_data/MUSum_10m_SSR2/SSR2_Slope10m_AEA.tif',
  `abr`='C:/Geodata/project_data/ssro2_ann_beam_rad_int.tif',
  `swi`='C:/Geodata/project_data/ssro2_saga_twi_int.tif'
)

library(raster)
raster.list <- rapply(raster.list, how = "replace", f = function(i) {
  i <- try(raster::raster(i))
  if (class(i) == "try-error")
    stop(paste0("Cannot find raster file: ", i), call. = FALSE)
  else return(i)
})
rez <- lapply(raster.list, extract, y=foo)
rez.df <- as.data.frame(do.call('cbind', rez))
rez.df$pID <- foo$pID
rez.df$musym <- merge(rez.df, s.utm, by="pID", all.x=T, all.y=F, drop=F)$musym
rez.df$musym <- factor(rez.df$musym)
save(rez.df, file="samples_bak.Rda")
load("samples_bak.Rda")


# fit a random forest model to the full-resolution data
library(randomForest)
model <- musym ~ maatC + mapmm + epptmm + ffd + abr + swi
rf1 <- randomForest(model, data=rez.df)

# prepare a set of coarse rasters for prediction

# pick a target grid/CRS (slope map)
target <- raster.list[[5]]

# create polygon from extent of mupolygons, in target CRS
s.target <- spTransform(FedData::polygon_from_extent(s), CRS(proj4string(target)))

# crop the target grid to extent of mupolygons
target <- crop(target, extent(s.target))

# create a dissolved mupolygon for masking, in target CRS
s.agg <- spTransform(aggregate(s, dissolve = TRUE), CRS(proj4string(s.target)))

# iteratively load rasters -- first crop to extent of shapefile (in foreign CRS)
# then mask out all areas not overlapping the shapefile
predict.ras <- lapply(raster.list[1:length(raster.list)], function(r) { 
  s.target.foreign <- spTransform(s.target, CRS(proj4string(r)))
  out <- crop(r, extent(s.target.foreign))
  #out <- mask(out, spTransform(s.agg, CRS(proj4string(s.target.foreign))))
  out <- projectRaster(out, target, res = 1000)
  return(out)
})
predict.stack <- stack(predict.ras)
save(predict.stack, file="predict_stack_bak.Rda")
writeRaster(predict.stack, "C:/Geodata/CA649_predictionstack.tif", overwrite=TRUE)
predict.stack <- stack("C:/Geodata/CA649_predictionstack.tif")
names(predict.stack) <- names(predict.ras)
plot(predict.stack)

# make a prediction from the stack, using the model trained on detailed data
pred <- predict(predict.stack, rf1)
save(pred, file="prediction.Rda")
writeRaster(pred, "CA649_prediction.tif", overwrite=TRUE)
writeRaster(mask(pred, s.agg), "CA649_prediction_mask.tif", overwrite=TRUE)

cols <- viridis::viridis(7)
plot(mask(pred, s.agg), col=cols[getValues(pred)])
plot(s.agg, add=T)


library(rasterVis)
rasterVis::levelplot(mask(pred, s.agg))
