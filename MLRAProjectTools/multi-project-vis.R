#####
# multi-project-vis.R
#  @purpose: view legend level and spatial extent information for several MLRA projects
#  @author: andrew brown
#  @last updated: 2019/03/19
#####

library(soilDB)

get_nasis_projects <- function() {
    if(!requireNamespace('RODBC'))
      stop('please install the `RODBC` package', call.=FALSE)
    q <- "SELECT projectiid, uprojectid, projectname, projectdesc FROM project_View_1;"
    channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
    d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
    RODBC::odbcClose(channel)
    return(d)
}

# hit NASIS local DB to get project names
p <-  get_nasis_projects()

# apply filter on project name
p.pattern <- "Auburn|Blasingame"

# use NASIS web reports interface to get legend and component data by project
projects <- soilDB::fetchNASISWebReport(projectname = p$projectname[grepl(p$projectname, pattern=p.pattern)])
save(projects, file = 'projects_bak.Rda')
load('projects_bak.Rda')
projects$spc$compnamepct <- paste0(projects$spc$compname, " (",projects$spc$comppct_r,"%)")

# visualize the horizon data in all mapunits matching the project pattern
par(mar=c(0.2,0.2,0.2,0.2))
groupedProfilePlot(projects$spc, groups='dmudesc', 
                   label = 'compnamepct', id.style="side",
                   color = 'claytotal_r')

# use national mapunit symbol to make MUKEY lookup table
nmusyms <- unique(projects$mapunit$nationalmusym)
q <- paste0('select mukey, muname, nationalmusym, musym from mapunit where nationalmusym IN ', 
            format_SQL_in_statement(nmusyms),";")
mukey.lut.res <- SDA_query(q)
mukey.lut <- mukey.lut.res$mukey
names(mukey.lut) <- mukey.lut.res$nationalmusym
knitr::kable(mukey.lut.res)

# do SDA spatial query using MUKEY to create WKT 
q <- paste0("select G.MupolygonWktWgs84 as geom, mapunit.mukey, muname, musym
  FROM mapunit CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
  WHERE mukey IN ",format_SQL_in_statement(mukey.lut),";")
res <- SDA_query(q)
s <- processSDA_WKT(res)
save(s, file='mupoly_bak.Rda')
load('mupoly_bak.Rda')
plot(s)
plot(FedData::polygon_from_extent(s), add=T, border="RED")

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

rgdal::writeOGR(s, dsn=".", layer="mupoly", driver="ESRI Shapefile", overwrite_layer = TRUE)

cols <- viridis::viridis(7)
plot(mask(pred, s.agg), col=cols[getValues(pred)])
plot(s.agg, add=T)


library(rasterVis)
rasterVis::levelplot(mask(pred, s.agg))
