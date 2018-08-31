raster.list <- list(
    atemp=list(`Mean Annual Air Temperature (degrees C)`='L:/NRCS/MLRAShared/Geodata/climate/raster/final_MAAT_800m.tif'),
    precip=list(`Mean Annual Precipitation (mm)`='L:/NRCS/MLRAShared/Geodata/climate/raster/final_MAP_mm_800m.tif'),
    ffd=list(`Frost-Free Days`='L:/NRCS/MLRAShared/Geodata/climate/raster/ffd_mean_800m.tif'),
    elevation=list(`Elevation (m)`='L:/NRCS/MLRAShared/Geodata/elevation/10_meter/ca630_elev'),
    slope=list(`Slope Gradient (%)`='L:/NRCS/MLRAShared/Geodata/elevation/10_meter/ca630_slope'),
    aspect=list(`Slope Aspect (degrees)`='L:/NRCS/MLRAShared/Geodata/DEM_derived/ca630_aspect')
)

mu.dsn <- 'L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb'
mu.layer <- 'ca630_a'
mu.col <- 'MUSYM'
pts.per.acre <- 3
p.quantiles <- c(0, 0.05, 0.5, 0.95, 1)
correct.sample.size <- FALSE
cache.samples <- FALSE

