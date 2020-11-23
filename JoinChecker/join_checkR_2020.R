library(rgdal)
library(sharpshootR)

full.paths <- vector('character',2)
full.paths[1] <- "E:/CA649/Geodata/Offical_Geodatabase/FGDB_CA630_Projects_2020_0922_ra.gdb"
full.paths[2] <- 'E:/CA649/Geodata/Offical_Geodatabase/FGCA649_Projects_2020_0921_agb.gdb'

gdb1 <- readOGR(full.paths[1], layer="ca630_a")
gdb2 <- readOGR(full.paths[2], layer="ca649_a")
gdb1 <- spTransform(gdb1, proj4string(gdb2))

gdb <- rbind(gdb1[(names(gdb1) %in% names(gdb2))], 
             gdb2[(names(gdb2) %in% names(gdb1))])

writeOGR(gdb, dsn = "JoinChecker", layer = "ca649_ca630_merge", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

common.lines <- readOGR(dsn="E:/workspace/sANDREWBOX/JoinChecker/QA_CommonLines.shp")

areasyms <- gdb$AREASYMBOL
fid.lookup.table <- data.frame(areasym = areasyms)
row.names(fid.lookup.table) <- fids
common.lines$LEFT_AREASYM <- fid.lookup.table[as.numeric(common.lines$LEFT_FID), ]
common.lines$RIGHT_AREASYM <- fid.lookup.table[as.numeric(common.lines$RIGHT_FID), ]

common.lines.sub <- common.lines[which(common.lines$LEFT_AREASYM == common.lines$RIGHT_AREASYM),]

plot(common.lines.sub)
writeOGR(common.lines.sub, dsn="E:/workspace/sANDREWBOX/JoinChecker", layer="visual_csl_check_20200921",driver="ESRI Shapefile")
