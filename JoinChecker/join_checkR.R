library(rgdal)
library(sharpshootR)

base.path <- 'L:/NRCS/MLRAShared/'
survey.areas <- c('CA630','CA077','CA632','CA644')
file.names <- paste0("/", survey.areas, "_Join_FY2018.gdb")
full.paths <- paste0(base.path, survey.areas, file.names)
full.paths[1] <- "L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb"
full.paths

gdb1 <- readOGR(full.paths[1], layer="ca630_a")
gdb2 <- readOGR(full.paths[2], layer="ca077_a")
gdb3 <- readOGR(full.paths[3], layer="ca632_a")
gdb4 <- readOGR(full.paths[4], layer="ca644_a")

gdb <- rbind(gdb1[(names(gdb1) %in% names(gdb2))], 
             gdb2[(names(gdb2) %in% names(gdb1))],
             gdb3[(names(gdb3) %in% names(gdb1))],
             gdb4[(names(gdb4) %in% names(gdb1))])

p <- polygonAdjacency(gdb, v = 'MUSYM')


plot(gdb)
writeOGR(gdb, dsn=".", layer = "test", driver="ESRI Shapefile",overwrite_layer=T)

common.lines <- readOGR(dsn="S:/NRCS/Archive_Andrew_Brown/CA630/WesternJoin/MUSYM_Changes","commonlines630_077_632_644")

fids <- sapply(slot(gdb, "polygons"), slot, "ID")
areasyms <- gdb$AREASYMBOL
fid.lookup.table <- data.frame(areasym=areasyms)
row.names(fid.lookup.table) <- fids
common.lines$LEFT_AREASYM <- fid.lookup.table[as.numeric(common.lines$LEFT_FID),]
common.lines$RIGHT_AREASYM <- fid.lookup.table[as.numeric(common.lines$RIGHT_FID),]

common.lines.sub <- common.lines[which(common.lines$LEFT_AREASYM == common.lines$RIGHT_AREASYM),]

plot(common.lines.sub)
writeOGR(common.lines.sub, dsn="S:/NRCS/Archive_Andrew_Brown/CA630/WesternJoin/MUSYM_Changes", layer="visual_csl_check_20180413",driver="ESRI Shapefile")
