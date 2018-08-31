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

#gdb_arc <- readOGR(dsn="S:/NRCS/Archive_Andrew_Brown/CA630/WesternJoin/MUSYM_Changes","master_gdb_20180417")
#p <- polygonAdjacency(gdb_arc, v = 'MUSYM')
#fids <- sapply(slot(gdb_arc, "polygons"), slot, "ID")
#plot(gdb_arc[p$commonLines,])
#p$commonLines
common.lines <- readOGR(dsn="S:/NRCS/Archive_Andrew_Brown/CA630/WesternJoin/MUSYM_Changes","commonlines630_077_632_644")

areasyms <- gdb$AREASYMBOL
fid.lookup.table <- data.frame(areasym=areasyms)
row.names(fid.lookup.table) <- fids
common.lines$LEFT_AREASYM <- fid.lookup.table[as.numeric(common.lines$LEFT_FID), ]
common.lines$RIGHT_AREASYM <- fid.lookup.table[as.numeric(common.lines$RIGHT_FID), ]

common.lines.sub <- common.lines[which(common.lines$LEFT_AREASYM == common.lines$RIGHT_AREASYM),]

plot(common.lines.sub)
writeOGR(common.lines.sub, dsn="S:/NRCS/Archive_Andrew_Brown/CA630/WesternJoin/MUSYM_Changes", layer="visual_csl_check_20180413",driver="ESRI Shapefile")


## construct a list of new symbols by survey area
## 

#MU concepts brought into/out of CA630
syms_ca630 <- levels(gdb1$MUSYM)
syms_ca077 <- levels(gdb2$MUSYM)
syms_ca632 <- levels(gdb3$MUSYM)
syms_ca644 <- levels(gdb4$MUSYM)
newsym_630 <- syms_ca630[nchar(levels(gdb1$MUSYM)) <= 3]
newsym_630
newsym_077 <- syms_ca077[nchar(levels(gdb2$MUSYM)) >= 4] #pulling in all along CA077 boundary
newsym_077
newsym_632 <- syms_ca632[nchar(levels(gdb3$MUSYM)) >= 4]
newsym_632
newsym_644 <- syms_ca644[!is.na(as.numeric(levels(gdb4$MUSYM)))]
newsym_644

data.frame(newsym_630)
