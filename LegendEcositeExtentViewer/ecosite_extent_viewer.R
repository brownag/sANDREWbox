#ecosite extent viewer
# takes spatial SSURGO-style shapefile, aggregates ecosites to determine dominant, tags dominant ecosite to delineations by MUSYM
#requirements:
# - NASIS selected set containing desired DMU components
# - Shapefile with "MUSYM" column identifying spatial extent of DMU components

### 
### SETUP
### 
dmu_desc_prefix <- "CA630"
musym_col_name <- 'MUSYM'

data_source <- "L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb" #path to shp, no trailing slash, can be GDB
data_layer <- "ca630_a" #no .shp extension

output_data_path <- "S:/NRCS/Archive_Andrew_Brown/CA630/Ecosites"
output_layer <- "ca630_a_es"
###
###
###

library(rgdal)
f <- fetchNASIS_component_data()
output <- data.frame(DMU=levels(factor(f$dmudesc)))
output$dominant_ecosite <- ""

for(d in f$dmudesc) {
  f.s <- f[which(f$dmudesc == d), ]
  es.comp <- aggregate(f.s$comppct_r, by=list(f.s$ecosite_id), FUN=sum) #sum up percentage of MU in each ecosite
  domes <- ""
  if(nrow(es.comp)) {
    row.idx <- which(es.comp[,2] == max(es.comp[,2])) #get the dominant ecosite
    domes <- es.comp[row.idx,1]
    if(length(row.idx) > 1) 
      domes <- paste0(domes, collapse = "+")
  }
  output[which(output$DMU == d), ]$dominant_ecosite <- domes #set the corresponding row in the output table
}

loutput <- output[,2]
loutput <- setNames(loutput, output[,1]) #make a named list, using musyms as names with dominant ecosites as value

mu <- readOGR(dsn = data_source, layer = data_layer) #read spatial extent shapefile
mu$dom_ecosite <- loutput[paste0(dmu_desc_prefix,as.character(mu[[musym_col_name]]))] #do ecosite lookup, create new attribute in data frame
writeOGR(mu,dsn = output_data_path, layer=output_layer, driver="ESRI Shapefile", overwrite_layer = T) #write output shapefile
