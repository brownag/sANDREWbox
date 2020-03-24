# fetchKSSL heavy metals demo by mlra (all horizons with optical glass counts by 7B1a2)
# demonstration of new extended geochemical data option in fetchKSSL
# andrew brown; 2020/3/23

library(aqp)
library(soilDB)

# select some MLRAs to compare
mlras <- c('127','147','148')

# further select some elements (patterns to search in column names)
element.list <- list("lead", "copper", "zinc", "mercury")

# fetchKSSL for set of MLRAs
# with returnGeochemicalData = TRUE the result is a list, not just a SPC
#  - SPC is in the $SPC element
#  - geochem is in $geochem
#  - optical in $optical
#  - xray in $xrd_thermal
f <- fetchKSSL(mlra = mlras, returnGeochemicalData = TRUE)

# extract just SPC component from each MLRA and combine them with union
spc <- f$SPC

# extract geochem table from each MLRA and combine with rbind
geochem <- f$geochem

# keep just rows with trace elemental analysis counts
geochem <- geochem[geochem$trace_element_method == "4H1a",]

# remove non-target elements, keep identifier columns
keep.idx <- do.call('c', lapply(element.list, function(element) { 
    grep(element, colnames(geochem))
  }))
geochem<- geochem[, c(1:3,keep.idx)]

# transfer geochem to horizon table (note that labsampnum is commonly nonunique)
horizons(spc) <- unique(geochem)

# transfer mlra labels to geochem table
spc$hzmlra <- denormalize(spc, 'mlra')
h <- horizons(spc)
geochem$mlra <- h[match(geochem$labsampnum, h$labsampnum), 'hzmlra']

# do some simple boxplots, one for each element
lapply(as.list(colnames(geochem[,-c(1:3, length(colnames(geochem)))])), function(element) {
  print(sprintf("%s ~ mlra", element))
  boxplot(data=geochem, as.formula(sprintf("%s ~ mlra", element)), xlab="MLRA")
})

# slab-wise plots 

# slab by depth.interval
depth.interval <- 10
res <- slab(spc, fm = mlra ~ lead_trace_element + mercury_trace_element, slab.structure = depth.interval)

library(lattice)
for(m in mlras) {
  res.sub <- res[res$mlra == m,]
  print(xyplot(top ~ p.q50 | variable,  data=res.sub, 
               ylab='Depth',
               xlab=paste0('median bounded by 25th and 75th percentiles (MLRA: ', m,')'),
               lower=res.sub$p.q25, upper=res.sub$p.q75, ylim=c(200,-2),
               panel=panel.depth_function,
               alpha=0.25, sync.colors=TRUE,
               par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2)),
               prepanel=prepanel.depth_function,
               cf=res.sub$contributing_fraction, cf.col='black', cf.interval=depth.interval, 
               layout=c(2,1), strip=strip.custom(bg=grey(0.8)),
               scales=list(x=list(tick.number=4, alternating=3, relation='free'))
  ))
}
