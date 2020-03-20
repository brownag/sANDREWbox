# fetchKSSL heavy metals demo by mlra (all horizons with optical glass counts by 7B1a2)
# demonstration of new extended geochemical data option in fetchKSSL
# andrew brown; 2020/3/19

library(dplyr)
library(aqp)
library(soilDB)

# select some MLRAs to compare
mlras <- list('127','147','148')

# further select some elements (patterns to search in column names)
element.list <- list("lead", "copper", "zinc", "mercury")

# use lapply to iterate over MLRAs and do fetchKSSL
# with returnGeochemicalData = TRUE the result is a list, not just a SPC
# For each MLRA:
#  - SPC is in the $SPC element
#  - geochem is in $geochem
#  - optical in $optical
#  - xray in $xrd_thermal

f <- lapply(mlras, function(a.mlra) soilDB::fetchKSSL(mlra = a.mlra, returnGeochemicalData = TRUE))

# extract just SPC component from each MLRA and combine them with union
spc <- aqp::union(lapply(f, function(x) x$SPC))

# extract geochem table from each MLRA and combine with rbind
geochem <- do.call('rbind', lapply(f, function(x) x$geochem))

# keep just rows with trace elemental analysis counts
geochem <- dplyr::filter(geochem, trace_element_method == "4H1a")

# remove non-target elements, keep identifier columns
keep.idx <- do.call('c', lapply(element.list, 
                               function(element) grep(element, colnames(geochem))))
geochem<- geochem[, c(1:3,keep.idx)]

# transfer mlra labels to geochem table
spc$hzmlra <- denormalize(spc, 'mlra')
h <- horizons(spc)
geochem$mlra <- h[match(geochem$labsampnum, h$labsampnum), 'hzmlra']

# do some simple boxplots, one for each element
lapply(as.list(colnames(geochem[,-c(1:3, length(colnames(geochem)))])), function(element) {
  print(sprintf("%s ~ mlra", element))
  boxplot(data=geochem, as.formula(sprintf("%s ~ mlra", element)), xlab="MLRA")
})