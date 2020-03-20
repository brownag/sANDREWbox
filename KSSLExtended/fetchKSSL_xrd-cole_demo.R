# fetchKSSL XRD data demo -- comparing montmorillonite peaks with COLE
# demonstration of new extended geochemical data option in fetchKSSL
# andrew brown; 2020/3/19
library(dplyr)
library(aqp)
library(soilDB)

# select some MLRAs to compare
#   Blackland Prairies, Cretaceous Coastal Plain, Southern Coastal Plain
mlras <- list('85','86A','86B','133A','135A','135B')

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
xrd_thermal <- do.call('rbind', lapply(f, function(x) x$xrd_thermal))

xrd_thermal <- dplyr::filter(xrd_thermal, 
                             x_ray_method == "7A2i", 
                             is.na(differential_thermal_analysis_method))
xrd_thermal <- xrd_thermal[,-which(apply(xrd_thermal, 2, function(x) all(is.na(x))))]

horizons(spc) <- unique(xrd_thermal[na.omit(match(spc$labsampnum, xrd_thermal$labsampnum)),])

splined <- data.frame(mt_montmorillonite_x_ray = horizons(spc)$mt_montmorillonite_x_ray, 
                      COLEws = horizons(spc)$COLEws)

# make a plot -- at what point does a montmorillonite peak suggest elevated COLE?
boxplot(data = splined, 
     COLEws ~ mt_montmorillonite_x_ray, 
     pch = 19, cex = 0.3,
     main = paste("Relationship between Montmorillonite XRD Peak Size\n and Coefficient of Linear Extensibility \n(MLRAs 85, 86A, 86B, 133A, 135A, 135B)"))

# jitter added for clarity, spline is fit through untransformed values
splined <- df[complete.cases(df), ]

lines(smooth.spline(splined$mt_montmorillonite_x_ray, splined$COLEws, 
                    df = length(unique(splined$mt_montmorillonite_x_ray))), 
      lwd=2, lty=2, col='RED')


