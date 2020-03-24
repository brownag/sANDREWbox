# fetchKSSL XRD data demo -- comparing montmorillonite peaks with COLE
# demonstration of new extended geochemical data option in fetchKSSL
# andrew brown; 2020/3/23
library(aqp)
library(soilDB)

# select some MLRAs to compare
#   Blackland Prairies, Cretaceous Coastal Plain, Southern Coastal Plain
mlras <- c('85','86A','86B','133A','135A','135B')

# fetchKSSL for set of MLRAs
# with returnGeochemicalData = TRUE the result is a list, not just a SPC
#  - SPC is in the $SPC element
#  - geochem is in $geochem
#  - optical in $optical
#  - xray in $xrd_thermal
f <- fetchKSSL(mlra=mlras, returnGeochemicalData = TRUE)

# extract just SPC component from each MLRA and combine them with union
spc <- f$SPC

# extract geochem table from each MLRA and combine with rbind
xrd_thermal <- f$xrd_thermal

# get just 7A2i, non-differential-thermal
xrd_thermal <-  xrd_thermal[which(xrd_thermal$x_ray_method == "7A2i" &
                             is.na(xrd_thermal$differential_thermal_analysis_method)),]

# remove all-NA columns (neater)
na.col.idx <- which(apply(xrd_thermal, 2, function(x) all(is.na(x))))
xrd_thermal <- xrd_thermal[,-na.col.idx]

# merge back into horizon table
horizons(spc) <- unique(xrd_thermal[na.omit(match(spc$labsampnum, xrd_thermal$labsampnum)),])

splined <- data.frame(mt_montmorillonite_x_ray = horizons(spc)$mt_montmorillonite_x_ray, 
                      COLEws = horizons(spc)$COLEws)

# make a plot -- at what point does a montmorillonite peak suggest elevated COLE?
boxplot(data = splined, 
     COLEws ~ mt_montmorillonite_x_ray, 
     pch = 19, cex = 0.3,
     main = paste("Relationship between Montmorillonite XRD Peak Size\n and Coefficient of Linear Extensibility \n(MLRAs 85, 86A, 86B, 133A, 135A, 135B)"))

# jitter added for clarity, spline is fit through untransformed values
df <- splined[complete.cases(splined), ]

lines(smooth.spline(df$mt_montmorillonite_x_ray, df$COLEws, 
                    df = length(unique(df$mt_montmorillonite_x_ray))), 
      lwd=2, lty=2, col='RED')


