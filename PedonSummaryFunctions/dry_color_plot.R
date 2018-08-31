library(soilDB)
library(sharpshootR)
f <- fetchNASIS_pedons()
aggregateColorPlot(aggregateColor(f, groups = 'genhz', col = 'dry_soil_color'), label.font = 1, label.cex = 0.95, print.n.hz = TRUE)
