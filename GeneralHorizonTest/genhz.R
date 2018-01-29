## genhz rule tester

library(soilDB)
f <- fetchNASIS()
#f.bak <- f
setwd("report_tests/region2/shiny-pedon-summary")
source('config.R')
f$genhz <- generalize.hz(f$hzname, gen.hz.rules[[1]]$n, gen.hz.rules[[1]]$p)

table(f$genhz,f$hzname)
