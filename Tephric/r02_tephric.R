library(aqp)
library(soilDB)

# load a regional pedon file
load("E:/r02_pedons.RData")

# get soils with tephra, volcanic ash, cinders or volcanic bombs in parent material
r02_tephra <- r02_pedons[which(grepl(r02_pedons$pmkind, 
                                     pattern="tephra|volcanic ash|cinders|volcanic bombs", 
                                     ignore.case = TRUE)),]

# remove pedons without WGS84 lat/long
r02_tephra.bad.idx <- which(is.na(r02_tephra$x_std) | is.na(r02_tephra$y_std))
r02_tephra <- r02_tephra[-r02_tephra.bad.idx,]

coordinates(r02_tephra) <- ~ x_std + y_std
proj4string(r02_tephra) <- "+proj=longlat +datum=WGS84"

maps::map(database = "world")
points(r02_tephra@sp, pch="*", col="RED")
message(paste0("Found ", length(r02_tephra)," R02 pedons with tephra, volcanic ash, cinders or volcanic bombs in parent material"))

# create list of possible tephra soils in RO2
paste0(as.character(r02_tephra$upedonid), collapse=",")