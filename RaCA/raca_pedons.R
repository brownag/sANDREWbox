library(aqp)
library(soilDB)

# get United States FIPS codes
data("state_FIPS_codes")

# iterate over state
res <- lapply(state_FIPS_codes$state_alpha, function(fips) {
  # query RaCA by State
  f <- try(fetchRaCA(state = fips))
  if(inherits(f, 'try-error'))
    return(NULL)
  return(f)
})

# save full result to file as a big list
# save(res, file = "E:/Geodata/soils/raca_all.Rda")

# create a soil profile collection from $pedons
spc <- aqp::union(lapply(res, function(x) if(!is.null(x)) x$pedons))]

# remove invalid profiles
hz.logictest <- aqp::checkHzDepthLogic(spc)
spc$valid <- hz.logictest$valid
spcv <- filter(spc, valid)

# truncate to [5,100]
spcv100 <- suppressWarnings(trunc(spcv, 5, 100))

# calculate completeness
spcv100 <- mutate_profile(spcv100, completeness = sum(hzdepb - hzdept) / 95)

# keep only complete data
raca_5to100 <- filter(spcv100, completeness == 1)

# create a soil profile collection from $sample
spc <- aqp::union(lapply(res, function(x) if(!is.null(x)) x$sample))]

# spatial plot
coordinates(raca_5to100) <- ~ x + y
proj4string(raca_5to100) <- "+proj=longlat +datum=WGS84"

plot(raca_5to100@sp)

# ? ...
