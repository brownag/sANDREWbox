library(aqp)
library(soilDB)
library(sf)

# get a SoilProfileCollection of all components in CA630
f <- fetchSDA(WHERE = "areasymbol = 'CA630'", duplicates=TRUE)

# use MUKEY to download the SDA spatial data (takes a little bit)
# optionally load it from file -- though you will 
spatial <- fetchSDA_spatial(unique(f$mukey), chunk.size = 1)

# use mukey, a site-level attribute, to split components into mapunits
f.mukey <- aqp::split(f, 'mukey')

# determine mapunit dominant condition for a site-level attribute attr
#   where dmu is a SPC where all profiles have identical MUKEY  
dominant.condition <- function(dmu, attr) {
  # get needed vars
  mukey <- unique(dmu$mukey)
  d.attr <- dmu[[attr]]
  d.comppct <-  dmu[["comppct_r"]]
  
  # sum up comppct for each level of attr
  res <- aggregate(d.comppct, list(d.attr), sum)
  if(!nrow(res))
    res <- data.frame(a=character(0), b=character(0))
  
  # select the first occurence of maximum value
  #  might not be stable if two attr have same percentage
  suppressWarnings(res <- res[which(res$x == max(res$x, na.rm=TRUE))[1],])
  names(res) <- c("dominant_condition", "dominant_condition_pct")
  
  # create data.frame output
  return(data.frame(mukey=mukey, res))
}

# example usage
dominant.condition(f.mukey[[1]], "ecoclassid") 

# calculate for all MUKEYs in CA630
dominant.condition.table <- do.call('rbind', lapply(f.mukey, dominant.condition, "ecoclassid"))

# join in identifying info
q <-  paste0("select * from mapunit where mukey in ",
             format_SQL_in_statement(dominant.condition.table$mukey))
legendinfo <- SDA_query(q)

# add legend info to spatial layer
spatial_sf <- st_as_sf(spatial)
spatial_sf <- merge(spatial_sf, legendinfo, sort=FALSE)

# add dominant condition info to spatial layer
spatial_sf <- merge(spatial_sf, dominant.condition.table, sort=FALSE)

# output
par(mar=c(0,0,0,0))
plot(spatial_sf[,'dominant_condition'], lty=0)
st_write(spatial_sf, "CA630_ES_dominant_condition.shp")
