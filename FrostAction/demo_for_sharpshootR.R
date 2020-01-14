library(sharpshootR)
library(soilDB)

getData <- function(station_id) {
  print(station_id)
  x <- try(CDECquery(id = station_id, sensor=30, interval='D', start='1900-01-01', end='2020-12-31'))
  if(class(x) == 'try-error')
    return(NULL)
  return(x)
}

#Grab a single station's data
foo <- getData("RND")

plot(data=foo, value ~ datetime, pch=".", main="Mean Daily Air Temperature Timeseries\nROUND MOUNTAIN (CDF) -  MCCLOUD, CA",
     ylab="Mean Daily Air Temperature, degrees F", xlab="Time")
abline(h=32, lwd=2, col="RED")

.SimpleTemperatureFilter <- function(y, nodata.value=-9999, bad.data.threshold=-50, quantile.threshold = 0.999) {
  #Filter known NoData values
  y[y == nodata.value] <- NA
  
  #Throw out physically unreasonable values (typically strongly negative values for CDEC)
  bad.idx <- which(y < bad.data.threshold)
  y[bad.idx] <- NA
  
  #We will use the derivative of the time series to find discontinuous/anomalous extreme values
  # but sometimes bad data occur adjacent to NoData. Encode all NAs with alternating 999 / -999 for diff()
  fix <- rep(c(999,-999), round((length(y[is.na(y)]) + 1) / 2))
  fix.idx <- which(is.na(y))
  
  #calculate the absolute change in temperature between adjacent points; duplicate first data point 
  # to preserve indexing relative to original time series (diff between duplicate and true first point equals zero)
  da.y <- abs(diff(c(y[1], abs(y))))
  
  if(length(fix.idx))
    da.y <- da.y[-fix.idx]
  
  thresh <- quantile(da.y, probs=quantile.threshold[1], na.rm=T)
  bad.idx <- which(da.y > thresh)
  y[bad.idx] <- NA
  
  thresh2 <- quantile(y, probs=c(1 - quantile.threshold, quantile.threshold), na.rm=T) 
  y[y <= thresh2[1] | y >= thresh2[2]] <- NA
  
  return(y)
}

FreezingIndex <- function(d, frostTemp, p, FUN = .SimpleTemperatureFilter, ...) {
  d.y <- do.call('rbind',
          lapply(split(d, f = d$year), 
            function(i) { 
                a.y <- alignDOY(i$datetime, i$value)
                ffp <- frostFreePeriod(i)
                a.y <- .SimpleTemperatureFilter(a.y)
                a.y[a.y > frostTemp] <- NA
                a.y <- -(a.y - frostTemp)
                if(!is.null(ffp))
                  return(sum(a.y, na.rm=T))
                return(NA)
            }))
  n.yrs <- nrow(d.y)
  q <- quantile(d.y[,1], probs=p, na.rm=T)[1]
  return(q)
}

DesignFreezingIndex <- function(d, ...) {
  frostTemp <- 32
  p <- 0.9 #the "design" freezing index is the Freezing Index 90th percentile
  return(FreezingIndex(d, frostTemp, p, ...))
}

DesignFreezingIndex(foo)
