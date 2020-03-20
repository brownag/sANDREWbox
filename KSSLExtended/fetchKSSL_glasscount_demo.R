# glass content comparison by mlra (all horizons with optical glass counts by 7B1a2)
# demonstration of new extended geochemical data option in fetchKSSL
# andrew brown; 2020/3/19

library(dplyr)
library(aqp)
library(soilDB)

# use lapply to iterate over MLRAs and do fetchKSSL
# with returnGeochemicalData = TRUE the result is a list, not just a SPC
# For each MLRA:
#  - SPC is in the $SPC element
#  - geochem is in $geochem
#  - optical in $optical
#  - xray in $xrd_thermal
mlras <- list('22A','22B','1','3')
f <- lapply(mlras, function(a.mlra) soilDB::fetchKSSL(mlra = a.mlra, returnGeochemicalData = TRUE))

# extract just SPC component from each MLRA and combine them with union
spc <- aqp::union(lapply(f, function(x) x$SPC))

# extract optical table from each MLRA and combine with rbind
optical <- do.call('rbind', lapply(f, function(x) x$optical))

# keep just glass counts
optical <- dplyr::filter(optical, glass_count_method == "7B1a2")

# keep columns that have any values non-NA
optical <- optical[,-which(apply(optical, 2, function(col) all(is.na(col))))]

# calculate the "total glass" portion (i think this works... could add up all relevant fractions too)
optical$total_glass <- 100 - optical$ot_other_glass_count

# inspect "combined" distribution -- results of _individual_ grain count analyses
# (all MLRAs, all soils, all size fractions, not corrected for fine-earth separates)
plot(density(optical$total_glass, na.rm=T, from=0), 
     main="Probability Density Plot of \nTotal Glass % by Grain Count (7B1a2)")

# get just totals, size fractions and labsampnum
just.totals <- optical[,c("labsampnum", "analyzed_size_frac", "total_glass")]

# create lookup table to relate size fractions to sand fractions in SPC horizon table
sizes <- c("0.02-0.05 mm", 
           "0.1-0.25 mm", 
           "0.05-0.1 mm", 
           "0.25-0.5 mm")
size.lut <- list('silt_c_psa','vfs','fs','ms')
names(size.lut) <- sizes

# determine unique pedon_key for pedons with glass count totals
pedon_key.hasdata <- unique(horizons(spc)[which(horizons(spc)$labsampnum %in% just.totals$labsampnum),]$pedon_key)

# subset SPC to just ones with glass totals for at least one horizon
spc.sub <- spc[which(spc$pedon_key %in% pedon_key.hasdata)]

# this routine loops through profiles and horizons
# matches them up with the glass count totals
# corrects glass counts for silt+sand size fractions
tot_glass_labsampnum <- profileApply(spc.sub, function(p) {
  h <- horizons(p)[horizons(p)$labsampnum %in% just.totals$labsampnum,]
  
  res <- data.frame(labsampnum=NA, total_glass_pct=NA)
  
  if(any(p$labsampnum %in% just.totals$labsampnum) & 
     !any(aqp::hzDepthTests(p$hzn_top, p$hzn_bot))) {
    
    # get just the totals for current profile
    sub.totals <- just.totals[just.totals$labsampnum %in% p$labsampnum,]
    
    # iterate over each horizon with glass count data
    # determine total glass sum weighted across size fractions
      props <- as.data.frame(h[,as.character(size.lut)])
      props <- props / rowSums(props)
      props <- cbind(data.frame(labsampnum=h$labsampnum), props)
      
      # determine fractional proportions for each optical mineralogy sample
      sub.totals$frac_prop <- NA
      for(l in h$labsampnum) {
        prop.idx <- which(props$labsampnum == l)
        total.idx <- which(sub.totals$labsampnum == l)
        size.cols <- as.character(size.lut[sub.totals$analyzed_size_frac[total.idx]])
        sub.totals$frac_prop[total.idx] <- as.numeric(props[prop.idx, size.cols])
      }
      
      # scale % glass by proportion of csi+sand fraction
      sub.totals$marginal_product <- sub.totals$total_glass * sub.totals$frac_prop
      
      # this is how to do it without the correction for size fractions --- some will come in >100% glass
      # sub.totals$marginal_product <- sub.totals$total_glass
      
      # inspect
      #print(sub.totals)
      
      # combine results
      res <- do.call('rbind', lapply(split(sub.totals, f=sub.totals$labsampnum), function(x) {
        df <- data.frame(labsampnum=unique(x$labsampnum), total_glass_pct=sum(x$marginal_product))
        rownames(df) <- NULL
        return(df)
      }))
  }
  rownames(res) <- NULL
  return(res)
}, frameify=TRUE)

# total glass counts by labsampnum back into big SPC
spc.sub$total_glass <- tot_glass_labsampnum[match(spc.sub$labsampnum, 
                                                  tot_glass_labsampnum$labsampnum),]$total_glass_pct

# look at total glass (note these are the scaled weighted averages)
plot(density(spc.sub$total_glass, na.rm=T))

# compare quantiles for the MLRAs (across all soils/depths)
res <- lapply(mlras, function(m) {
  prb <- c(0,0.05,0.25,0.5,0.75,0.95,1)
  dat <- spc.sub[spc.sub$mlra == m,]$total_glass
  r <- quantile(dat, probs=prb, na.rm=T)
  cbind(t(r), data.frame(n=sum(!is.na(dat))))
})
names(res) <- mlras

# make a table
round(do.call("rbind", res),1)

# make a boxplot
boxplot(data=horizons(spc.sub), 
        total_glass ~ denormalize(spc.sub, "mlra"),
        xlab="MLRA", ylab="Total Glass % Weighted Sum (csi to ms)")
