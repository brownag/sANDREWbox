# load required packages
library(lattice)
library(soilDB)
library(aqp)
library(rgdal)
library(plyr)
library(sp)
library(cluster)
library(ape)
library(RColorBrewer)
library(latticeExtra)
library(plotrix)
library(classInt)
library(sharpshootR)
library(raster)
###
###
###
workdir <- "E:\\workspace\\"
outdir <- "output\\"
targetMU='603.|604.|8110'
###
###
###

setwd(workdir)

# setup plotting style for later
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

# Get map unit data
mu <- readOGR(dsn = 'L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb', layer = 'ca630_a', stringsAsFactors=FALSE)
surveybdy <- readOGR(dsn = 'L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb', layer = 'ca630_b', stringsAsFactors=FALSE)

# get pedon data
x <- fetchNASIS()
x$taxonname = toupper(x$taxonname)
par(mfrow=c(1,2))

# keep only pedons with real coordinates
good.idx <- which(!is.na(x$x_std))
x <- x[good.idx, ]
n.idx = which(!is.na(notes$Easting))
n=notes[n.idx,]

#initalize spatial object
coordinates(x) <- ~ x_std + y_std

# set spatial reference
proj4string(x) <- '+proj=longlat +datum=NAD83'

# extract spatial data + site level attributes for each pedon
x.sub <- as(x, 'SpatialPointsDataFrame')

# transform to CRS of mus
x.sub <- spTransform(x.sub, CRS(proj4string(mu)))

# subset seleted map unit
idx <- grep(targetMU, mu$MUSYM, ignore.case = TRUE)
s.sub <- mu[idx , ]

# copy MUSYM to pedons using spatial overlay
x.sub$musym <- over(x.sub, s.sub)$MUSYM

#title(targetMU)
#plot(surveybdy)
#plot(s.sub,add=TRUE)

x.sub=x.sub[!is.na(x.sub$musym),] #kicks out NA (filtered) records

#calculate some acreage information and print
aoi_ac=floor(sum(s.sub$Shape_Area/4046.856))
tot_ac=floor(sum(mu$Shape_Area/4046.856))
print(paste('A total of',length(x.sub$musym), 'out of', length(x), 'pedons fall within the boundaries of the area of interest'))
print(paste0("The area of interest spans ",aoi_ac," acres. This comprises ",floor(aoi_ac/tot_ac*100)," percent of the ",tot_ac," acres in the CA630 survey area"))
print(paste0("On average there are ",round(length(x.sub$musym)/aoi_ac*1000,2)," pedons per 1000 acres in the area of interest."))

#remove NAs
idx <- which(!is.na(x.sub$musym))
y <- x.sub[idx, ]

# get profiles in x whose peiid are in subset y
idx <- which(profile_id(x) %in% y$peiid)
x.mu <- x[idx, ]
x.mu$musym=y$musym

setwd(outdir)
# Plot by ecosite
#First get ecosites that are used in AOI
mastrast=readAll(raster('E:/geodata/ca630/soil_temperature/spatial_data/mast-model.tif'))
mastunc=readAll(raster('E:/geodata/ca630/soil_temperature/spatial_data/mast-model-mesic_thermic-uncertainty.tif'))
esfact=factor(x.mu$ecositeid)
x.mu$modelmast=extract(x = mastrast,y = data.frame(x=x.mu$utmeasting,y=x.mu$utmnorthing))
x.mu$modelmast_buf=lapply(extract(x = mastrast,y = data.frame(x=x.mu$utmeasting,y=x.mu$utmnorthing),buffer=50),FUN=mean,na.rm=T)
x.mu$modelmast_buf2=extract(x = mastrast,y = data.frame(x=x.mu$utmeasting,y=x.mu$utmnorthing),buffer=200)
x.mu$modelunc=extract(x = mastunc,y = data.frame(x=x.mu$utmeasting,y=x.mu$utmnorthing))
x.mu$ismesic=(x.mu$modelmast<=15)
x.mu$isuncertain=(x.mu$modelunc==1)

for(f in levels(esfact)) {
  sx.es=x.mu[which(x.mu$ecositeid == f)]
  if(nrow(sx.es) > 0) {
    
    par(mfrow=c(1,1))
    pdf(file = paste0(f,"_CA630_map.pdf"),width=11,height=8.5) 
    plot(mastrast)
    plot(surveybdy,add=T)
    points(data.frame(x=sx.es$utmeasting,y=sx.es$utmnorthing))
    title(paste0("Distribution of ",f," sites in CA630 survey area.\nMAST values in degrees C."))
    dev.off()    
    
    par(mfrow=c(1,1))
    mast_nona=na.omit(unlist(sx.es$modelmast_buf2))
    if(is.numeric(mast_nona) && length(mast_nona) >= 2) {
      pdf(file = paste0(f,"_CA630_density.pdf"),width=11,height=8.5) 
      d<-density(mast_nona)
      plot(d,main=paste0("Kernel Density of MAST model values extracted from ",f," sites"))
      polygon(d,col="blue",border="black")
      dev.off()      
    } else {
      print(paste0("Not enough values to make kernel Density of MAST model values extracted for ",f," sites"))
    }
    
    pdf(file = paste0(f,"_CA630.pdf"),width=11,height=8.5) 
    groupedProfilePlot(sx.es, groups = 'temp_class', alt.label='taxonname', alt.label.col='black', label='pedon_id', print.id = TRUE, id.style = 'side', cex.id=0.6, cex.names=0.6, y.offset=7, axis.line.offset=-2.5, group.name.cex=0.5, group.name.offset = -6, color='clay')
    #plotSPC(sx.mu, groups = 'taxonname', plot.order=order(sx.mu$slope_field), label='pedon_id', cex.id=0.7, cex.names=0.7, col.label=' ', width=0.15, y.offset=0, id.style='side', x.idx.offset=0.2, shrink=FALSE, shrink.cutoff=1)
    title(paste(f,'Grouped Profile Plot - Mesic-Thermic from NASIS Pedon Taxonomy table\n\n'))
    dev.off()
    
    pdf(file = paste0(f,"_CA630_ismesic.pdf"),width=11,height=8.5) 
    groupedProfilePlot(sx.es, groups = 'ismesic', alt.label='taxonname', alt.label.col='black', label='pedon_id', print.id = TRUE, id.style = 'side', cex.id=0.6, cex.names=0.6, y.offset=7, axis.line.offset=-2.5, group.name.cex=0.5, group.name.offset = -6, color='clay')
    #plotSPC(sx.mu, groups = 'taxonname', plot.order=order(sx.mu$slope_field), label='pedon_id', cex.id=0.7, cex.names=0.7, col.label=' ', width=0.15, y.offset=0, id.style='side', x.idx.offset=0.2, shrink=FALSE, shrink.cutoff=1)
    title(paste(f,'Grouped Profile Plot - MAST Mesic-Thermic\n\n'))
    dev.off()
    
    pdf(file = paste0(f,"_CA630_mestherm_unc.pdf"),width=11,height=8.5) 
    groupedProfilePlot(sx.es, groups = 'isuncertain', alt.label='taxonname', alt.label.col='black', label='pedon_id', print.id = TRUE, id.style = 'side', cex.id=0.6, cex.names=0.6, y.offset=7, axis.line.offset=-2.5, group.name.cex=0.5, group.name.offset = -6, color='clay')
    #plotSPC(sx.mu, groups = 'taxonname', plot.order=order(sx.mu$slope_field), label='pedon_id', cex.id=0.7, cex.names=0.7, col.label=' ', width=0.15, y.offset=0, id.style='side', x.idx.offset=0.2, shrink=FALSE, shrink.cutoff=1)
    title(paste(f,'Grouped Profile Plot - Mesic-Thermic Uncertainty\n\n'))
    dev.off()
  }