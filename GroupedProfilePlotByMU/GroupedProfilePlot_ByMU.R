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
###
###
###
workdir <- "S:\\NRCS\\Archive_Andrew_Brown\\Scripts\\GroupedProfilePlotByMU\\"
outdir <- paste0(workdir,"output\\")

mu_source <- 'L:/NRCS/MLRAShared/CA630/FG_CA630_OFFICIAL.gdb'
mu_layer <- 'ca630_a'
bdy_layer <- 'ca630_b'

targetMU='603.|604.|8110'
###
###
###

setwd(workdir)

# setup plotting style for later
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

# Get map unit data
mu <- readOGR(dsn = mu_source, layer = mu_layer, stringsAsFactors=FALSE)
surveybdy <- readOGR(dsn = mu_source, layer = bdy_layer, stringsAsFactors=FALSE)

# get pedon data
x <- fetchNASIS()
x$taxonname = toupper(x$taxonname)
par(mfrow=c(1,2))

# keep only pedons with real coordinates
good.idx <- which(!is.na(x$x_std))
x <- x[good.idx, ]

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

#get ready to write output 
setwd(outdir)

#Count number of pedons per map unit and per map unit component

if(!dir.exists("stats"))
  dir.create("stats")

# #gives # of pedons per mapunit
write.csv(file='stats\\pedons_perMU.csv',x=aggregate(data.frame(n=x.sub$musym),by=list(MUSYM=x.sub$musym),FUN=length))
#gives # of pedons per mapunit per component
write.csv(file='stats\\pedons_perMU_perCOMP.csv',x=aggregate(data.frame(n=x.sub$musym),by=list(MUSYM=x.sub$musym, COMPONENT=x.sub$taxonname),FUN=length))
#gives # of pedons per ecosite
write.csv(file='stats\\pedons_perECOS.csv',x=aggregate(data.frame(n=x.sub$musym),by=list(ECOSITE=x.sub$ecositeid),FUN=length))
#gives # of pedons per mapunit per ecosite
write.csv(file='stats\\pedons_perMU_perECOS.csv',x=aggregate(data.frame(n=x.sub$musym),by=list(MUSYM=x.sub$musym, ECOSITE=x.sub$ecositeid),FUN=length))


# Plot ordering by taxonname
par(mar=c(1,1,5,1))#,mfrow=c(4,1))
mufact=factor(x.sub$musym)
for(f in levels(mufact)) {
  sx.mu=x.mu[which(x.mu$musym == f)]
  
  if(!dir.exists("csv"))
    dir.create("csv")
  
  write.csv(site(sx.mu),file = paste0("./csv/",f,".csv"))
  
  if(!dir.exists("pedonlists"))
    dir.create("pedonlists")
  
  write(paste0(site(sx.mu)$pedon_id,collapse=","),sep="",file = paste0("./pedonlists/",f,".csv"))
  
  if(nrow(sx.mu) > 0) {
    if(f != "1031?") { 
      #TODO: do a proper check for non-canonical MUSYMs that contain illegal characters for filenames... 
      #      but not as restrictive as requiring a numeric musym
      pdf(file = paste0(f,".pdf"),width=11,height=8.5) 
      groupedProfilePlot(sx.mu, groups = 'taxonname', alt.label='ecositeid', alt.label.col='black', label='pedon_id', print.id = TRUE, id.style = 'side', cex.id=0.6, cex.names=0.6, y.offset=7, axis.line.offset=-2.5, group.name.cex=0.5, group.name.offset = -6, color='clay')
      title(paste(f,'Grouped Profile Plot\n\n'))
      dev.off()
    }
  } else {
    print(paste0("No pedons in MUSYM ",f,"!"))
  }
}

