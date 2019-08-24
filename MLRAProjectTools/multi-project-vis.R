#####
# multi-project-vis.R
#  @purpose: view legend level and spatial extent information for one or more MLRA projects
#  @author: andrew brown
#  @date: 2019/03/19
#  @last updated: 2019/06/26
#  @instructions: 
#   1. load desired projects into NASIS selected set. optional: load TUD pedons into selected set
#   2. choose a filter to subset NASIS projects (e.g. MLRA, taxonname, etc)
#   3. set output file name
#####
# SETUP
#####
# set your working directory for output file
setwd("S:/NRCS/430 SOI Soil Survey/430-05 Soil Survey Area Case Files/Projects/EVAL projects/2019 Projects/Sierra_EVAL")
#setwd("E:/CA649/")

# filter on project name
p.pattern <- "Sierra"

# filter on MLRA shapefile (for extent map)
mlra.pattern <- "18"

# project extent map (additional constraints on extent)
project.extent.file <- "" #"E:/CA649/ca649_mvo_FY2020.shp"

output.utm.zone <- "10"

#output file prefix
out.file.prefix <- "FY19_Sierra_EVAL"

# name of shapefile output
shp.file.name <- "FY19_Sierra_EVAL" # no .shp extension
#####

library(aqp)
library(soilDB)
library(rgdal)
library(rgeos)
library(ape)
library(cluster)
library(latticeExtra)
library(plotrix)

get_nasis_projects <- function() {
    if(!requireNamespace('RODBC'))
      stop('please install the `RODBC` package', call.=FALSE)
    q <- "SELECT projectiid, uprojectid, projectname, projectdesc FROM project_View_1;"
    channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
    d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
    RODBC::odbcClose(channel)
    return(d)
}

# load projects out of nasis selected set
p <-  get_nasis_projects()

# use NASIS web reports interface to get additional data for project
projects <- soilDB::fetchNASISWebReport(projectname = p$projectname[grepl(p$projectname, pattern=p.pattern)])
projects$spc$compnamepct <- paste0(projects$spc$compname, " (",projects$spc$comppct_r,"%)")

# visualize the component horizon data in all mapunits associated with one or more projects as identified above
par(mar=c(0.2,0.2,3,0.2))
groupedProfilePlot(projects$spc, groups='dmudesc', 
                   label = 'compnamepct', id.style="side",
                   color = 'claytotal_r')

# load pedons out of nasis selected set
f <- fetchNASIS()
f <- f[which(!is.na(f$x_std)),]
coordinates(f) <- ~ x_std + y_std
proj4string(f) <- "+proj=longlat +datum=WGS84"

# visualize horizon data in TUD pedons 
par(mar=c(0.5,1.5,4.5,3))
groupedProfilePlot(f, groups='taxonname', 
                   label = 'pedon_id', id.style="side", col.legend.cex=1.1,
                   color = 'texcl', axis.line.offset=-1.5, cex.names=1.1)

# use national mapunit symbol to make MUKEY lookup table
nmusyms <- unique(projects$mapunit$nationalmusym)
q <- paste0('select mukey, muname, nationalmusym, musym from mapunit where nationalmusym IN ', 
            format_SQL_in_statement(nmusyms),";")
mukey.lut.res <- SDA_query(q)
mukey.lut <- mukey.lut.res$mukey
names(mukey.lut) <- mukey.lut.res$nationalmusym
mukey.lut.rev <- names(mukey.lut)
names(mukey.lut.rev) <- as.character(mukey.lut)

mu.table <- mukey.lut.res[order(mukey.lut.res$muname),]
write.csv(mu.table, file=paste0(out.file.prefix, "_MU-TABLE.csv"))
knitr::kable(mu.table)

# do SDA spatial query using MUKEY to create WKT 
q <- paste0("select G.MupolygonWktWgs84 as geom, mu.mukey, muname, musym, nationalmusym, l.areasymbol
  FROM mapunit AS mu CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mu.mukey) as G
  INNER JOIN legend l ON mu.lkey = l.lkey
  WHERE mukey IN ",format_SQL_in_statement(mukey.lut),";")
res <- SDA_query(q)
s <- processSDA_WKT(res)
# save(s, file='mupoly_bak.Rda')
# load('mupoly_bak.Rda')

# if an additional extent file was supplied by user, use that to filter further
if(!is.null(project.extent.file) & project.extent.file != '') {
  proj.extent <- readOGR(project.extent.file)
  proje <- spTransform(proj.extent, CRS(proj4string(s)))
  s.sub <- s[!is.na(over(s, proje)[,1]),]
} else {
  s.sub <- s
}

# do spatial overlay of pedons on polygons
f$musym <- over(f@sp, s.sub)$musym
f <- f[which(!is.na(f$musym)), ]

mlra <- readOGR('F:/Geodata/soils/MLRA_Boundaries_CA/mlra_a_ca.shp')
mlra <- spTransform(mlra, CRS(proj4string(s.sub)))
mlra.sub <- mlra[grepl(mlra$MLRARSYM, pattern=mlra.pattern),]
par(mar=c(0.1,0.1,0.1,0.1))
plot(s)
plot(s.sub, border="black", col="red", add=T)
plot(mlra.sub, border="blue",add=T)
legend('bottomleft',lty=c(1,NA,1), lwd=c(2,NA,2),
       legend=c("MUSYM Extent","Project Extent","MLRA Boundary"),
       col=c("BLACK","RED","BLUE"), fill=c(NA, "RED", NA), cex=1.2)
#plot(FedData::polygon_from_extent(s), add=T, border="RED")

# acres by musym in area
s.utm <- spTransform(s, CRS(paste0("+proj=utm +zone=",output.utm.zone)))
s$acres_int <- round(gArea(s.utm, TRUE) / 4046.86)
s.sub.utm <- spTransform(s.sub, CRS(paste0("+proj=utm +zone=",output.utm.zone)))
s.sub$acres_int <- round(gArea(s.sub.utm, TRUE) / 4046.86)
lapply(split(slot(s.sub, 'data'), f = s.sub$musym), function(x) sum(x$acres_int))

print(paste0("Total spatial acres in subset: ", sum(s.sub$acres_int)))
print(paste0("Total spatial acres in full extent of project mapunits: ", sum(s$acres_int)))

# print out total acres by NASIS project
lapply(split(projects$mapunit, projects$mapunit$projectname), function(x) sum(x$muacres))

rgdal::writeOGR(s.sub.utm, dsn=".", layer=shp.file.name, driver="ESRI Shapefile", overwrite_layer = TRUE)

