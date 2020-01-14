# ca729 component QC
library(soilDB)

# find all pedons with kind OSD pedon and TUD pedon
pedon <- fetchNASIS_pedons(stringsAsFactors = FALSE)
osdtud <- subsetProfiles(pedon, s = "pedontype == 'OSD pedon' | pedontype ==  'TUD pedon'")

# compare OSD/TUD to copedon populated in component
component <- fetchNASIS_components(stringsAsFactors = FALSE)

get_rv_pedon <- function(coiid) {
  q <- paste0("SELECT * FROM copedon WHERE copedon.coiidref IN ", 
              format_SQL_in_statement(coiid)," AND copedon.rvindicator = 1;")
  channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  return(d)
}

rv_pedons <- get_rv_pedon(component$coiid)
not.tagged.idx <- which(!(site(osdtud)$peiid %in% rv_pedons$peiidref))

### OUTPUT: list of pedons with purpose OSD or TUD pedon that are not marked as RV pedon
paste0(osdtud$pedon_id[not.tagged.idx], collapse=',')

#---------------------------------------------------
#---------------------------------------------------
#---------------------------------------------------

# diagnostic features -- RV 
diag <- diagnostic_hz(component)

# any missing top depth RV?
any(is.na(diag$featdept_r))

# any missing bottom depth RV (not discontinuity or contact)?
any(is.na(diag$featdepb_r) & !grepl(diag$featkind, pattern="discontinuity|abrupt"))

# any lithic contact bottom depth not equal to 200?
diag[!is.na(diag$featdepb_r) & (diag$featdepb_r != 200) & grepl(diag$featkind, pattern="$lithic contact"),]

# any paralithic contact bottom depth not equal to tdepth+25 (or 26 to account for bad rounding?)
diag[!is.na(diag$featdepb_r) & 
       (diag$featdepb_r != diag$featdept_r + 25) &
       (diag$featdepb_r != diag$featdept_r + 26) & 
       grepl(diag$featkind, pattern="paralithic contact"),]

# diagnostic features --  low and high not populated but compkind is series
coseries <- component[component$compkind == "series",]
get_component_diaghz_from_NASIS_db_v2 <- function (SS = TRUE) 
{
  if (!requireNamespace("RODBC")) 
    stop("please install the `RODBC` package", call. = FALSE)
  channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
  q <- "SELECT coiidref as coiid, featkind, featdept_l, featdept_r, featdept_h, featdepb_l, featdepb_r, featdepb_h \n  FROM \n  codiagfeatures_View_1 AS cdf\n  ORDER BY cdf.coiidref, cdf.featdept_r;"
  if (SS == FALSE) {
    q <- gsub(pattern = "_View_1", replacement = "", x = q, 
              fixed = TRUE)
  }
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  d <- uncode(d)
}
diag.series <- get_component_diaghz_from_NASIS_db_v2()
any(is.na(diag.series$featdept_l) | is.na(diag.series$featdept_h))

# check higher taxa with L and H

# coverkind -- run through the key, compare stored versus calculated

# geomorph -- ensure that flattening of geomorph on RV results in correct number of records (1:1 with component)
get_component_cogeomorph_data_from_NASIS_db_2 <- function (SS = TRUE) 
{
  if (!requireNamespace("RODBC")) 
    stop("please install the `RODBC` package", call. = FALSE)
  q <- "SELECT cogeo.coiidref as coiid, cogeo.geomfmod, geomorfeat.geomfname, cogeo.geomfeatid, 
          cogeo.existsonfeat, cogeo.geomfiidref, cogeo.cogeomdiid, lower(geomorfeattype.geomftname) as geomftname,
          cogeo.rvindicator
        FROM component_View_1 AS co
        INNER JOIN cogeomordesc_View_1 AS cogeo ON co.coiid = cogeo.coiidref
        INNER JOIN geomorfeat ON geomorfeat.geomfiid = cogeo.geomfiidref
        INNER JOIN geomorfeattype ON geomorfeattype.geomftiid = geomorfeat.geomftiidref
        ORDER BY coiid, geomfeatid ASC;"
  channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
  if (SS == FALSE) {
    q <- gsub(pattern = "_View_1", replacement = "", x = q, 
              fixed = TRUE)
  }
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  return(d)
}
ca729_landforms <- uncode(get_component_cogeomorph_data_from_NASIS_db_2())
ca729.landform.bycomp <- split(ca729_landforms,ca729_landforms$coiid)
multirvland <- (unlist(lapply(ca729.landform.bycomp, function(x) {
  return(sum(x[x$geomftname=="landform",]$rvindicator) > 1 | 
           sum(x[x$geomftname=="landscape",]$rvindicator) > 1)
})))
paste0(names(ca729.landform.bycomp)[multirvland], collapse=",")

# geomorph -- ensure all components have RV landscape and landform
onervland <- (unlist(lapply(ca729.landform.bycomp, function(x) {
  return((sum(x[x$geomftname=="landform",]$rvindicator) != 1 & all(is.na(x[x$geomftname=="landform",]$existsonfeat))) | 
           sum(x[x$geomftname=="landscape",]$rvindicator) != 1)
})))

all(onervland)

# only the water mapunit is missing a landscape
# there are 44 components with 2 RV landform, but they are correctly nested
paste0(names(ca729.landform.bycomp)[onervland], collapse=",")

# subsidence -- populated for all relevant components (all non miscellaneous areas?)
get_component_subsidence_from_NASIS_db <- function () {
  if (!requireNamespace("RODBC")) 
    stop("please install the `RODBC` package", call. = FALSE)
  channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
  q <- "SELECT dmuiidref, coiid, initsub_l, initsub_r, initsub_h, totalsub_l, totalsub_r, totalsub_h
        FROM component
        ORDER BY coiid;"
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  d <- uncode(d)
}
subsidence <- get_component_subsidence_from_NASIS_db()
subsidence <- subset(subsidence, subsidence$dmuiidref %in% component$dmuiid)
s.subsidence <- split(subsidence, subsidence$coiid)
res.subsidence <- unlist(lapply(s.subsidence, function(x) {
  return(!all(apply(x, 2, function(z) return(all(!is.na(z))))))
}))
paste0(names(res.subsidence)[res.subsidence & component$compkind == "miscellaneous area"], collapse=",")

# surface fragments  -- check number of kinds within component; flag n>2 then n>1
get_componentSURFFRAG_data_NASIS <- function() {
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  q <- "SELECT nationalmusym, dmuiid, coiid, compname, compkind, localphase, areasymbol, musym, muname, mustatus, 
  sfragcov_l, sfragcov_r, sfragcov_h, 
  sfragsize_l, sfragsize_r, sfragsize_h, 
  sfragkind, sfragshp, sfraground, sfraghard FROM legend AS l
  INNER JOIN area AS ar ON ar.areaiid = l.areaiidref
  INNER JOIN lmapunit AS lmu ON lmu.liidref = l.liid
  INNER JOIN mapunit AS mu ON mu.muiid = lmu.muiidref
  INNER JOIN correlation AS cor ON cor.muiidref = mu.muiid
  INNER JOIN datamapunit AS dmu ON dmu.dmuiid = cor.dmuiidref
  INNER JOIN component AS co ON co.dmuiidref = dmu.dmuiid
  LEFT JOIN cosurffrags AS csf ON co.coiid = csf.coiidref
  WHERE cor.repdmu = 1 AND lmu.mustatus != 4"
  channel <- RODBC::odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel)
  return(d)
}

ca729.surffrag <- uncode(get_componentSURFFRAG_data_NASIS())
ca729.surffrag.split <- split(ca729.surffrag, f=ca729.surffrag$coiid)
rez <- unlist(lapply(ca729.surffrag.split, function(x) {
  sum(length(unique(x$sfragkind)))
}))
any(rez > 1)

# horizon designations -  replaced H horizon designations in all components that are compkind series
coseries <- component[component$compkind == "series",]
any(grepl(coseries$hzname, pattern="H"))


