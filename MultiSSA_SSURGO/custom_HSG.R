# custom Hydrologic Soil Group aggregation script
#   using tuolumne county (tuol stan IRWM) extent
#   and "dominant soil condition" logic
# @last updated: 2019/03/15
# @author: andrew brown

library(rgdal)
library(soilDB)
library(tidyverse)

# read the shapefile
mu <- readOGR(dsn = "mupoly_tuol-stan_IWRM.shp")

# construct SQL query to get muaggatt table
q <- paste0("SELECT mukey, muname, hydgrpdcd FROM muaggatt WHERE mukey IN ", 
            format_SQL_in_statement(unique(mu$MUKEY)))
leg <- as_tibble(SDA_query(q)) 
leg2 <- leg %>%
  filter(is.na(hydgrpdcd))

# construct SQL query to send to SDA, using the mukeys to get component-level HSG
q <- paste0("SELECT mukey, cokey, compname, compkind, comppct_r, 
            majcompflag, hydgrp FROM component WHERE mukey IN ", 
            format_SQL_in_statement(unique(leg$mukey)))

# create a tibble with SDA result
rez <- as_tibble(SDA_query(q))

# replace null values with Not Rated (group X)
rez$hydgrp[is.na(rez$hydgrp)] <- "X"

most_limiting_HSG <- function(hydgrp) {
  HSG <- c("D","A/D","C","B","A","X") 
  # in case of a comppct tie -- most limiting wins, 
  # but if tied with NoData "X," then the rating _based on data_ wins
  
  # make sure we aren't missing any groups
  test <- hydgrp[!(hydgrp %in% HSG)]
  if(length(test)) 
    print(test)
  
  foo <- unlist(lapply(as.list(hydgrp), function(x) return(which(x == HSG))))
  if(length(foo))
    return(HSG[min(foo)[1]])
  # if there are no soil components (misc areas removed)... return NoData "X"
  return("X")
}

# calculate conditional sums within groups of mukey and hydgrp
# ignore miscellaneous areas, break ties using standard most limiting rules defined above
foo <- rez %>% 
  group_by(mukey,hydgrp,compkind) %>%
  filter(compkind != "Miscellaneous area") %>%
  summarise(cond_sum = sum(comppct_r)) %>%
  group_by(mukey) %>%
  filter(cond_sum == max(cond_sum)) %>%
  summarise(cHSG = most_limiting_HSG(hydgrp), cHSG_pct = unique(cond_sum))

# make sure every mukey has some sort of a rating
all(unique(mu$mukey) %in% foo$mukey)

# make sure there is only one rating per mukey
nrow(foo) == length(unique(foo$mukey))

mu.custom <- merge(mu, leg, by.x="MUKEY", by.y="mukey", all.x=TRUE)
mu.custom <- merge(mu.custom, as.data.frame(foo), by.x="MUKEY", by.y="mukey", all.x=TRUE, drop=F)

# check for "revisions" to standard muaggatt HSG, and move them over to custom
# these are all soils that were assigned "A/D" HSG, but their HSG calculates as A (no water table?)
custom.idx <- which(mu.custom$hydgrpdcd != mu.custom$cHSG)
mu.custom[custom.idx,"cHSG"] <- mu.custom[custom.idx,]$hydgrpdcd

# check that all of the original assignments (that weren't NA) are retained in custom rating
# i.e. we only filled in records that were NA, we didnt change any official records
all(mu.custom$hydgrpdcd[!is.na(mu.custom$hydgrpdcd)] == mu.custom$cHSG[!is.na(mu.custom$hydgrpdcd)])

# check that the mapunits only comprised of misc areas get an X rating (not NA)
# these are water, dam and mine tailings units: W, 1012, DAM, 999
unique(mu.custom[is.na(mu.custom$cHSG),]$MUSYM)
mu.custom[is.na(mu.custom$cHSG),"cHSG"] <- "X"

# write shapefile to file
writeOGR(mu.custom, dsn=".", layer="mupoly_tuol-stan_customHSG_v2",
        driver="ESRI Shapefile", overwrite_layer = TRUE)

test2 <- readOGR(dsn=".", layer="mupoly_tuol-stan_customHSG")
print(unique(data.frame(musym=mu.custom$MUSYM, new=mu.custom$cHSG,old=test2$cHSG)[which(mu.custom$cHSG != test2$cHSG),]), row.names = FALSE)

# mapunit/components not rated for HSG are written out to file
# this can be helpful if a mapunit is not rating as expected
foo2 <- foo %>%
  filter(hydgrp == 'X')
nr <- unique(rez[rez$mukey %in% foo2$mukey,])
nr <- merge(leg, nr, by="mukey", drop=FALSE)
write.csv(nr, file = "problemHSG_mucomp.csv")

# method for querying SDA to get full extent of the MUKEYs (not limited to IRWM)
# q <- paste0("select G.MupolygonWktWgs84 as geom, mapunit.mukey, muname FROM mapunit
#             CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
#             WHERE mukey IN ",
#             format_SQL_in_statement(leg2$mukey))
# res <- SDA_query(q)
# s <- processSDA_WKT(res)# 
# writeOGR(s, dsn=".", layer="CA109_aggregateHSG_ProblemMapunits", driver="ESRI Shapefile", overwrite_layer=T)
