#color table, albedo prediction demo
# uses all color observations from CA630 and pairs dry and moist colors by horizon
#   TODO: surface horizons only
#   TODO: filter on broken face/crushed (does anyone other than Andy and I do the crushed?)
# applies the universal albedo prediction equation albedo = 0.069*color_value - 0.114 to both moist and dry albedo (assumes crushed and smoothed colors)
# then, uses the correction factor to reflect a 45% reduction of albedo for moist color values, relative to dry
# 
# also, tries to independently predict a correction factor for dry -> moist conversion from CA630 data 
library(soilDB)
source("PedonSummaryFunctions/pedon_summary_functions.R")

get_colors_from_NASIS_db_nosimplify <- function() {
    if (!requireNamespace("RODBC")) 
      stop("please install the `RODBC` package", call. = FALSE)
    q <- "SELECT peiid, phiid, ms.ChoiceLabel AS colormoistst, colorpct as pct, mh.ChoiceName AS colorhue, mv.ChoiceName AS colorvalue, mc.ChoiceName AS colorchroma\nFROM\n  pedon_View_1 INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref\n  INNER JOIN phcolor_View_1 ON phorizon_View_1.phiid = phcolor_View_1.phiidref\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1242) AS mh ON phcolor_View_1.colorhue = mh.ChoiceValue\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1244) AS mv ON phcolor_View_1.colorvalue = mv.ChoiceValue\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1241) AS mc ON phcolor_View_1.colorchroma = mc.ChoiceValue\n  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1243) AS ms ON phcolor_View_1.colormoistst = ms.ChoiceValue\n  ORDER BY phiid, colormoistst;"
    channel <- RODBC::odbcDriverConnect(connection = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
    d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
    RODBC::odbcClose(channel)
    return(d)
}

df <- get_colors_from_NASIS_db_nosimplify()
df.dry <- df[which(df$colormoistst == 'Dry'),]
df.moi <- df[which(df$colormoistst == 'Moist'),]
df.dry$albedo <- 0.069*df.dry$colorvalue - 0.114
df.moi$albedo <- 0.069*df.moi$colorvalue - 0.114
df.moi$albedo_frommoist <- df.moi$albedo / 0.55
df2 <- merge(df.dry, df.moi, by="phiid")
pedons <- fetchNASIS()
horizons(pedons) <- merge(horizons(pedons), df2, by="phiid")

albedo.moist <- unlist(profileApply(pedons, function(x) {
  h.idx <- intersectHorizon(x, getMineralSoilSurfaceDepth(x)+1)
  horizons(x)[horizons(x)$phiid==h.idx,]$albedo.y
}, simplify = F))

albedo.frommoist <- unlist(profileApply(pedons, function(x) {
  h.idx <- intersectHorizon(x, getMineralSoilSurfaceDepth(x)+1)
  horizons(x)[horizons(x)$phiid==h.idx,]$albedo_frommoist
}, simplify = F))

albedo.dry <- unlist(profileApply(pedons, function(x) {
  h.idx <- intersectHorizon(x, getMineralSoilSurfaceDepth(x)+1)
  horizons(x)[horizons(x)$phiid==h.idx,]$albedo.x
}, simplify = F))

plot(jitter(albedo.dry,1), jitter(albedo.frommoist,1),xlim=c(0, 0.5),ylim=c(0, 0.5),pch=1) 
# plot surface horizon [albedo dry] versus [albedo dry (calculated from moist)]
points(jitter(albedo.dry,1), jitter(albedo.moist,1),xlim=c(0, 0.5),ylim=c(0, 0.5),pch="*") 
# plot all horizons

model.dryvmoist.obs <- lm(albedo.dry ~ albedo.moist)
model.dryvmoist.pred <- lm(albedo.dry ~ albedo.frommoist)

abline(model.dryvmoist.obs)
abline(model.dryvmoist.pred)

summary(model.dryvmoist.obs)
summary(model.dryvmoist.pred)

#anova(model.dryvmoist.obs, model.dryvmoist.pred) #cant do this, models have same number of DF duh

#glm test -- asking the question: "does it matter if you estimate the dry albedo from the moist color value?"
df.long <- data.frame(dry=c(albedo.dry, albedo.dry), moist=c(albedo.moist, albedo.frommoist), met=c(rep("Observed", length(albedo.moist)), rep("Calculated", length(albedo.frommoist))))
g1 <- glm(data=df.long, dry~moist+met)
summary(g1)
#answer is yes. 

#OK. so the method for determining color is significant. 
# new question is: can we use this info to build a better model that adds additional correction for moist?
better.albedo.model.prediction <- as.numeric(predict(g1, data.frame(dry=albedo.dry, moist=albedo.moist, met=rep("Observed", length(albedo.dry)))))
obspred <- lm(log10(better.albedo.model.prediction)~albedo.dry)
plot(obspred)
plot(jitter(albedo.moist,1), jitter(better.albedo.model.prediction,1),xlim=c(0, 0.5),ylim=c(0, 0.5))
points(jitter(albedo.moist,1), jitter(albedo.dry,1),xlim=c(0, 0.5),ylim=c(0, 0.5),pch="*")
abline(0,1)
abline(obspred)
summary(obspred)

#interestingly, we obtain a similar relationship to what the calculation uses... moist albedo is reduced 45% relative to dry.
# only difference is that we have a significant intercept of 0.1676... which suggests that our albedoes calculated from dark moist values should have higher albedos when dry than the calculation predicts