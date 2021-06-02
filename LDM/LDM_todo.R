library(aqp)
library(soilDB)

library(SoilTaxonomy)
mollisols <- getChildTaxa("mollisols", level = "suborder")[[1]]
alfisols <- getChildTaxa("alfisols", level = "suborder")[[1]]
inceptisols <- getChildTaxa("inceptisols", level = "suborder")[[1]]
vertisols <- getChildTaxa("vertisols", level = "suborder")[[1]]

# OC, pH, base saturation (pH 7, sum cat), matrix color
# 
# correlated as: Mollisols 
#                Xeric and Ustic Alfisols (w/ and w/o a mollic)
#                Xeric and Ustic Inceptisols (w/ and w/o a mollic)
#                Xeric and Ustic Vertisols (w/ and w/o a mollic)

# horizons above a natric (ustic, xeric aridic)
taxa  <- toupper(SoilTaxonomy::taxon_code_to_taxon(LETTERS[1:12]))
# 
# big_spc <- lapply(taxa, fetchLDM, dsn = "E:/Geodata/soils/LDM-compact.sqlite", 
#                     what = "corr_taxorder")
# big_spc <- fetchLDM("alfisols",
#                     dsn = "E:/Geodata/soils/LDM-compact.sqlite", what = "corr_taxorder")
library(soilDB)
load("LDM/bigspc.rda")
mollisols_spc <- bigspc
# mollisols_spc <- fetchLDM("MOLLISOLS",
#                           dsn = "E:/Geodata/soils/LDM-compact.sqlite",
#                           what = "corr_taxorder", tables = c("lab_physical_properties",
#                                                              "lab_chemical_properties"))

# save(mollisols_spc, file = "mollisols_LDM.rda")
# load(file = "mollisols_LDM.rda")

diagtable <- read.csv("E:/Geodata/soils/NASIS_morph_spc_diag.csv")

mollic_epipedons <- subset(diagtable, featkind %in% c("mollic epipedon","umbric epipedon"))

# remove bad logic layers (sort of heavy handed)
mollisols_clean <- subset(mollisols_spc, checkHzDepthLogic(mollisols_spc)$valid)

# mollisols confirmed as having a mollic epipedon
mollisols_w_mollic <- subset(mollisols_clean, mollisols_clean$peiid %in% mollic_epipedons$peiid)

site(mollisols_w_mollic) <- subset(mollic_epipedons, peiid %in% mollisols_w_mollic$peiid)

# just the portion of profiles with mollic
mollisols_mollic <- glom(mollisols_w_mollic, 
                         z1 = mollisols_w_mollic$featdept, 
                         z2 = mollisols_w_mollic$featdepb)

mollisols_w_mollic$natric_top <-  minDepthOf(mollisols_w_mollic, pattern = "n")$hzn_top

# get corresponding morphologic data
load(file = "LDM/mollic/morphology_spc.rda")

mollisols_mollic_morph <- subset(morphology_spc, 
                                 morphology_spc$peiid %in% mollisols_mollic$pedoniid)

# we consider O horizons "dark" even without colors, but will only count mineral towards mollic 
mollisols_mollic_morph$isDark <- hasDarkColors(mollisols_mollic_morph) | grepl("O", mollisols_mollic_morph$hzname)

# get darkness depths
darkdepths <- data.table::as.data.table(depthOf(
    mollisols_mollic_morph,
    pattern = "TRUE",
    top = FALSE,
    hzdesgn = "isDark"
  ))

# find profiles with a confirmed dark surface
dark_confirmed <- darkdepths[horizons(mollisols_mollic_morph), on = c("peiid", "hzdepb")][,
                             list(darksurface = suppressWarnings(1:.N == cumsum(na.omit(i.isDark)))[1] == TRUE), 
                             by = "peiid"]
dark_peiids <- dark_confirmed[which(dark_confirmed$darksurface), ]$peiid
mollisols_mollic_morph_dark <- subset(mollisols_mollic_morph, peiid %in% dark_peiids)

# subset lab data to match profiles with dark morphology
mollisols_w_mollic_morph <- subset(mollisols_w_mollic, 
                                   pedoniid %in% profile_id(mollisols_mollic_morph_dark))

# merge in lab data to site and horizons of morph SPC

# is site data needed? Need to deal with dupe cols
# use peiid since that is the idname for the morph SPC
labsite <- site(mollisols_w_mollic_morph)
labsite <- labsite[,colnames(labsite)[!colnames(labsite) %in% names(mollisols_mollic_morph_dark)]]
labsite$peiid <- labsite$pedoniid
site(mollisols_mollic_morph_dark) <- labsite

# merge in lab data to horizons of morph
labhz <- horizons(mollisols_w_mollic_morph)
labhz <- labhz[,colnames(labhz)[!colnames(labhz) %in% c("pedon_key","hzID")]]

horizons(mollisols_mollic_morph_dark) <- labhz

# OK?
spc_in_sync(mollisols_mollic_morph_dark)
all(checkHzDepthLogic(mollisols_mollic_morph_dark)$valid)

# estimate mollic thickness requirement
mollisols_mollic_morph_dark$mtr <- profileApply(mollisols_mollic_morph_dark,
                                                mollic.thickness.requirement,
                                                clay.attr = "clay_total")

# estimate thickness based on dark color morphology
mollisols_mollic_morph_dark$dark_depth <- getSurfaceHorizonDepth(mollisols_mollic_morph_dark,
                                                                 pattern = "TRUE",
                                                                 hzdesgn = "isDark")$hzdepb
mollisols_mollic_morph_dark$carbon_combined <- mollisols_mollic_morph_dark$total_carbon_ncs
idx <- is.na(mollisols_mollic_morph_dark$carbon_combined) & !is.na(mollisols_mollic_morph_dark$organic_carbon_walkley_black)
mollisols_mollic_morph_dark$carbon_combined[idx] <- mollisols_mollic_morph_dark$organic_carbon_walkley_black[idx]

save(mollisols_mollic_morph_dark, file = 'LDM/mollic/clean_mollisols.rda')

par(mar=c(0,0,2.5,1), mfrow=c(2,1))
mollisol_natric <- subset(mollisols_mollic_morph_dark, !is.na(natric_top))

plotSPC(mollisol_natric, 
        cex.name = 0.6, y.offset = 5,
        color = "base_sat_nh4oac_ph_7")
plotSPC(mollisol_natric, 
        cex.name = 0.6,
        color = "isDark")

mollisol_natric$surfacedepth <- mollisol_natric$hzn_top[mollisol_natric[,,.FIRST,.HZID]]
mollisol_natric$surfacedepth[is.na(mollisol_natric$surfacedepth)] <- 0
mollisol_above_natric <- glom(mollisol_natric, mollisol_natric$surfacedepth, mollisol_natric$natric_top)

# load('LDM/mollic/clean_mollisols.rda')
# 
par(mar=c(1,1,3,1), mfrow=c(1,1))
plot(mollisols_mollic_morph_dark[20:30,], name="")

# 86% of mollisols with lab+morphologic data meet the 5-3-3 color requirements
# 83% of all soils with mollics lab+morphologic data meet the 5-3-3 color requirements
prop.table(table(mollisols_mollic_morph_dark$dark_depth >= mollisols_mollic_morph_dark$mtr))

mollisols_dark <- subset(mollisols_mollic_morph_dark, dark_depth >= mtr)

mollisols_dark_mollic <- glom(mollisols_dark,
                              z1 = mollisols_dark$featdept, 
                              z2 = mollisols_dark$featdepb)

make_plots <- function(object, atitle) {
  
  k_suffix <- subsetHz(object, grepl("k", hzname))
  
  plot(density(object$ph_h2o, na.rm = TRUE), xlim=c(4,10),
       main = sprintf("pH 1:1 Water (%s)", atitle))
  abline(v = 6.5, lty = 2)
  
  plot(density(object$ph_cacl2, na.rm = TRUE), xlim=c(4,10),
       main = sprintf("pH CaCl2 (%s)", atitle))
  abline(v = 6, lty = 2)
  
  plot(density(k_suffix$ph_h2o, na.rm = TRUE), xlim=c(4,10),
       main = sprintf("pH 1:1 Water (%s; k suffix)", atitle))
  abline(v = 6.5, lty = 2)
  
  plot(density(k_suffix$ph_cacl2, na.rm = TRUE), 
       xlim=c(4,10),
       main = sprintf("pH CaCl2 (%s; k suffix)", atitle))
  abline(v = 6, lty = 2)
  
  plot(density(object$carbon_combined, na.rm = TRUE, from = 0, to = 20), 
       main = sprintf("Total Carbon NCS or Organic Carbon by Walkley-Black (%s)", atitle))
  abline(v = 0.6, lty = 2)
  
  plot(density(object$base_sat_nh4oac_ph_7, na.rm = TRUE, from = 0, to = 100), 
       main = sprintf("Base Saturation NH4OAc (pH 7) (%s)", atitle))
  abline(v = 50, lty = 2)
  
  plot(density(k_suffix$base_sat_nh4oac_ph_7, na.rm = TRUE, from = 0, to = 100), 
       main = sprintf("Base Saturation NH4OAc (pH 7)(%s; k suffix)", atitle))
  abline(v = 50, lty = 2)
  
  plot(density(object$base_sat_sum_of_cations_ph_8_2, na.rm = TRUE, from = 0, to = 100), 
       main = sprintf("Base Saturation Sum Cations (pH 8.2) (%s)", atitle))
  abline(v = c(35, 75), lty = 2)
  
  plot(density(k_suffix$base_sat_sum_of_cations_ph_8_2, na.rm = TRUE, from = 0, to = 100), 
       main = sprintf("Base Saturation Sum Cations (pH 8.2) (%s; k suffix)", atitle))
  abline(v = c(35, 75), lty = 2)
  
  show(hexbin::hexbinplot(data = horizons(object), base_sat_nh4oac_ph_7 ~ ph_h2o, 
                          main = sprintf("Base Saturation NH4OAc (pH 7)\n v.s. pH 1:1 Water (%s)",
                                         atitle)))
  # abline(h = 50, lty = 2)
  # abline(v = 6.5, lty = 2)
  
  show(hexbin::hexbinplot(data = horizons(object), base_sat_sum_of_cations_ph_8_2 ~ ph_h2o, 
                          main = sprintf("Base Saturation Sum Cations (pH 8.2)\n v.s. pH 1:1 Water (%s)", atitle)))
  # abline(h = c(35, 75), lty = 2)
  # abline(v = 6.5, lty = 2)
  
  show(hexbin::hexbinplot(data = horizons(object), base_sat_nh4oac_ph_7 ~ ph_cacl2, 
                          main = sprintf("Base Saturation NH4OAc (pH 7)\n v.s. pH CaCl2 (%s)", atitle)))
  # abline(h = 50, lty = 2)
  # abline(v = 6, lty = 2)
  
  show(hexbin::hexbinplot(data = horizons(object), base_sat_sum_of_cations_ph_8_2 ~ ph_cacl2, 
                          main = sprintf("Base Saturation Sum Cations (pH 8.2)\n  v.s. pH CaCl2 (%s)", atitle)))
  # abline(h = c(35, 75), lty = 2)
  # abline(v = 6, lty = 2)
  
}

make_plots(mollisols_dark_mollic, atitle = "Mollic and Umbric Epipedons")
make_plots(mollisol_above_natric, atitle = "Horizons above Natric")

byhzgrp_suffix <- split(
  horizons(mollisols_dark_mollic),
  generalize.hz(
    mollisols_dark_mollic$hzname,
    new = c("k", "y", "z", "n", "c"),
    pat = c("k", "y", "z", "n", "c")
  )
)

byhzgrp_master <- split(
  horizons(mollisols_dark_mollic),
  generalize.hz(
    mollisols_dark_mollic$hzname,
    new = c("A", "B", "C"),
    pat = c("A", "B", "C")
  )
)

sapply(byhzgrp_suffix, nrow)
sapply(byhzgrp_master, nrow)

mollisols_mollic_morph_dark$caco3_lt_2_mm <- mollisols_mollic_morph_dark$caco3_lt_2_mm

colorsub <- subsetHz(mollisols_mollic_morph_dark, carbon_combined > 0.6, caco3_lt_2_mm >= 1)
colordata <- horizons(colorsub)[, c('caco3_lt_2_mm',"isDark")]
par(mar=c(4,4,1,2), mfrow=c(1,1))

boxplot(log10(colordata$caco3_lt_2_mm) ~ horizons(colorsub)$d_value, xlab="Dry Value", ylab="log10 CaCO3 % <2mm")
boxplot(log10(colordata$caco3_lt_2_mm) ~ horizons(colorsub)$m_value, xlab="Moist Value", ylab="log10 CaCO3 % <2mm")

highcaco3 <- subset(horizons(mollisols_mollic_morph_dark), caco3_lt_2_mm > 40)

write.csv(file = "mollic_umbric_lab_morph_highCaCO3.csv", highcaco3)
