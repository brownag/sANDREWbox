tar_option_set(packages = c("magrittr","aqp"))
list(
  tar_target(name = mollic_umbric_lab_morph,
             read.csv(file = "mollic_umbric_lab_morph.csv", stringsAsFactors = FALSE)
  ), 
  tar_target(name = mollic_umbric_lab_morph_complete,{
             ldx <- complete.cases(mollic_umbric_lab_morph[,c('hzdept','caco3_lt_2_mm','isDark','carbon_combined')])
             mollic_umbric_lab_morph_complete <- mollic_umbric_lab_morph[ldx, ]
             depths(mollic_umbric_lab_morph_complete) <- peiid ~ hzdept + hzdepb
             mollic_umbric_lab_morph_complete$isDarkMoist <- mollic_umbric_lab_morph_complete %>% 
               hasDarkColors(d_value = NA)
             mollic_umbric_lab_morph_complete
    }),
  tar_target(name = q1_data, mollic_umbric_lab_morph_complete %>% subsetHz(carbon_combined >= 0.6 &
                                                                          caco3_lt_2_mm >= 15 & 
                                                                           !isDark | 
                                                                            (is.na(isDark) & !isDarkMoist))),
  tar_target(name = q2_data, mollic_umbric_lab_morph_complete %>% subsetHz(carbon_combined >= 0.6 &
                                                                          caco3_lt_2_mm >= 40 & 
                                                                           !isDark | 
                                                                            (is.na(isDark) & !isDarkMoist))),
  tar_target(name = q1_data_18, q1_data %>% subsetHz(hzdept <= 18)),
  tar_target(name = q2_data_18, q2_data %>% subsetHz(hzdept <= 18)),
  tar_target(name = q1_data_25, q1_data %>% subsetHz(hzdept <= 25)),
  tar_target(name = q2_data_25, q2_data %>% subsetHz(hzdept <= 25))
)
