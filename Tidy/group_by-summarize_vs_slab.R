library(aqp)
library(soilDB)

kssldat <- fetchKSSL(mlra = c("18","22A"))

# remove invalid profiles 
kssldat <- filter(kssldat, checkHzDepthLogic(kssldat)$valid)

# this truncates the whole SPC to [0,30]
kssltrunc <- trunc(kssldat, 0, 30)

# new way to do it: group_by + summarize
ksslgroup <- group_by(kssltrunc, "mlra") 

# profile identity split to get profile-level averages
# ksslgroup <- group_by(kssltrunc, idname(kssltrunc)) 

# summarize takes one or more expressions that resolve to 1 value per group
res1 <- summarize(ksslgroup, 
                  oc_mean = mean(oc, na.rm = TRUE), 
                  ph_h2o_mean = mean(ph_h2o, na.rm = TRUE),
                  oc_sd = sd(oc, na.rm = TRUE), 
                  ph_h2o_sd = sd(ph_h2o, na.rm = TRUE))

# slab way to do it; using 0-30cm trunc'd SPC
slabres  <- aqp::slab(ksslgroup, mlra ~ oc + estimated_oc + estimated_om + ph_h2o, 
                      slab.fun = function(x) {
                        foo <- c(mean(x, na.rm = TRUE), 
                                 sd(x, na.rm = TRUE))
                        names(foo) <- c("Mean","SD")
                        return(foo)
                      })#
                      # alternately, can use 30cm slab structure.
                      #, slab.structure = 30)

res2 <- do.call('rbind', lapply(split(slabres, f = slabres$mlra), function(mlra) {
  data.frame(mlra = unique(mlra$mlra),
             oc_mean = mean(subset(mlra, variable == "oc")$Mean, na.rm = TRUE), 
             ph_h2o_mean = mean(subset(mlra, variable == "ph_h2o")$Mean, na.rm = TRUE),
             oc_sd = mean(subset(mlra, variable == "oc")$SD, na.rm = TRUE), 
             ph_h2o_sd = mean(subset(mlra, variable == "ph_h2o")$SD, na.rm = TRUE))
}))

# this is using un-sliced data
res1      
#  mlra  oc_mean ph_h2o_mean    oc_sd ph_h2o_sd
#1   18 1.748223    6.097802 3.800573 0.5514775
#2  22A 2.970130    5.700705 3.315585 0.5555779

# this is using sliced data -- similar values
res2
# mlra  oc_mean ph_h2o_mean    oc_sd ph_h2o_sd
# 18    18 1.682727    6.090462 4.044607 0.5393427
# 22A  22A 2.972001    5.696293 3.167955 0.5505772