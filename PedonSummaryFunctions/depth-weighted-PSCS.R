library(soilDB)

f<- fetchNASIS()
f.sub <- f[!is.na(f$taxonkind) & f$taxonkind == "series",]

depth.weighted.average <- function(spc, tdepth, bdepth, attr, ...) {
  #expand `attr` in formula
  custom.formula <- formula(paste0(tdepth,":",bdepth," ~ ", 
                                   paste0(attr, collapse=" + ")))
  # calculate a depth-weighted average using aqp::slice()
  return(mean(slice(spc, custom.formula, just.the.data=TRUE)[[attr]],
              na.rm = TRUE))
}

f.sub$pscs_clay <- profileApply(f.sub, function(p) {
  bounds <- estimatePSCS(p)
  return(depth.weighted.average(p, tdepth = bounds[1], bdepth = bounds[2], attr='clay'))
})
f.sub$pscs_frags <- profileApply(f.sub, function(p) {
  bounds <- estimatePSCS(p)
  return(depth.weighted.average(p, tdepth = bounds[1], bdepth = bounds[2], attr='total_frags_pct_nopf'))
})

quantile(f.sub$pscs_clay, probs = c(0,0.05,0.5,0.95,1), na.rm=T)

f.sub2 <- f.sub[-which(f.sub$pscs_frags > 35),]
f.skeletal <- f.sub[which(f.sub$pscs_frags > 35),]
paste0(site(f.skeletal)$peiid, collapse=",")

quantile(f.sub2$pscs_frags, probs = c(0,0.05,0.5,0.95,1), na.rm=T)
quantile(f.skeletal$pscs_frags, probs = c(0,0.05,0.5,0.95,1), na.rm=T)
