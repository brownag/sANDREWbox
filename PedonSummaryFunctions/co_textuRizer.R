source("PedonSummaryFunctions\\pedon_summary_functions.R")
source("PedonSummaryFunctions\\fine_earth_fractions.R")
library(soilDB)
profileApply(fetchNASIS_components(),checkComponentFineEarthLimits)

ncd <- fetchNASIS_components()

l1 <- profileApply(ncd, getComponentAWCLow)
l2 <- profileApply(ncd, getComponentAWCRV)
l3 <- profileApply(ncd, getComponentAWCHigh)

df <- data.frame(chiid=names(l1), low=as.numeric(l1), rv=as.numeric(l2), high=as.numeric(l3))
df$chiid <- as.numeric(t(as.data.frame(strsplit(as.character(df$chiid),split="\\."),stringsAsFactors = F))[,2])

foo <- profileApply(ncd, makeComponentTextureSummary, simplify=F)
foo <- lapply(foo, FUN=function(x) {
  return(x[order(as.numeric(t(as.data.frame(strsplit(as.character(x$depth)," to "), stringsAsFactors = F))[,1])),])
})

for(f in 1:length(foo)) {
  d <- data.frame(coiid=rep(names(foo)[f], times=nrow(foo[[f]])), stringsAsFactors = F)
  df <- cbind(d, foo[f])
  df <- merge(df, site(ncd), by="coiid")[,1:9]
  colnames(df) <- c("coiid","chiid","depth","textures","clay","frags","dmudesc","compname")
  df <- df[,c(7:8,1:6)]
  buf <- rep("",nrow(df))
  df <- cbind(df,data.frame(awclo=buf,awcrv=buf,awchi=buf,ksatclass=buf,om=buf))
  write.table(df, file="component_awc_worksheet.csv", sep = ",", append=TRUE, quote = FALSE, eol = "\n", row.names = F)
}
