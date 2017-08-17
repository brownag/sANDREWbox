
csv.path <- "GroupedProfilePlotByMU/csv"
mu.pattern <- "404."
output.name <- "404X"
filez <- list.files(csv.path,recursive=T,full.names = TRUE)
target_files <- grepl(filez, pattern=mu.pattern)
#column names to pull from raw GPP by MU output
colnamz <- c("peiid","pedon_id","bedrckdepth","bedrock_kind","class_type","taxonname","part_size_class","tax_subgroup","ecositeid","musym")

buff <- NULL
for(f in filez[target_files]) {
  r <- read.csv(f)
  print(names(r))
  r <- r[, colnamz]
  if(is.null(buff)) {
    buff <- r
  } else {
    buff <- rbind(buff, r)
  }
}

buff <- cbind(buff, data.frame(component_office=" ", component_lab=" "))

write.csv(buff, file=paste0("PedonWorksheets/",output.name,".csv"), row.names=FALSE)
