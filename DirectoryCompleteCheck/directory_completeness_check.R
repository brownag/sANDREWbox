# CHECK REPORT DIRECTORY VALIDATOR
#  determines list of files present in 'sample.dir' and checks whether they exist in 'test.dir'
#  allows for filtering based on filename prefix
# @andrew brown

############################################################################################################################
# SETUP
############################################################################################################################
sample.dir <- 'S:/NRCS/Archive_Andrew_Brown/CA630/QC/620X & 605X'
test.dir <- 'S:/NRCS/Archive_Andrew_Brown/CA630/QC/607X'
prefixes <- c("CHECK","MANU") # prefixes to filter filenames. keep files starting with one of the prefixes listed.
                              # use a blank ("") to get all files in sample.dir, but this may not be desirable if there are
                              # other non-report files present in the directory.
############################################################################################################################
foo <- list.files(sample.dir, recursive = TRUE) #recursively list files in sample
report.list <- basename(foo) #get the base name

report.filter <- unlist(lapply(report.list, FUN=function(xx) {
  any(startsWith(xx, prefixes))
}))
report.list <- report.list[report.filter]

directory.list <- dirname(foo)
directory.list <- levels(factor(unlist(directory.list[dir.exists(paste0(sample.dir,"/",unlist(directory.list)))])))
# gets list of subdirectories, currently not used as the folder structure is not enforced just the list of files/reports

complete.qcreport.set <- data.frame(reports=report.list)
write.csv(complete.qcreport.set, "qc_reports_complete.csv")

test.foo <- list.files(test.dir, recursive = TRUE)
test.report.list <- unlist(lapply(strsplit(test.foo, split = "/"), function(x) return(x[length(x)])))

# list the missing reports
missing.list <- report.list[!(report.list %in% test.report.list)]
if(length(missing.list)) {
  print(paste0("The following reports are missing from ",test.dir,": "))
  print(missing.list)
} else {
  print("All files from sample directory are present in test directory. See qc_reports_complete.csv for a complete list of report file names being checked.")
}