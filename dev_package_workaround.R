# get all dependencies and CRAN versions of NCSS-tech packages
install.packages(c("stringi","aqp","soilDB","sharpshootR")) #note stringi must come first! install the binary (option:'n') NOT from source

# get latest GitHub version of NCSS-tech packages
devtools::install_github(c('ncss-tech/aqp','ncss-tech/soilDB','ncss-tech/sharpshootR','ncss-tech/soilReports'),dependencies=F) 
