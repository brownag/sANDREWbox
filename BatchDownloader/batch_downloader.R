#Batch file downloadR


#format URL,File Title
files=read.csv("file_urls.csv",stringsAsFactors=FALSE)

files=files[28,]
files
#TODO: add PDF scraper
#host_url="http://www.blahblah.com/index.html"
#pdf_pattern="^(https?:\/\/)?www\.([\da-z\.-]+)\.([a-z\.]{2,6})\/[\w \.-]+?\.pdf$" #TODO add capture around whole url
##scan source of host_url for PDF urls, capture matches 
##create vector of URLs
#TODO: figure out default naming scheme (use <a> tags? or just filename? optrions)

#uses method "internal"
for(i in 1:length(files[,1])) {
  download.file(url=files[i,1],destfile=paste0(files[i,2],".pdf"),mode="wb")
}

#oneliner requires "libcurl"
##check for support with capabilities("libcurl")
#download.file(url=files[,1],destfile=paste(files[,2],".pdf",sep=""),mode="wb",method="libcurl")
