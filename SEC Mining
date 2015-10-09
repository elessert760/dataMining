#Weird that this works on my desktop R version 3.1.1 but not on my laptop r version 3.2.0

library(xml2)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)

pages <- seq(from = 1, to = 4600, by = 100)
links <- paste("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=*&sort=Date&startDoc=", pages, "&numResults=100&isAdv=true&formType=FormSD&fromDate=mm/dd/yyyy&toDate=mm/dd/yyyy")

dat <- data.frame()

#some people may not like the regex thing here, but it works

for(i in links){
doc <- read_html(i)
nodes <-  xml_find_all(doc, "//a")
hrefs <- xml_attr(nodes, "href")  
hrefs <- gsub(pattern = "^\\javascript:(*)", replacement = "", x= hrefs)
hrefs <- gsub(pattern = "^\\open(*)", replacement = "", x= hrefs)
hrefs <- gsub(pattern = "new", replacement = "", x= hrefs)
hrefs <- gsub(pattern = "ciksearch", replacement = "", x= hrefs)
hrefs <- gsub(pattern = "\\(\\'", replacement = "", x= hrefs)
hrefs <- gsub(pattern = "filing", replacement = "", x= hrefs)
hrefs <- gsub(pattern = "\','');", replacement = "", x= hrefs)
sorted_links <- hrefs[grep("http://", x = hrefs)]

split_strings <- str_split(pattern = "\','", string = sorted_links)
as.list(split_strings)

foo <- as.data.frame(unique(sapply(split_strings, "[[", 1)))
#should probably get the second element and rbind them together as a label

colnames(foo)<- colnames(dat)

dat <- rbind(foo, dat)



}
