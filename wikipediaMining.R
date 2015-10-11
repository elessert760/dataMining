# sessionInfo()
# R version 3.2.0 (2015-04-16)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 8 x64 (build 9200)
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] reshape2_1.4.1   rvest_0.3.0      data.table_1.9.4 tidyr_0.2.0     
# [5] dplyr_0.4.2      stringr_1.0.0    xml2_0.1.2      
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.1    XML_3.98-1.3   assertthat_0.1 chron_2.3-47   R6_2.1.1      
# [6] plyr_1.8.3     DBI_0.3.1      magrittr_1.5   httr_1.0.0     stringi_0.5-5 
# [11] curl_0.9.3     tools_3.2.0    selectr_0.2-3  parallel_3.2.0



library(xml2)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(rvest)
library(reshape2)

rm(list = ls())
link <- "https://en.wikipedia.org/wiki/List_of_companies_of_the_United_States"
wikipedialistmining <- function(link, filename){
  links <- link
  dat <- data.frame()
  
  doc <- read_html(links)
  nodes <- xml_find_all(doc, "//a")
  names <- xml_attr(nodes, "title")
  hrefs <- xml_attr(nodes, "href")
  web_address <- paste("https://en.wikipedia.org", hrefs, sep = "")
  
  Data <- cbind(names[1:1839], web_address[1:1839])
  Data <- as.data.frame(Data)
  Data$V2 <- gsub("%26", replacement = "&",x = Data$V2)
  Data$V2 <- gsub("%27", replacement = "'", x = Data$V2)
  Data <- Data[complete.cases(Data),]
  
  
  new_links <- Data$V2[5:length(Data$V2)]
  
  foo <- data.frame(matrix(ncol = 2))
  for (i in new_links){ 
    
    # td, tr, b
    # td, th, .vcard a
    name <- read_html(as.character(i))
    
    returned_variable = as.data.frame(name %>%
                                        html_nodes(css ='tr, th div a') %>%
                                        html_text(), stringsAsFactors = F)
    
    
    
    if (nrow(returned_variable)>0){
      returned_variable$Label <- i
    }else{
      returned_variable[1,1]<- "NULL"
      returned_variable[1,2]<- "NULL"
    }
    
    
    colnames(foo)<- colnames(returned_variable)
    
    foo <- rbind(foo, returned_variable)
  }
  
  
  colnames(foo) <- c("Mined Data","Label")
  
  
  Data$V1 <- as.character(Data$V1)
  foo$Label <- as.character(foo$Label)
  
  
  colnames(Data)<- c("Company Name", "Label")
  
  combined_data <- merge(foo, Data, by ="Label" )
  
  colnames(combined_data) <- c("URL","Mined Data", "Company Name")
  
  combined_data <- unique(combined_data)
  
  test_data<- gsub("\n", replacement = "|", x = combined_data$`Mined Data`,fixed = T)
  
  
  combined_data$`Mined Data` <- gsub("\n", replacement = "|", x = combined_data$`Mined Data`,fixed = T)
  combined_data$`Mined Data` <- paste("|", combined_data$`Mined Data`, sep = "")
  combined_data$`Mined Data` <- gsub("||", replacement = "|", x = combined_data$`Mined Data`,fixed = T)
  combined_data$`Mined Data` <- gsub("||", replacement = "|", x = combined_data$`Mined Data`,fixed = T)
  combined_data$`Mined Data` <- gsub("||", replacement = "|", x = combined_data$`Mined Data`,fixed = T)
  
  
  keyvaluepairs <- as.data.frame(str_split_fixed(string = combined_data$`Mined Data`, pattern = "\\|", n = 3))
  
  datatowrite <- cbind(combined_data[,c("URL","Company Name")], keyvaluepairs[,c("V2", "V3")])
  
  key <- c('Headquarters', 'Founded', 'Website', 'Type', 'Industry', 'Key people', 'Products', 'Number of employees', 
           'Revenue', 'TradedÂ as', 'Net income', 'Total assets', 'Operating income', 'Founder', 'Total equity', 
           'Area served', 'Slogan', 'Number of locations', 'Parent', 'Subsidiaries', 'Owner', 'Services', 'Divisions', 
           'Predecessor', 'FootnotesÂ / references', 'Defunct', 'Fate', 'Founders', 'IATA', 'Former type', 'Successor', 'Profit', 
           'Formerly called', 'Genre', 'Fleet size', 'Employees', 'Parent company', 'Brands', 'Trading name', 
           'Hubs', 'Commenced operations','Founder(s)', 'KeyÂ people', 'Company slogan', 'Production output', 'Type of site', 
           'AreaÂ served')
  
  datatowrite <- datatowrite[which(datatowrite$V2 %in% key),]
  
  
  wideformatdatatowrite <- reshape(datatowrite, idvar = c("Company Name", "URL"), timevar = "V2", direction = "wide")
  
  colnames(wideformatdatatowrite) <- gsub("V3\\.", "", colnames(wideformatdatatowrite))
  
  
  wideformatdatatowrite$Founder <-  paste(
    wideformatdatatowrite$Founder,wideformatdatatowrite$Founders, wideformatdatatowrite$`Founder(s)`, sep = ","
  )
  
  wideformatdatatowrite$Founders <- NULL
  wideformatdatatowrite$`Founder(s)`<- NULL
  
  wideformatdatatowrite$Founder <-  gsub("?|,?NA,?", replacement = "", x = wideformatdatatowrite$Founder)
  
  wideformatdatatowrite$Founder <-  trimws(wideformatdatatowrite$Founder)
  
  wideformatdatatowrite$Founder <-  gsub(", , ", replacement = "", x = wideformatdatatowrite$Founder)
  
  
  
  for (i in 1:ncol(wideformatdatatowrite)) {
    wideformatdatatowrite[,i] <-    trimws(wideformatdatatowrite[,i])
    wideformatdatatowrite[,i] <-    gsub("  ", replacement = " ", x = wideformatdatatowrite[,i])
    wideformatdatatowrite[,i] <-    gsub(",*$", replacement = "", x = wideformatdatatowrite[,i])
    wideformatdatatowrite[,i] <-    gsub("\\|*$", replacement = "", x = wideformatdatatowrite[,i])
    wideformatdatatowrite[,i] <-    gsub("\\[.*\\]", replacement = "", x = wideformatdatatowrite[,i])
    wideformatdatatowrite[,i] <-    gsub("Â", replacement = " ", x = wideformatdatatowrite[,i])
    wideformatdatatowrite[,i] <-    gsub("\\|", replacement = ", ", x = wideformatdatatowrite[,i])
    wideformatdatatowrite[,i] <-    gsub(";.*", replacement = "", x = wideformatdatatowrite[,i])
    
  }
  
  colnames(wideformatdatatowrite)<-    gsub("Â", replacement = " ", x = colnames(wideformatdatatowrite))
  
  
  setwd("C:/Users/Eric/Documents/R/DataMining/Wikipedia")
  
  write.csv(wideformatdatatowrite, file = paste(Sys.Date(), "Data Mining.csv", sep = "-"),row.names = F)
  
  
}


system.time(wikipedialistmining(link = "https://en.wikipedia.org/wiki/List_of_companies_of_the_United_States", filename = "testfileforfunction.csv"))
