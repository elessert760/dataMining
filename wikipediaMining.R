library(xml2)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(rvest)
library(reshape2)


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


new_links <- Data$V2[15:length(Data$V2)]

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
combined_data$`Mined Data` <- paste("|", combined_data$`Mined Data`, sep = "")

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

write.csv(wideformatdatatowrite, file = paste(Sys.Date(), filename, sep = "-"),row.names = F)


}


system.time(wikipedialistmining(link = "https://en.wikipedia.org/wiki/List_of_companies_of_the_United_States", filename = "testfileforfunction.csv"))
