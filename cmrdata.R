####CMR Web Scrape of Html content
####there is a separate file for getting hrefs out of the html frm Edgars search structure

library(rvest)

CMR <- function(object){
  
  setwd("C:/Users/Eric/Documents/R/DataMining/SEC_data/CMR_results")
  
  cmr_data <- read.csv("cmr_links_1.csv")
  
  dat = data.frame()
  
  for (i in object){ 
    
    
    name <- html(as.character(i))
    
    returned_name = as.data.frame(name %>%
                                    html_nodes("div, font") %>%
                                    html_text())
    
    rep_label <- rep(as.character(i), times = nrow(returned_name))
    
    bound_values <- as.data.frame(cbind(rep_label, returned_name))     
    
    dat = as.data.frame(rbind(bound_values, dat))
    
  } 
  
  dat = as.data.frame(unique(dat), rownames = F)
  
  colnames(dat) <- c("CMR.Link", "Text")
  
  dat = merge(cmr_data, dat, by = "CMR.Link")
  
  View(dat)

  write.csv(dat, "cmr_20150619_div_font.csv") 
  
  
}

CMR(cmr_data[,2])
