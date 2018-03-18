####CMR Web Scrape of Html content
####there is a separate file for getting hrefs out of the html frm Edgars search structure

library(rvest)


cmr_data <- read.csv("cmr_links.csv")

CMR <- function(i) { 
    
    obj <- read_html(i) %>% html_nodes("p, b, font, div") %>% html_text()
    
    obj %<>% map_chr(trimws)
    
    obj %<>% unique()
    
    obj %<>% paste(collapse = " ")
    
    return(obj)
  
    } 

  
test <- cmr_data$url[3:7] %>% map_chr(CMR)
