#Weird that this works on my desktop R version 3.1.1 but not on my laptop r version 3.2.0

library(tidyverse)
library(stringr)
library(rvest)
library(magrittr)
library(purrr)

pages <- seq(from = 1, to = 10000, by = 100)
links <- paste0("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=*&sort=Date&startDoc=", 
                pages,
                "&numResults=100&isAdv=true&formType=FormSD&fromDate=mm/dd/yyyy&toDate=mm/dd/yyyy")

get_links <- function(i){

doc <- read_html(i) %>% html_nodes("a") %>% html_attr("href") %>% grep(., pattern = "http:", value = T)

doc %<>% gsub(., pattern = "javascript:opennew(", replacement = "", fixed = T) %>% 
  gsub(., pattern = "javascript:opennewfiling(", replacement = "", fixed = T) %>% 
  str_split(",")


Links <- doc %>% map_chr(extract, 1)
Names <- doc %>% map_chr(extract, 2)

names(Names) <- "Names"
names(Links) <- "Links"

Links %<>% cbind(Names) %>% as_data_frame() %>% unique

colnames(Links) <- c("url", "company_name")

Sys.sleep(1)

return(Links)

}

links[1]

test <- links[1:10] %>% map_df(get_links)
test %<>% unique
test$url %<>% gsub(., pattern = "'", replacement = "")

test %>% write_csv("cmr_links.csv")
