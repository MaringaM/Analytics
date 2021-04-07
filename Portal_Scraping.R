
library(rvest)
library(dplyr)

dwh <- read_html("https://data.kenyahmis.org:9000/#/")
str(dwh)
body_nodes <- dwh %>% html_node('body') %>% html_children()
body_nodes

body_nodes %>% html_children()


rank <- dwh %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//h5[contains(@class, 'text-center m-2 card-title')]") %>% 
  rvest::html_text()

sentences <- dwh%>%
  html_nodes("body") %>%
  html_text()
