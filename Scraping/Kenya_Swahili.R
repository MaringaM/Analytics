# Kenyastar, Standardmedia.co.ke, theeastafrican, businessdailyafrica, nairobinews.nation.co.ke,
# nairobiwire.com

library(rvest)
library(dplyr)
library(stringr)
library(textcat)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(SnowballC)
library(wordcloud)
library(sentimentr)
library(ldatuning)
library(Rmpfr)

# VOA Swahili --------------------------
startdates <- c("20201215100000", "20201231100000", "20210115100000", "20210131100000",
                "20210215100000", "20210228100000")
enddates <- c("20201231100000", "20210115100000", "20210131100000",
              "20210215100000", "20210228100000", "20210315100000")

voa_urls_all <- c()

for(i in 1:length(startdates)){
  
  query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                  # "(covid%20OR%20coronavirus)%20",
                  "sourcelang:swahili%20",
                  # "sourcecountry:kenya%20",
                  "domain:voaswahili.com",
                  "&output=urllist&maxrecords=250&format=html",
                  "&startdatetime=", startdates[i],
                  "&enddatetime=", enddates[i])
  
  voa <- read_html(query)
  voa_urls <- voa %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  print(length(voa_urls))
  voa_urls_all <- c(voa_urls_all, voa_urls)
}

voa <- list()
# The East African
for(i in 1:length(voa_urls_all)){
  print(i)
  simple <- tryCatch({read_html(voa_urls_all[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".hdr-container") %>%
    html_nodes("h1") %>%
    html_text()
  date <- simple %>%
    html_nodes(".published") %>%
    html_nodes(".date") %>%
    html_text()
  text <- simple %>%
    html_nodes(".wsw") %>%
    html_nodes("p") %>%
    html_text()
  
  voa[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(5)
}

# BBC Swahili --------------------------
startdates <- c("20201215100000", "20201231100000", "20210115100000", "20210131100000",
                "20210215100000", "20210228100000")
enddates <- c("20201231100000", "20210115100000", "20210131100000",
              "20210215100000", "20210228100000", "20210315100000")

bbc_urls_all <- c()

for(i in 1:length(startdates)){
  
  query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                  # "(covid%20OR%20coronavirus)%20",
                  "sourcelang:swahili%20",
                  # "sourcecountry:kenya%20",
                  "domain:bbc.com",
                  "&output=urllist&maxrecords=250&format=html",
                  "&startdatetime=", startdates[i],
                  "&enddatetime=", enddates[i])
  
  bbc <- read_html(query)
  bbc_urls <- bbc %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  print(length(bbc_urls))
  bbc_urls_all <- c(bbc_urls_all, bbc_urls)
}

bbc <- list()
# The East African
for(i in 1:length(bbc_urls_all)){
  print(i)
  simple <- tryCatch({read_html(bbc_urls_all[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".bbc-13g9ltc-Headline.e1yj3cbb0") %>%
    # html_nodes("h1") %>%
    html_text()
  date <- simple %>%
    html_nodes(".bbc-4f4c49-StyledTimestamp.e4zesg50") %>%
    html_text()
  text <- simple %>%
    html_nodes(".bbc-1ff36h2.eoodvxp0") %>%
    html_nodes("p") %>%
    html_text()
  
  bbc[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(5)
}

# Taifa Leo -------------------------------
# Loop through 1 - 91
titles <- c()
for(i in 1:91){
  query <- paste0("https://taifaleo.nation.co.ke/?s=corona&paged=", i)
  tl <- read_html(query)
  title <- tl %>%
    html_nodes(".col-sm-12") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique()
  title <- title[!grepl("paged", title)]
  titles <- c(titles, title)
  print(length(titles))
  Sys.sleep(5)
}

tl_out <- list()
# The East African
for(i in 1:length(titles)){
  print(i)
  simple <- tryCatch({read_html(titles[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".post-content") %>%
    html_nodes(".entry-title") %>%
    # html_nodes("h2") %>%
    html_text()
  header <- header[1]
  date <- simple %>%
    html_nodes(".publish-date") %>%
    html_text()
  date <- date[1]
  text <- simple %>%
    html_nodes(".entry-content") %>%
    html_nodes("p") %>%
    html_text()
  
  tl_out[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(5)
}

all_outputs <- list(bbc, voa, tl_out)
saveRDS(all_outputs, './kenya_swahili_covid.rds')
