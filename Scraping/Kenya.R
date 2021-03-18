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

# Nairobi Wire --------------------------
query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                "(covid%20OR%20coronavirus)%20",
                "sourcelang:english%20",
                "sourcecountry:kenya%20",
                "domain:nairobiwire.com",
                "&output=urllist&maxrecords=250&format=html&timespan=3m")

nairobiwire <- read_html(query)
nairobiwire_urls <- nairobiwire %>%
  html_nodes("table") %>%
  html_nodes("a") %>%
  html_attr("href")

nairobiwire <- list()
# The East African
for(i in 1:length(nairobiwire_urls)){
  print(i)
  simple <- tryCatch({read_html(nairobiwire_urls[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".singlepage-title") %>%
    html_nodes("h1") %>%
    html_text()
  date <- simple %>%
    html_nodes(".meta-holder") %>%
    html_text()
  text <- simple %>%
    html_nodes(".theiaPostSlider_preloadedSlide") %>%
    html_nodes("p") %>%
    html_text()
  
  nairobiwire[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(5)
}

# Nairobi News -----------------------------------
query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                "(covid%20OR%20coronavirus)%20",
                "sourcelang:english%20",
                "sourcecountry:kenya%20",
                "domain:nairobinews.nation.co.ke",
                "&output=urllist&maxrecords=250&format=html",
                "&startdatetime=20201210100000&enddatetime=20210310100000")

nairobinews <- read_html(query)
nairobinews_urls <- nairobinews %>%
  html_nodes("table") %>%
  html_nodes("a") %>%
  html_attr("href")

nairobinews <- list()
# The East African
for(i in 1:length(nairobinews_urls)){
  print(i)
  simple <- tryCatch({read_html(nairobinews_urls[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".article-content") %>%
    html_nodes("h2") %>%
    html_text()
  date <- simple %>%
    html_nodes(".meta-author") %>%
    html_nodes("span") %>%
    html_text()
  date <- date[3]
  text <- simple %>%
    html_nodes(".article-content__content.clearfix.mt-40") %>%
    html_nodes("p") %>%
    html_text()
  
  nairobinews[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(5)
}

# Business Daily Africa --------------------------

startdates <- c("20201210100000", "20201231100000", "20210115100000", "20210131100000",
                "20210215100000", "20210228100000")
enddates <- c("20201231100000", "20210115100000", "20210131100000",
                "20210215100000", "20210228100000", "20210310100000")

businessdailyafrica_urls_all <- c()

for(i in 1:length(startdates)){
  
  query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                  "(covid%20OR%20coronavirus)%20",
                  "sourcelang:english%20",
                  "sourcecountry:kenya%20",
                  "domain:businessdailyafrica.com",
                  "&output=urllist&maxrecords=250&format=html",
                  "&startdatetime=", startdates[i],
                  "&enddatetime=", enddates[i])
  
  businessdailyafrica <- read_html(query)
  businessdailyafrica_urls <- businessdailyafrica %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  print(length(businessdailyafrica_urls))
  businessdailyafrica_urls_all <- c(businessdailyafrica_urls_all, businessdailyafrica_urls)
}


businessdailyafrica <- list()
# The East African
for(i in 1:length(businessdailyafrica_urls_all)){
  print(i)
  simple <- tryCatch({read_html(businessdailyafrica_urls_all[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".article-title") %>%
    html_text()
  date <- simple %>%
    html_nodes(".byline") %>%
    html_text()
  text <- simple %>%
    html_nodes(".article-story.page-box") %>%
    html_nodes("p") %>%
    html_text()
  
  businessdailyafrica[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(2)
}

# KenyaStar ---------------------------
startdates <- c("20201210100000", "20201221100000", "20201231100000", "20210110100000", "20210121100000",
                "20210131100000", "20210210100000", "20210220100000", "20210301100000")
enddates <- c("20201221100000", "20201231100000", "20210110100000", "20210121100000",
              "20210131100000", "20210210100000", "20210220100000", "20210301100000", "20210310100000")

kenyastar_urls_all <- c()

for(i in 1:length(startdates)){
  
  query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                  "(covid%20OR%20coronavirus)%20",
                  "sourcelang:english%20",
                  "sourcecountry:kenya%20",
                  "domain:kenyastar.com",
                  "&output=urllist&maxrecords=250&format=html",
                  "&startdatetime=", startdates[i],
                  "&enddatetime=", enddates[i])
  
  kenyastar <- read_html(query)
  kenyastar_urls <- kenyastar %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  print(length(kenyastar_urls))
  kenyastar_urls_all <- c(kenyastar_urls_all, kenyastar_urls)
}

kenyastar <- list()
# The East African
for(i in 1:length(kenyastar_urls_all)){
  print(i)
  simple <- tryCatch({read_html(kenyastar_urls_all[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".banner-section") %>%
    html_nodes("h2") %>%
    html_text()
  date <- simple %>%
    html_nodes(".banner-section") %>%
    html_nodes("p") %>%
    html_text()
  date <- date[1]
  text <- simple %>%
    html_nodes(".banner-text") %>%
    html_nodes("p") %>%
    html_text()
  
  kenyastar[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(6)
}

# EastAfrican --------------------------------
startdates <- c("20201210100000", "20201221100000", "20201231100000", "20210110100000", "20210121100000",
                "20210131100000", "20210210100000", "20210220100000", "20210301100000")
enddates <- c("20201221100000", "20201231100000", "20210110100000", "20210121100000",
              "20210131100000", "20210210100000", "20210220100000", "20210301100000", "20210310100000")

eastafrican_urls_all <- c()

for(i in 1:length(startdates)){
  
  query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                  "(covid%20OR%20coronavirus)%20",
                  "sourcelang:english%20",
                  "sourcecountry:kenya%20",
                  "domain:theeastafrican.co.ke",
                  "&output=urllist&maxrecords=250&format=html",
                  "&startdatetime=", startdates[i],
                  "&enddatetime=", enddates[i])
  
  eastafrican <- read_html(query)
  eastafrican_urls <- eastafrican %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  print(length(eastafrican_urls))
  eastafrican_urls_all <- c(eastafrican_urls_all, eastafrican_urls)
}

eastafrican <- list()
# The East African
for(i in 1:length(eastafrican_urls_all)){
  print(i)
  simple <- tryCatch({read_html(eastafrican_urls_all[i])},
    error = function(cond){
    message("URL not found")
    message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
          html_nodes("header") %>%
          html_nodes("h2") %>%
          html_text()
  date <- simple %>%
          html_nodes("header") %>%
          html_nodes("h6") %>%
          html_text()
  text <- simple %>%
          html_nodes(".article") %>%
          html_nodes("p") %>%
          html_text()
  
  eastafrican[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(5)
}

# Standard -----------------------------------------
startdates <- c("20201210100000", "20201221100000", "20201231100000", "20210110100000", "20210121100000",
                "20210131100000", "20210210100000", "20210220100000", "20210301100000")
enddates <- c("20201221100000", "20201231100000", "20210110100000", "20210121100000",
              "20210131100000", "20210210100000", "20210220100000", "20210301100000", "20210310100000")

standard_urls_all <- c()

for(i in 1:length(startdates)){
  
  query <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=",
                  "(covid%20OR%20coronavirus)%20",
                  "sourcelang:english%20",
                  "sourcecountry:kenya%20",
                  "domain:standardmedia.co.ke",
                  "&output=urllist&maxrecords=250&format=html",
                  "&startdatetime=", startdates[i],
                  "&enddatetime=", enddates[i])
  
  standard <- read_html(query)
  standard_urls <- standard %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  print(length(standard_urls))
  standard_urls_all <- c(standard_urls_all, standard_urls)
}

standard <- list()
# The East African
for(i in 1:length(standard_urls_all)){
  print(i)
  simple <- tryCatch({read_html(standard_urls_all[i])},
                     error = function(cond){
                       message("URL not found")
                       message(cond)})
  if(is.null(simple)){next}
  header <- simple %>%
    html_nodes(".articleheading") %>%
    html_text()
  date <- simple %>%
    html_nodes(".greysmall.mt-2.mt-md-2") %>%
    html_text()
  text <- simple %>%
    html_nodes(".content.main_article_section") %>%
    # html_nodes(".container") %>%
    html_nodes("p") %>%
    html_text()
  
  standard[[i]] <- list(
    'header' = header,
    'date' = date,
    'text' = text
  )
  
  Sys.sleep(3)
}

# Combine ----------------------
all_outputs <- list(eastafrican, kenyastar, standard, nairobinews, nairobiwire, businessdailyafrica)
saveRDS(all_outputs, './kenya_covid.rds')

all_outputs <- readRDS('./kenya_covid.rds')
all_text <- c()
for(i in 1:length(all_outputs)){
  paper <- all_outputs[[i]]
  for(j in 1:length(paper)){
    article <- paper[[j]]
    text <- paste(article[[3]], collapse = " ")
    all_text <- c(all_text, text)
  }
}

# Wordcloud -------------------
all_text <- data.frame(id = 1:length(all_text), text = all_text, stringsAsFactors = FALSE)
saveRDS(all_text, './all_text.rds')
tidy_articles <- all_text %>% 
  unnest_tokens(word, text, token = "words") %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  filter(!is.na(word)) %>%
  mutate(word_stem = wordStem(word, language="en"))
saveRDS(tidy_articles, './tidy_articles.rds')

word_counts <- tidy_articles %>%
  count(id, word_stem, sort = TRUE) %>%
  ungroup()

wordcloud_prep <- tidy_articles %>%
  ungroup() %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  filter(count > 100)

wordcloud(words = wordcloud_prep$word,
          freq = wordcloud_prep$count,
          max.words = 100,
          scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))

# LDA ---------------------------
word_counts_lda <- word_counts %>%
  filter(!word_stem %in% c("covid"))

articles_dtm <- word_counts_lda  %>%
  cast_dtm(id, word_stem, n)

result <- FindTopicsNumber(
  articles_dtm,
  topics = seq(from = 2, to = 50, by = 4),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

ke_lda <- LDA(articles_dtm, k = 10, control = list(seed = 77))
saveRDS(ke_lda, 'ke_lda.rds')
ke_lda <- readRDS('./ke_lda.rds')

ke_topics <- tidy(ke_lda, matrix = "beta")
ke_topics

ke_top_terms <- ke_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ke_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

ke_gamma <- tidy(ke_lda, matrix = "gamma")
article_classifications <- ke_gamma %>%
  mutate(document = as.numeric(document)) %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()
table(article_classifications$topic)
hist(article_classifications$gamma)

# Sentiment Analysis ------------
vaccine <- all_text[article_classifications[article_classifications$topic == 1, ]$document, ]

vaccine_sentiment <- vaccine %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

# How many Tweets were positive/negative?
n_pos <- sum(vaccine_sentiment$sentiment>0)
n_neg <- sum(vaccine_sentiment$sentiment<0)
sprintf("Of %0.0f English-language comments on vaccine, %0.0f had a positive sentiment, %0.0f had a negative sentiment, and %0.0f were neutral",
        nrow(vaccine), n_pos, n_neg, (nrow(vaccine) - n_pos - n_neg))

# Distribution of sentiments
vaccine_sentiment %>%
  ggplot(aes(x = sentiment)) +
  geom_histogram(color="darkblue", fill = "lightblue") +
  xlab("Sentiment") + 
  ylab("Count") + 
  ggtitle(sprintf("Vaccine: Sentiment by Comment (n = %0.0f)", nrow(vaccine)))

# Sentence sentiments
sentence_sentiments <- data.frame()
for(i in 1:nrow(vaccine)){
  dat <- vaccine[i, ]
  dat <- dat %>%
    mutate(vaccine_split = get_sentences(text))
  df <- sentiment(dat$vaccine_split)
  df$sentence <- dat$vaccine_split[[1]]
  df$element_id <- i
  sentence_sentiments <- rbind(sentence_sentiments, df)
}

sentence_sentiments %>%
  top_n(10, sentiment) %>%
  .$sentence

sentence_sentiments %>%
  top_n(10, -sentiment) %>%
  .$sentence


vaccine_neg <- sentence_sentiment %>% 
  top_n(100, -sentiment) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- vaccine_neg %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% c(stop_words$word, "covid", "19")) %>%
  filter(!word2 %in% c(stop_words$word, "covid", "19"))

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")


wordcloud_prep <- bigrams_united %>%
  ungroup() %>%
  group_by(bigram) %>%
  summarize(count = n())

wordcloud(words = wordcloud_prep$bigram,
          freq = wordcloud_prep$count,
          max.words = 100,
          scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))

# Sentiment Analysis All ------------
vaccine <- all_text[article_classifications[article_classifications$topic == 1, ]$document, ]
vaccine_sentiment <- vaccine %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

china <- all_text[article_classifications[article_classifications$topic == 2, ]$document, ]
china_sentiment <- china %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

sports <- all_text[article_classifications[article_classifications$topic == 3, ]$document, ]
sports_sentiment <- sports %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

development <- all_text[article_classifications[article_classifications$topic == 4, ]$document, ]
development_sentiment <- development %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

travel <- all_text[article_classifications[article_classifications$topic == 5, ]$document, ]
travel_sentiment <- travel %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

politics <- all_text[article_classifications[article_classifications$topic == 6, ]$document, ]
politics_sentiment <- politics %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

economics <- all_text[article_classifications[article_classifications$topic == 7, ]$document, ]
economics_sentiment <- economics %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

usa <- all_text[article_classifications[article_classifications$topic == 8, ]$document, ]
usa_sentiment <- usa %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

business <- all_text[article_classifications[article_classifications$topic == 9, ]$document, ]
business_sentiment <- business %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

education <- all_text[article_classifications[article_classifications$topic == 10, ]$document, ]
education_sentiment <- education %>%
  mutate(vaccine_split = get_sentences(text)) %>%
  group_by(id) %>%
  mutate(sentiment = mean(sentiment(vaccine_split)$sentiment))

outlist <- list(
  "vaccine_sentiment" = vaccine_sentiment,
  "china_sentiment" = china_sentiment,
  "sports_sentiment" = sports_sentiment,
  "development_sentiment" = development_sentiment,
  "travel_sentiment" = travel_sentiment,
  "politics_sentiment" = politics_sentiment,
  "economics_sentiment" = economics_sentiment,
  "usa_sentiment" = usa_sentiment,
  "business_sentiment" = business_sentiment,
  "education_sentiment" = education_sentiment
)

saveRDS(outlist, "sentiments.rds")
