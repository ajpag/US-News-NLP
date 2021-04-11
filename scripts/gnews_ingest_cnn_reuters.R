# not run
setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/scripts")

library(httr)
library(jsonlite)
library(lubridate)
library(rvest)
library(tidyverse)
library(tidytext)

# API Documentation: https://documenter.getpostman.com/view/12365554/TVep87Q1#intro

# note: generate your own GNews API token and save it as a file called "api_token"
token <- colnames(read.csv("api_token"))

# parameters
topic <- "\"COVID-19\""
country <- "us"
language <- "en"
# article limit per API call under the free plan
limit <- "20"
date_from <- "2020-03-01"
date_to <- "2020-03-30"
source <- "cnn"

# date range
date_init <- as.Date("2020-01-01")
date_end <- as.Date("2021-04-09")

get_date_range <- function(date_start, date_end, start_end = "start") {
  # gets start or end of week for each date, given range of dates
  # date_start: beginning date
  # date_end: end date
  # start_end: Get first or last date of the week. Values: "start" or "end" 
  dates <- integer(0)
  class(dates) <- "Date"
  i <- 1
  while (date_start <= date_end) {
    if (start_end == "start") 
      {dates[i] <- floor_date(date_start, unit = "week")}
    else 
      {dates[i] <- ceiling_date(date_start, unit = "week") - 1}
    date_start <- date_start + 7
    i <- i + 1
    }
  return(dates)
  }

# date ranges for API
dates_start <- get_date_range(date_init, date_end, start_end = "start")
dates_end <- get_date_range(date_init, date_end, start_end = "end")

# API call
get_news <- function(topic, country, language, date_from, 
                     date_to, source, limit, token) {
  # get GNews API response
  response <- GET(paste0("https://gnewsapi.net/api/search",
                         "?q=", URLencode(topic), 
                         "&country=", country,
                         "&language=", language,
                         "&from=", date_from,
                         "&to=", date_to,
                         "&inurl=", source,
                         "&limit=", limit,
                         "&api_token=", token),
                  type = "basic")
  return(response)
}

# API call - one per week
# check API page in https://gnewsapi.net/settings#/api to monitor progress
api_results <- list()
for (i in seq_along(dates_start)) {
  api_results[[i]] <- get_news(topic, country, language, 
                               dates_start[i], dates_end[i], 
                               source, limit, token)
}

# convert to dataframe
news <- bind_rows(
  lapply(
    api_results, function(x) {
    as.data.frame(fromJSON(content(x, "text"), flatten = TRUE))
      }
    )
  )

####### 
# CNN #
####### 

get_cnn_text <- function(url)
{
  # get first sentence of article text
  df_first <- data.frame(
    text = read_html(url) %>%
      xml_find_all("//p[contains(@class, 'zn-body__paragraph speakable')]") %>%
      html_text(trim = TRUE),
    stringsAsFactors = FALSE
  )
  # get remainder of article text
  df_text <- data.frame(
    text = read_html(url) %>%
      xml_find_all("//div[contains(@class, 'zn-body__paragraph')]") %>%
      html_text(trim = TRUE),
    stringsAsFactors = FALSE
  )
  # get full article text
  final_text <- bind_rows(df_first, df_text)
  return(paste(final_text))
}

# full article text
text <- lapply(news$articles.article_url, get_cnn_text)

# add full text to df
news$text <- NA
for (i in seq_along(text)) {news$text[i] <- text[[i]]}

news$text <- if_else(news$text == "character(0)", 
                     "N/A", 
                     # remove combine syntax, i.e. "c()"
                     substring(news$text, 4, nchar(news$text) - 2))

# write to csv
write_csv(news, file = "../data/news_data_sample.csv")

###########
# Reuters #
###########

article_url <- news$articles.article_url[4]
get_reuters_text <- function(url) {
  df <- data.frame(text = read_html(url) %>% 
                     xml_find_all(
                       "//p[contains(@class, 'Paragraph-paragraph-2Bgue ArticleBody-para-TD_9x')]"
                     ) %>%
                     html_text(trim = TRUE),
                   stringsAsFactors = FALSE
  )
  return(paste(df))
}

get_reuters_text(article_url)

# full article text
text <- lapply(news$articles.article_url, get_reuters_text)

# add full text to df
news$text <- NA
for (i in seq_along(text)) {news$text[i] <- text[[i]]}
# write to csv
write.csv(news, file = "news_data_sample.csv")

#######
# BBC #
#######

# article_url <- news$articles.article_url[1]
# get_bbc_text <- function(url) {
#   df <- data.frame(text = read_html(url) %>% 
#                    xml_find_all(
#                      "//div[contains(@class, 'ssrcss-3z08n3-RichTextContainer e5tfeyi2')]"
#                      ) %>%
#                    html_text(trim = TRUE),
#                  stringsAsFactors = FALSE
#                  )
#   return(df)
# }
# 
# get_bbc_text(article_url)
# 
# # full article text
# text <- bind_rows(lapply(news$articles.article_url, get_bbc_text))

##############
# Al Jazeera #
##############

# article_url <- news$articles.article_url[1]
# get_jazeera_text <- function(url) {
#   df <- data.frame(text = read_html(url) %>% 
#                      xml_find_all(
#                        "//div[contains(@class, 'wysiwyg wysiwyg--all-content')]"
#                      ) %>%
#                      html_text(trim = TRUE),
#                    stringsAsFactors = FALSE
#   )
#   return(df)
# }
# 
# get_jazeera_text(article_url)
# 
# # full article text
# text <- bind_rows(lapply(news$articles.article_url, get_jazeera_text))

#################

# convert to tidy format
text_tidy <- text %>% unnest_tokens(word, text)

# get sentiment dictionary - try the different lexicons and compare
# https://hoyeolkim.wordpress.com/2018/02/25/the-limits-of-the-bing-afinn-and-nrc-lexicons-with-the-tidytext-package-in-r/
get_sentiments("bing")[ , 2] %>% distinct()

