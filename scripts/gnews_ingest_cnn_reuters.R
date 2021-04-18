# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/scripts")

library(httr)
library(jsonlite)
library(lubridate)
library(rvest)
library(tidyverse)
library(tidytext)

################
# Get API data #
################

# API Documentation: https://documenter.getpostman.com/view/12365554/TVep87Q1#intro

# note: generate your own GNews API token and save it as a file called "api_token"
token <- colnames(read.csv("../api_token"))

# parameters
topic <- "\"COVID-19\""
country <- "us"
language <- "en"
# article limit per API call under the free plan
limit <- "20"
source_c <- "cnn"
source_r <- "reuters"

# date range
date_init <- as.Date("2020-01-01")
date_end <- as.Date("2021-04-09")

get_date_range <- function(date_start, date_end, start_end = "start") {
  # get start or end of week for each date, given range of dates
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

get_api_data <- function(topic, country, language, dates_start, 
                         dates_end, source, limit, token) {
  # API call - one call per week
  # check API page in https://gnewsapi.net/settings#/api to monitor progress
  api_results <- list()
  for (i in seq_along(dates_start)) {
    api_results[[i]] <- get_news(topic, country, language, 
                                 dates_start[i], dates_end[i], 
                                 source, limit, token)
  }
  
  # only include API results that return a valid status code
  api_status_codes <- which(sapply(api_results, function(x) x$status_code) == 200)
  
  # convert to dataframe
  news <- bind_rows(
    lapply(
      api_results[api_status_codes], function(x) {
        as.data.frame(fromJSON(content(x, "text"), flatten = TRUE))
      }
    )
  )
  return(news)
}


# CNN API dataframe
news_cnn <- get_api_data(topic, country, language, dates_start, 
                         dates_end, source = source_c, limit, token)

# Reuters API dataframe
news_reuters <- get_api_data(topic, country, language, dates_start, 
                             dates_end, source = source_r, limit, token)

############################
# Scrape article full text #
############################

get_cnn_text <- function(url)
{
  # pull CNN article text given url.
  html_ <- read_html(url)
  # get first sentence of article text
  df_first <- data.frame(
    text = html_ %>%
      xml_find_all("//p[contains(@class, 'zn-body__paragraph speakable')]") %>%
      html_text(trim = TRUE),
    stringsAsFactors = FALSE
  )
  # get remainder of article text
  df_text <- data.frame(
    text = html_ %>%
      xml_find_all("//div[contains(@class, 'zn-body__paragraph')]") %>%
      html_text(trim = TRUE),
    stringsAsFactors = FALSE
  )
  # get full article text
  final_text <- bind_rows(df_first, df_text)
  return(paste(final_text))
}

get_reuters_text <- function(url) {
  # pull Reuters article text given url
  df <- data.frame(text = read_html(url) %>% 
                     xml_find_all(
                       "//p[contains(@class, 'Paragraph-paragraph-2Bgue ArticleBody-para-TD_9x')]"
                     ) %>%
                     html_text(trim = TRUE),
                   stringsAsFactors = FALSE
  )
  return(paste(df))
}

get_article_text <- function(news_url, source) {
  # fetch full article text given article URL
  # source: either "cnn" or "reuters"
  if (source == "cnn") {
    # full article text
    # this can take time. Find a more fficient way to pull this.
    text <- get_cnn_text(news_url) 
    }
  else if (source == "reuters") {
    text <- get_reuters_text(news_url)
  }
  return(text)
} 

# full article text
# this can take time. Find a more fficient way to pull this.
text_c <- lapply(news_cnn$articles.article_url, get_article_text, source = "cnn")
text_r <- lapply(news_reuters$articles.article_url, get_article_text, source = "reuters")


# add full text to API dataframes
news_cnn$text <- news_reuters$text <- NA
for (i in seq_along(text)) {
  news_cnn$text[i] <- text_c[[i]]
  news_reuters$text[i] <- text_r[[i]]
  }

clean_null_articles <- function(news_df, fill_value = "N/A") {
  # clean up articles that did not return a result
  # news_df: API news dataframe. Assumes full article text column name is "text"
  # fill_value: value to populate null articles. Default = "N/A".
  news_df$text <- if_else(news_df$text == "character(0)", 
                           "N/A", 
                           # remove combine syntax, i.e. "c()"
                           substring(news_df$text, 4, nchar(news_df$text) - 2))
  return(news_df)
}

news_cnn <- clean_null_articles(news_cnn)
news_reuters <- clean_null_articles(news_reuters)

# write to csv
write_csv(news_cnn, file = "../data/news_data_cnn.csv")
write_csv(news_reuters, file = "../data/news_data_reuters.csv")

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

############
# Fox News #
############

# source_f <- "foxnews"
# # Fox API dataframe
# news_fox <- get_api_data(topic, country, language, dates_start, 
#                          dates_end, source = source_f, limit, token)
# 
# article_url <- news_fox$articles.article_url[8]
# 
# get_fox_text <- function(url) {
#  df <- as.data.frame(read_html(url) %>%
#                        xml_find_all("///p[contains(@p, '')]") %>%
#                        html_text(trim = TRUE),
#                      stringsAsFactors = FALSE)
#   return(paste(df))
# }
# 
# 
# text_f <- lapply(news_fox$articles.article_url, get_fox_text)
# 
# # add full text to API dataframes
# news_fox$text <- NA
# for (i in seq_along(text_f)) {
#   news_fox$text[i] <- text_f[[i]]
# }
# 
# clean_null_articles <- function(news_df, fill_value = "N/A") {
#   # clean up articles that did not return a result
#   # news_df: API news dataframe. Assumes full article text column name is "text"
#   # fill_value: value to populate null articles. Default = "N/A".
#   news_df$text <- if_else(news_df$text == "character(0)", 
#                           "N/A", 
#                           # remove combine syntax, i.e. "c()"
#                           substring(news_df$text, 4, nchar(news_df$text) - 2))
#   return(news_df)
# }
# 
# news_fox <- clean_null_articles(news_fox)
# 
# # write to csv
# write_csv(news_fox, file = "../data/news_data_fox.csv")
# # write_csv(news_reuters, file = "../data/news_data_reuters.csv")

#################

# # convert to tidy format
# text_tidy <- text %>% unnest_tokens(word, text)
# 
# # get sentiment dictionary - try the different lexicons and compare
# # https://hoyeolkim.wordpress.com/2018/02/25/the-limits-of-the-bing-afinn-and-nrc-lexicons-with-the-tidytext-package-in-r/
# get_sentiments("bing")[ , 2] %>% distinct()

