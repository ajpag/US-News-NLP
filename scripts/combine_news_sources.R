# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(dplyr)
library(stringr)
library(readr)

# Combine news sources

#######
# CNN #
#######

articles_cnn <- read_csv("../data/news_data_cnn.csv") %>%
  filter(text != "N/A") %>%
  mutate(text = str_remove_all(text, "CNN")) %>%
  mutate(text = str_replace(text, fixed("()"), "")) %>%
  mutate(text = str_replace(text, fixed("( Business)"), "")) %>%
  mutate(text = str_replace_all(text, "[\n]", "")) %>%
  mutate(text = gsub("\\\"", "", text, fixed = TRUE)) %>%
  mutate(text = gsub("\"", "", text, fixed = TRUE))

###############
# Add Reuters #
###############

articles_cnn_reuters <- bind_rows(articles_cnn, 
                                  read_csv("../data/news_data_reuters.csv"))

###############
# WSJ and BBC #
###############

# read wsj
articles_wsj <- read_csv("../data/news_wsj_new.csv") %>% 
  mutate(text = text_new) %>% 
  select(-text_new)
# read bbc
articles_bbc <- read_csv("../data/news_bbc_new.csv") %>% 
  mutate(text = text_new) %>% 
  select(-text_new)

# read wsj and bbc
articles_bbc_wsj <- bind_rows(articles_wsj, articles_bbc)

# remove first three columns and consolidate source names
articles_bbc_wsj <- articles_bbc_wsj %>% 
  select(-c(X1, X.1, X, X.2)) %>% 
  mutate(articles.source_name = 
           if_else(articles.source_name == "Wall Street Journal", 
                   "The Wall Street Journal",
                   articles.source_name))

########################
# Combine news sources #
########################

# combine articles
articles <- bind_rows(articles_cnn_reuters, articles_bbc_wsj)

articles %>% group_by(articles.source_name) %>% summarise(n())

# write file
write_csv(articles, file = "../data/news_all.csv")