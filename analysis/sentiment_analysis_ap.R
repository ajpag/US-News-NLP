# not run
# getwd("C:\Users\apagta950\Documents\NYU\Courses\Spring 2021\MDML\Final Project\US-News-NLP\analysis")

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(tidytext)

# read cnn
cnn <- read_csv("../data/news_data_cnn.csv")
reuters <- read_csv("../data/news_data_reuters.csv")

#####################
# Sentiment by Word #
#####################

tokenize_words <- function(df) {
  # tokenize words and add sentiments given news dataframe
  words <- df %>% 
    unnest_tokens(word, text) %>% 
    left_join(get_sentiments(lexicon = "bing") %>% 
                mutate(sentiment_bing = sentiment) %>% 
                select(-sentiment)) %>% 
    left_join(get_sentiments(lexicon = "afinn") %>% 
                mutate(sentiment_afinn = as.factor(value)) %>% 
                select(-value)) %>% 
    left_join(get_sentiments(lexicon = "loughran") %>% 
                mutate(sentiment_loughran = sentiment) %>% 
                select(-sentiment)) %>% 
    left_join(get_sentiments(lexicon = "nrc") %>% 
                mutate(sentiment_nrc = sentiment) %>% 
                select(-sentiment))
  return(words)
}

# baseline: tokenize words
words <- bind_rows(tokenize_words(cnn), tokenize_words(reuters))

# re-shape for plotting
words_long <- words %>% 
  pivot_longer(cols = contains("sentiment"), 
               names_to = "lexicon", 
               values_to = "sentiment")

words_long %>% 
  drop_na() %>% 
  ggplot(aes(x = sentiment, fill = articles.source_name)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(~lexicon, scales = "free")

##########################
# Sentiment by Sentences #
##########################

