# not run
# getwd("C:\Users\apagta950\Documents\NYU\Courses\Spring 2021\MDML\Final Project\US-News-NLP\analysis")

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(tidytext)

# read cnn and reuters. Drop International Reuters news sources
articles <- bind_rows(read_csv("../data/news_data_cnn.csv"), 
                      read_csv("../data/news_data_reuters.csv") %>% 
                        filter(
                          (articles.source_domain == "www.reuters.com") & 
                          (articles.source_name == "Reuters"))
                      )

###############################
# Baseline: Sentiment by Word #
###############################

# number of articles by source
articles %>%
  group_by(articles.source_domain, articles.source_name) %>% 
  summarise(articles = n()) %>% 
  arrange(desc(articles))

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
words <- tokenize_words(articles)

# words per article
words %>% 
  group_by(articles.source_name, articles.article_url) %>% 
  summarise(word_count = n()) %>% 
  ggplot(aes(x = word_count, fill = articles.source_name)) +
  geom_histogram(position = "dodge") + 
  labs(title = "Word Count Distribution")

# top 20 words
words_long %>% 
  select(word, sentiment, lexicon) %>% 
  drop_na() %>% 
  group_by(word, articles.) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(word, -count), y = count)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Word Sentiment")
  
# re-shape for plotting
words_long <- words %>% 
  pivot_longer(cols = contains("sentiment"), 
               names_to = "lexicon", 
               values_to = "sentiment")

# plot
words_long %>%
  drop_na() %>% 
  ggplot(aes(x = sentiment, fill = articles.source_name)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(~lexicon, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Word Sentiment")

##########################
# Sentiment by Sentences #
##########################

# Get sentiment on articles looking at broader context instead of just words

##################
# Topic Modeling #
##################

# Engineer new features for sentiment charts and classification model

##################
# Classification #
##################

# Labels: Liberal, Conservative, Moderate
# GBM, BART

