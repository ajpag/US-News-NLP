# not run
setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(reshape2)
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
  group_by(articles.source_name) %>% 
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

# re-shape for plotting
words_long <- words %>% 
  pivot_longer(cols = contains("sentiment"), 
               names_to = "lexicon", 
               values_to = "sentiment")

# top words with a sentiment
words_long %>% 
  select(word, sentiment, articles.source_name) %>% 
  drop_na() %>% 
  group_by(word, articles.source_name) %>% 
  summarise(count = n()) %>%
  group_by(articles.source_name) %>% 
  mutate(pct_of_total = count / sum(count)) %>% 
  arrange(desc(pct_of_total)) %>% 
  head(30) %>% 
  ggplot(aes(x = reorder(word, pct_of_total), y = pct_of_total)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  labs(title = "Top Words (with a sentiment)")

# plot
words_long %>% 
  group_by(lexicon, sentiment, articles.source_name) %>%
  drop_na() %>% 
  summarise(count = n()) %>% 
  group_by(lexicon, articles.source_name) %>% 
  mutate(pct_of_total = count / sum(count)) %>% 
  ggplot(aes(x = sentiment, 
             y = pct_of_total, 
             fill = articles.source_name)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~lexicon, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  labs(title = "Sentiment Distribution by Word")

# sentiment by week - afinn
words_long %>% 
  drop_na() %>% 
  mutate(date = floor_date(articles.published_datetime, unit = "week"),
         sentiment = as.numeric(sentiment)) %>% 
  filter(lexicon == "sentiment_afinn") %>% 
  group_by(date, articles.source_name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  ggplot(aes(x = date, y = avg_sentiment, color = articles.source_name)) +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Average Sentiment by Week")


#######################################
# Baseline: Sentiment by Word TF-IDF #
#######################################

# term frequency
words %>% 
  group_by(word, articles.source_name) %>% 
  summarise(word_count = n()) %>% 
  group_by(articles.source_name) %>% 
  mutate(total_words = sum(word_count)) %>% 
  ggplot(aes(x = word_count / total_words)) +
  geom_histogram() + 
  facet_wrap(~articles.source_name) + 
  labs(title = "Word Frequency Distribution", x = "pct of Words") 

# tf-idf (for words with a sentiment)
# may need to create custom stop words to remove meaningless words
# reference: https://www.tidytextmining.com/tfidf.html
stop_words_custom <- tibble(word = c("n"))

words_tf_idf <- words_long %>% 
  anti_join(stop_words_custom) %>% 
  group_by(word, articles.article_url, articles.source_name) %>% 
  summarise(word_count = n()) %>% 
  bind_tf_idf(word, articles.article_url, word_count)

# Highest words by tf_idf
words_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  head(100) %>% 
  ggplot(aes(x = reorder(word, tf_idf), 
             y = tf_idf, 
             fill = articles.source_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()+ 
  labs(title = "TF-IDF: Top Words", x = "word") + 
  theme(legend.position = "bottom")

##########################
# Sentiment by Sentences #
##########################

# Get sentiment on articles looking at sentence context instead of just words

sentences <- articles %>% 
  unnest_tokens(sentence, text, token = "sentences")


##################
# Topic Modeling #
##################

# Engineer new features for sentiment charts and classification model

##################
# Classification #
##################

# Labels: Liberal, Conservative, Moderate
# GBM, BART

