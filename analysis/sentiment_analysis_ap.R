# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(reshape2)
library(stringr)
library(tidyr)
library(tidytext)

figures_dir <- "../figures/"

# # read cnn and clean
# articles_cnn <- read_csv("../data/news_data_cnn.csv") %>% 
#   filter(text != "N/A") %>% 
#   mutate(text = str_remove_all(text, "CNN")) %>% 
#   mutate(text = str_replace(text, fixed("()"), "")) %>% 
#   mutate(text = str_replace(text, fixed("( Business)"), "")) %>% 
#   mutate(text = str_replace_all(text, "[\n]", "")) %>% 
#   mutate(text = gsub("\\\"", "", text, fixed = TRUE)) %>% 
#   mutate(text = gsub("\"", "", text, fixed = TRUE))

# read cnn and reuters. Drop International Reuters news sources
articles_cnn_reuters <- bind_rows(read_csv("../data/news_data_cnn.csv"), 
                                  read_csv("../data/news_data_reuters.csv") %>% 
                                    filter(
                                      (articles.source_domain == "www.reuters.com") 
                                      & 
                                      (articles.source_name == "Reuters"))
)

# read wsj and bbc
articles_bbc_wsj <- bind_rows(read_csv("../data/news_wsj.csv"),
                              read_csv("../data/news_bbc.csv"))

# remove first three columns and consolidate source names
articles_bbc_wsj <- articles_bbc_wsj %>% 
  select(-c(X1, X.1, X)) %>% 
  mutate(articles.source_name = 
           if_else(articles.source_name == "Wall Street Journal", 
                   "The Wall Street Journal",
                   articles.source_name))

# combine articles
articles <- bind_rows(articles_cnn_reuters, articles_bbc_wsj)

###############################
# Baseline: Sentiment by Word #
###############################

# number of articles by source
articles %>%
  group_by(articles.source_name) %>% 
  summarise(articles = n()) %>% 
  arrange(desc(articles))

tokenize_words <- function(df, text) {
  # tokenize words and add sentiments given news dataframe
  # text: column to tokenize
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
words <- tokenize_words(articles, text)

# words per article
p1 <- words %>% 
  group_by(articles.source_name, articles.article_url) %>% 
  summarise(word_count = n()) %>% 
  ggplot(aes(x = word_count, fill = articles.source_name)) +
  geom_histogram(position = "dodge") + 
  labs(title = "Word Count Distribution") + 
  theme(legend.position = "bottom")

# re-shape for plotting
words_long <- words %>% 
  pivot_longer(cols = contains("sentiment"), 
               names_to = "lexicon", 
               values_to = "sentiment")

# top words with a sentiment
p2 <- words_long %>% 
  select(word, sentiment, articles.source_name) %>% 
  drop_na() %>% 
  group_by(word, articles.source_name) %>% 
  summarise(count = n()) %>%
  group_by(articles.source_name) %>% 
  mutate(pct_of_total = count / sum(count)) %>% 
  arrange(desc(pct_of_total)) %>% 
  head(100) %>% 
  ggplot(aes(x = reorder(word, pct_of_total), y = pct_of_total)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  facet_wrap(~articles.source_name, scales = "free") +
  labs(title = "Top Words (with a sentiment)") + 
  theme(legend.position = "bottom")

# plot
p3 <- words_long %>% 
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
p4 <- words_long %>% 
  drop_na() %>% 
  mutate(date = floor_date(articles.published_datetime, unit = "week"),
         sentiment = as.numeric(sentiment)) %>% 
  filter(lexicon == "sentiment_afinn") %>% 
  group_by(date, articles.source_name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  ggplot(aes(x = date, y = avg_sentiment, color = articles.source_name)) +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Average Sentiment by Week") + 
  theme(legend.position = "bottom")

#######################################
# Baseline: Sentiment by Word TF-IDF #
#######################################

# term frequency
p5 <- words %>% 
  group_by(word, articles.source_name) %>% 
  summarise(word_count = n()) %>% 
  group_by(articles.source_name) %>% 
  mutate(total_words = sum(word_count)) %>% 
  ggplot(aes(x = word_count / total_words)) +
  geom_histogram() + 
  facet_wrap(~articles.source_name) + 
  labs(title = "Word Frequency Distribution", x = "pct of Words")

# tf-idf (for words with a sentiment)
# create custom stop words to remove meaningless words
# reference: https://www.tidytextmining.com/tfidf.html
stop_words_custom <- tibble(word = c("n", "2w7hx9t"))

words_tf_idf <- words_long %>% 
  anti_join(stop_words_custom) %>% 
  group_by(word, articles.article_url, articles.source_name) %>% 
  summarise(word_count = n()) %>% 
  bind_tf_idf(word, articles.article_url, word_count)

# Highest words by tf_idf
p6 <- words_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  head(70) %>% 
  ggplot(aes(x = reorder(word, tf_idf), 
             y = tf_idf, 
             fill = articles.source_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_wrap(~articles.source_name, scales = "free") + 
  labs(title = "TF-IDF: Top Words", x = "word") + 
  theme(legend.position = "bottom")

##########################
# Sentiment by Sentences #
##########################

# Get sentiment on articles looking at sentence context instead of just words

sentences <- articles %>% 
  mutate(article_num = row_number()) %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% 
  mutate(sentence_num = row_number())

# add sentiment for each word
sentence_sentiment <- sentences %>% 
  unnest_tokens(word, sentence, drop = FALSE) %>% 
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

# convert to long format for plotting
sentence_long <- sentence_sentiment %>% 
  pivot_longer(cols = contains("sentiment"), 
               names_to = "lexicon", 
               values_to = "sentiment")

# average afinn sentiment by sentence
avg_sentiment_afinn <- sentence_long %>% 
  filter(lexicon == "sentiment_afinn") %>% 
  mutate(articles.published_datetime = date(articles.published_datetime)) %>% 
  drop_na() %>% 
  group_by(articles.source_name, articles.published_datetime, 
           sentence_num) %>% 
  summarise(avg_sentiment_afinn = mean(as.numeric(sentiment)))

# plot
p7 <- avg_sentiment_afinn %>% 
  group_by(articles.source_name) %>% 
  summarise(avg_sentiment_afinn = mean(avg_sentiment_afinn)) %>% 
  ggplot(aes(x = articles.source_name, y = avg_sentiment_afinn)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Afinn Sentiment by News Source",
       subtitle = "By Sentence")

# plot by day
p8 <- avg_sentiment_afinn %>% 
  group_by(articles.source_name, 
           week = floor_date(articles.published_datetime, "week")) %>% 
  summarise(avg_sentiment_afinn = mean(avg_sentiment_afinn)) %>% 
  ggplot(aes(x = week, y = avg_sentiment_afinn)) +
  geom_line() +
  facet_wrap(~articles.source_name) + 
  labs(title = "Average Afinn Sentiment by News Source",
       subtitle = "By Sentence")

##############
# save plots #
##############
plot_titles <- c("words_per_article", "top_words_with_sentiment",
                 "word_sentiment_distr", "word_avg_sentiment_by_week_afinn",
                 "word_freq_distr", "word_tfidf_top_words",
                 "sentence_afinn_sentiment", "sentence_afinn_sentiment_week")

plots <- list(p1, p2, p3, p4, p5, p6, p7, p8)

for (i in seq_along(plot_titles)) {
  p <- plots[[i]]
  ggsave(plot = p, file = paste(figures_dir,
                                plot_titles[i],
                                ".png",
                                sep = ""), 
         height = 10, width = 10)
}

#############################
# Sentence Sentiment: Trump #
#############################

# Look at sentiment comparison filtered by articles with "Trump" keyword

#############################
# Sentence Sentiment: Biden #
#############################

# Look at sentiment comparison filtered by articles with "Biden" keyword

##################
# Topic Modeling #
##################

# Engineer new features for sentiment charts and classification model

##################
# Classification #
##################

# Labels: Liberal, Conservative, Moderate
# GBM, BART

