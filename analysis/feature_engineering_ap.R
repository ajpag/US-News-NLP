# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tidytext)
library(topicmodels)
seed <- 14

# directories
figures_dir <- "../figures/"
data_dir <- "../data/"

# read news data
articles <- read_csv(paste0(data_dir, "news_all.csv"))

# Purpose: Engineer new features for classification modeling

################################ Topic Modeling ################################

# also removed top COVID-19 high-frequency words
# the articles were already filtered to COVID-19 articles via GNews API
stop_words_custom <- tibble(word = c("n", "2w7hx9t", "202f", "comma",
                                     "covid", "19", "coronavirus", 
                                     "virus", "health", "people",
                                     "bbc", "reuters", "cnn", "wsj"))

# word frequencies
term_freq <- articles %>% 
  mutate(document = row_number()) %>% 
  unnest_tokens(word, text) %>%
  group_by(document, word) %>% summarise(n = n()) %>% 
  anti_join(stop_words, by = c(word = "word")) %>% 
  anti_join(stop_words_custom, by = c(word = "word"))

# document-term matrix
news_dtm <- term_freq %>% cast_dtm(document, word, n)

#######
# VEM #
#######

# fit a topic model, VEM method
# try different k and fitting methods
topics <- 7
news_lda <- LDA(news_dtm, k = topics, control = list(seed = seed))

# plot top terms
plot_lda <- function(lda_df, n_terms = 10, plot_title = "") {
  # Plots top n terms from an LDA object
  # lda_df: top term
  # n_terms: number of top terms to plot per topic
  
  # per-topic per-word probabilities
  news_topics <- tidy(lda_df, matrix = "beta")
  
  top_terms <- news_topics %>%
    group_by(topic) %>%
    top_n(n_terms, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # plot top terms
  p <- top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", ncol = 2) +
    coord_flip() + 
    labs(title = plot_title)
  
  return(p)
}


# plot top terms per topic
p1 <- plot_lda(news_lda, 10, "Topic Modeling (LDA VEM): Top Terms")

# save plot
ggsave(plot = p1, file = paste0(figures_dir, "lda_top_terms.png"), 
       height = 10, width = 10)

# add topic probabilities to dataframe 
# topic probabilities by document
lda_probs <- tidy(news_lda, matrix = "gamma") %>%
  pivot_wider(names_from = topic, 
              values_from = gamma,
              names_prefix = "topic_vem_")

# add to dataframe
articles <- articles %>% left_join(lda_probs)

#########
# Gibbs #
#########

# try Gibbs method
news_lda_g <- LDA(news_dtm, k = topics, method = "Gibbs", 
                control = list(seed = seed))

# plot top terms per topic
p2 <- plot_lda(news_lda_g, 10, "Topic Modeling (LDA Gibbs): Top Terms")

# save plot
ggsave(plot = p2, file = paste0(figures_dir, "lda_top_terms_gibbs.png"), 
       height = 10, width = 10)

# add topic probabilities to dataframe 
# topic probabilities
lda_probs_g <- tidy(news_lda, matrix = "gamma")
colnames(lda_probs_g) <- paste0("topic_", colnames(lda_probs_g), "_prob_gibbs")

# add topic probabilities to dataframe 
# topic probabilities by document
lda_probs_g <- tidy(news_lda_g, matrix = "gamma") %>%
  pivot_wider(names_from = topic, 
              values_from = gamma,
              names_prefix = "topic_gibbs_")

# add to dataframe
articles <- articles %>% left_join(lda_probs_g)

################################# Sentiment Features ###########################

# Calculate average sentiment per article

##################
# Tokenize Words #
##################

# add row number for each article
articles <- articles %>% mutate(document = row_number())

tokenize_words <- function(df, text) {
  # tokenize text by word and add sentiments
  # text: column to tokenize
  words <- df %>% 
    unnest_tokens(word, text) %>% 
    left_join(get_sentiments(lexicon = "bing") %>% 
                mutate(sentiment_bing = sentiment) %>% 
                select(-sentiment)) %>% 
    left_join(get_sentiments(lexicon = "afinn") %>% 
                mutate(sentiment_afinn = value) %>% 
                select(-value)) %>% 
    left_join(get_sentiments(lexicon = "loughran") %>% 
                mutate(sentiment_loughran = sentiment) %>% 
                select(-sentiment)) %>% 
    left_join(get_sentiments(lexicon = "nrc") %>% 
                mutate(sentiment_nrc = sentiment) %>% 
                select(-sentiment))
  return(words)
}

#####################
# Sentiment by word #
#####################

# tokenize text
words <- tokenize_words(articles, text)

# calculate average afinn sentiment per word and article
avg_sentiment_afinn_word <- words %>% 
  select(document, sentiment_afinn) %>%
  drop_na() %>% 
  group_by(document) %>% 
  summarise(avg_sentiment_afinn_word = mean(sentiment_afinn))

# add to dataframe
articles <- articles %>% left_join(avg_sentiment_afinn_word)

#########################
# Sentiment by sentence #
#########################

# tokenize by sentence and word
sentences <- articles %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% 
  group_by(document) %>% 
  mutate(sentence_num = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, sentence)

# add average sentiment by sentence
sentences_sentiment <- sentences %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(document) %>% 
  summarise(avg_sentiment_afinn_sent = mean(value))

# add to dataframe
articles <- articles %>% left_join(sentences_sentiment)

# inspect differences between sentiment by word and sentence across data sources
articles %>% 
  pivot_longer(cols = c(avg_sentiment_afinn_word, 
                        avg_sentiment_afinn_sent)) %>% 
  ggplot(aes(x = value, fill = name)) + 
  geom_histogram() + 
  facet_wrap(~artic)
  labs(title = "Sentiment by Article") + 
  theme(legend.position = "bottom")

# plot average sentiment by topic and news source

  
# write to csv
write_csv(articles, paste0(data_dir, "news_model_input.csv"))

