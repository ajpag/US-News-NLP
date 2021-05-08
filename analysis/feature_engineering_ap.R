# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(anytime)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(tidytext)
library(topicmodels)
seed <- 14

# directories
figures_dir <- "../figures/"
data_dir <- "../data/"

# read news data
articles <- read_csv(paste0(data_dir, "news_all.csv"))  %>% 
  mutate(document = as.character(row_number()))

# filter out international Reuters sources
news_source_exclude <- c("Reuters Africa", "Reuters Australia", 
                         "Reuters India", "Reuters UK", "Reuters.com")

# remove international Reuters articles
articles <- filter(articles, !articles.source_name %in% news_source_exclude)

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
              names_prefix = "prob_topic_")

# add to dataframe
articles <- articles %>% left_join(lda_probs)

#########
# Gibbs #
#########

# results were very similar to VEM

# # try Gibbs method
# news_lda_g <- LDA(news_dtm, k = topics, method = "Gibbs", 
#                 control = list(seed = seed))
# 
# # plot top terms per topic
# p2 <- plot_lda(news_lda_g, 10, "Topic Modeling (LDA Gibbs): Top Terms")
# 
# # save plot
# ggsave(plot = p2, file = paste0(figures_dir, "lda_top_terms_gibbs.png"), 
#        height = 10, width = 10)
# 
# # add topic probabilities to dataframe 
# # topic probabilities
# lda_probs_g <- tidy(news_lda, matrix = "gamma")
# colnames(lda_probs_g) <- paste0("topic_", colnames(lda_probs_g), "_prob_gibbs")
# 
# # add topic probabilities to dataframe 
# # topic probabilities by document
# lda_probs_g <- tidy(news_lda_g, matrix = "gamma") %>%
#   pivot_wider(names_from = topic, 
#               values_from = gamma,
#               names_prefix = "topic_gibbs_")
# 
# # add to dataframe
# articles <- articles %>% left_join(lda_probs_g)

################################# Sentiment Features ###########################

# Calculate average sentiment per article

##################
# Tokenize Words #
##################

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
  summarise(avg_sentiment_afinn_word = mean(sentiment_afinn)) %>% 
  mutate(document = as.character(document))

# word count per article
word_count <- words %>% 
  group_by(document) %>% 
  mutate(document = as.character(document)) %>% 
  summarise(word_count = n())

# word count with mapped sentiment
word_count_sentiment <- words %>%
  group_by(document) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(document = as.character(document)) %>% 
  summarise(word_count_sentiment = n())

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
  summarise(avg_sentiment_afinn_sent = mean(value)) %>% 
  mutate(document = as.character(document))

# add to dataframe
# also add word counts
articles <- articles %>% 
  left_join(sentences_sentiment) %>% 
  left_join(word_count) %>% 
  left_join(word_count_sentiment)

##################### Plot Features ############################################

# inspect differences between sentiment by word and sentence across data sources
articles %>% 
  pivot_longer(cols = c(avg_sentiment_afinn_word, 
                        avg_sentiment_afinn_sent)) %>% 
  ggplot(aes(x = value, fill = name)) + 
  geom_histogram() + 
  facet_wrap(~articles.source_name) + 
  labs(title = "Sentiment by Article") + 
  theme(legend.position = "bottom")

# plot topic probabilities by news source
p_topic <- articles %>% 
  select(articles.source_name | contains("topic")) %>% 
  pivot_longer(cols = -articles.source_name) %>% 
  drop_na() %>% 
  group_by(name, articles.source_name) %>% 
  summarise(topic_prob = mean(value)) %>% 
  ggplot(aes(x = articles.source_name, y = topic_prob, fill = articles.source_name)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~name) + 
  theme(legend.position = "bottom") + 
  labs(title = "Topic Probabilities by News Source")

ggsave(plot = p_topic, file = paste0(figures_dir, "topic_probabilities_news_source.png"),
       height = 10, width = 10)

############################### TF-IDF #########################################

# drop for now.. this would add too many columns to the dataframe

# # prep data for tf_idf
# word_counts_ <- words %>% 
#   group_by(document, word) %>% 
#   summarise(n = n())
# 
# # get tf-idf
# tf_idf <- bind_tf_idf(word_counts_, document, word, n)

########################## Other features #######################################

# add hour published and day of week published
articles <- articles %>% 
  mutate(published_hour_et = hour(anytime(articles.published_timestamp)), 
         published_dow = weekdays(articles$articles.published_datetime))

# add Jazmine's keyword sentiment features
keyword_features <- read_csv(paste0(data_dir, "sentiment_by_words_new_features.csv"))

# join datasets and remove redundant columns
articles <- articles %>% 
  left_join(keyword_features %>% 
              select(-c(articles.title, text, articles.published_datetime)) %>% 
              mutate(article.no = as.factor(article.no)), 
            by = c("document" = "article.no"))


# ============= these are new features related to other research papers ================ (Shim)

all_news_class <- articles %>%
  mutate(text1 = tolower(text),
         # new features1
         # study by "Politicization and Polarization in COVID-19 News Coverage"
         covid19 = grepl(c("corona", "coronavirus","covid"), text1)*1,
         scientist = grepl(c('scientist', 'research', 'professor', 'health official', 'doctor','dr', 'health commission','expert', 'health leader', 'health service','health authorit', 'world health organization', 'centers for disease control and prevention', 'cdc', 'national institutes of health', 'health and human services', 'mayo clinic', 'johns hopkins' , 'fauci', 'birx', 'tedros'), text1)*1,
         republican = grepl(c("republican","gop", 'conservative', "trump", "pence", "mcconnell","white house","administration"), text1)*1,
         democrat = grepl(c("democrat","liberal","progressive","pelosi" ,"schumer", "biden", "obama","newsom" ,"whitmer","cuomo","biden,","sanders"), text1)*1,
        # new feature 2
        # study name "Polarization in elite communication on the COVID-19 pandemic"
        repub_words = grepl(c("coronavirus","china","businesses","realdonaltrump","relief","inittogether","small","together", "cares","great"), text1)*1,
        demo_words = grepl(c("health","need","crisis","public","workers","trump","must","pandemic","care","leave","paid","familiesfirst","people","sick","emergency"), text1)*1,
        # new feature 3
        # research paper by Pew Research Center
        div_words1 = grepl(c("social distance","gathering","avoid","large groups"), text1)*1,
        div_words2 = grepl(c("limit","carry-out","restaurant","restaurants"), text1)*1,
        div_words3 = grepl(c("closing","close","k-12","school"), text1)*1,
        div_words4 = grepl(c("race","racial","racism","blm"), text1)*1,
        div_words5 = grepl(c("climate","climate change","global warming","global climate change"), text1)*1,
         ) %>% 
  select(-text1)
# ============= ends here ================

# write to csv
write_csv(all_news_class, paste0(data_dir, "news_model_input.csv"))

