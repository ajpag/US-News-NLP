# not run
setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

# library(anytime) # epoch to datetime
library(dplyr)
library(caret) # Gradient Boosting Machine  
library(gbm) # need for feature importance
library(lubridate)
library(readr)
library(pROC) # multi-class ROC and AUC
library(tidyr)
seed <- 14
set.seed(seed)

# Purpose: Predict the news source

data_dir <- "../data/"
figures_dir <- "../figures/"

articles <- read_csv(paste0(data_dir, "news_model_input.csv"))

# filter to columns for modeling
articles_input <- articles %>% 
  select(c(document, articles.source_name, avg_sentiment_afinn_word, 
           articles.published_timestamp, articles.published_datetime,
           avg_sentiment_afinn_sent, word_count, word_count_sentiment,
           published_hour_et, published_dow) | 
           contains("topic")) %>% 
  drop_na()

####################### Assess Bias in Across News Sources #####################

########################################
# Chi-Square Test: Topic Probabilities #
########################################

# Assess bias in topic probabibilities
# rescale to integers for Chi-Square test
articles_topic_prob_sc <- articles %>% 
  select(articles.source_name | contains("topic")) %>% 
  group_by(articles.source_name) %>% 
  drop_na() %>% 
  summarise(round((across(contains("prob"), mean))*100, 0))

# convert to matrix
articles_sc_mat <- articles_topic_prob_sc %>% 
  select(-articles.source_name) %>% 
  as.matrix

# provide row names
rownames(articles_sc_mat) <- articles_topic_prob_sc$articles.source_name
# convert to table
articles_sc_mat <- as.table(articles_sc_mat)

# See "topic_probabilities_news_source.png" in /figures to visualize avg probabilities

# conduct chi-square test
print(chisq.test(articles_sc_mat))

##########################################
# Chi-Square Test: Sentiment by Sentence #
##########################################

# Assess bias in average sentiment
# rescale to non-negative integers for Chi-Square test
articles_sentiment_sc <- articles %>% 
  select(articles.source_name, avg_sentiment_afinn_sent) %>% 
  group_by(articles.source_name) %>% 
  drop_na() %>% 
  summarise(avg_sentiment = mean(avg_sentiment_afinn_sent)) %>% 
  mutate(avg_sentiment_sc = -round(avg_sentiment * 100, 0))

# See "sentence_afinn_sentiment.png" in /figures to visualize avg sentiment

# convert to matrix
articles_sentiment_sc_mat <- articles_sentiment_sc %>% 
  select(avg_sentiment_sc) %>% 
  as.matrix

# provide row names
rownames(articles_sentiment_sc_mat) <- articles_sentiment_sc$articles.source_name
# convert to table
articles_sentiment_sc_mat <- as.table(articles_sentiment_sc_mat)

# conduct chi-square test
print(chisq.test(articles_sentiment_sc_mat))

###############################
# Chi-Square Test: Word Count #
###############################

articles_word_count <- articles %>% 
  select(articles.source_name, word_count) %>% 
  group_by(articles.source_name) %>% 
  summarise(avg_word_count = mean(word_count)) %>% 
  mutate(avg_word_count_chi = round(avg_word_count, 0))

# plot average word count
p1 <- articles_word_count %>% 
  ggplot(aes(x = articles.source_name, y = avg_word_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Word Count per Article")

# save plot  
ggsave(plot = p1, 
       file = paste0(figures_dir, "avg_word_count.png"), 
       width = 10, height = 10)


# convert to matrix
articles_word_count_mat <- as.matrix(articles_word_count$avg_word_count_chi)
rownames(articles_word_count_mat) <- articles_word_count$articles.source_name

# chi-square test
chisq.test(articles_word_count_mat)

############# Classification Modeling: Gradient Boosting Machine ###############

######################
# Train / Test Split #
######################

# train / test split
test_rate <- .2
articles_train <- articles_input %>% sample_frac(size = 1 - test_rate)
articles_test <- articles_input[-c(pull(articles_train %>% select(document))), ]

# inspect distribution of news source
# make sure there is no imbalance across news sources
bind_rows(articles_train %>% mutate(set = "train"),
          articles_test %>% mutate(set = "test")) %>% 
  group_by(articles.source_name, set) %>% 
  summarise(article_count = n()) %>% 
  ggplot(aes(x = articles.source_name, y = article_count)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~set)

#################
# GBM Model Fit #
#################

# fit model
gbm_fit <- train(articles.source_name ~ . - articles.published_datetime 
                                          - articles.published_timestamp 
                                          - document
                                          - published_hour_et,
               data = articles_train,
               method = "gbm")

# predictions on test set
gbm_preds <- predict(gbm_fit, articles_test)
# probabilities on test set
gbm_prob <- predict(gbm_fit, articles_test, type = "prob")

######################
# Feature Importance #
######################

# plot variable importance
varImp(gbm_fit)

######################
# Validation Metrics #
######################

gbm_accuracy <- mean(gbm_preds == articles_test$articles.source_name)
paste("GBM Accuracy:", round(gbm_accuracy, 3))

#######
# AUC #
#######

# auc
multiclass.roc(articles_test$articles.source_name, as.vector(gbm_prob))
#auc_test <- performance(rocr_test, measure = "auc")@y.values[[1]]

paste("Logistic Regression AUC:", auc_test)
