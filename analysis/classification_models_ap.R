# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(anytime) # epoch to datetime
library(bartCause) # Bayesian Additive Regression Trees
library(dplyr)
library(caret) # Gradient Boosting Machine  
library(gbm) # Gradient Boosting Machine  
library(lubridate)
library(readr)
library(ROCR)
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
           avg_sentiment_afinn_sent, word_count, word_count_sentiment) | 
           contains("topic")) %>% 
  mutate(published_hour_et = hour(anytime(articles.published_timestamp)),
         published_dow = weekdays(articles$articles.published_datetime)) %>% 
  drop_na()

################### Train / Test Split #########################################

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

###################### Gradient Boosted Machine ################################

#############
# Model Fit #
#############

# fit model
gbm_fit <- train(articles.source_name ~ . - articles.published_datetime - 
                                        articles.published_timestamp - document,
               data = articles_train,
               method = "gbm")

# predictions on test set
gbm_preds <- predict(gbm_fit, articles_test)

# fit model
# multi-class is ill-advised per documentation
gbm_fit <- gbm(articles.source_name ~ . - articles.published_datetime - 
                   articles.published_timestamp - document,
                 data = articles_train %>% 
                 mutate(published_dow = as.factor(published_dow)),
                 distribution = "multinomial")


######################
# Validation Metrics #
######################

#######
# AUC #
#######

######################
# Feature Importance #
######################


###################### Bayesian Additive Regress Trees #########################

######################
# Validation Metrics #
######################

#######
# AUC #
#######

######################
# Feature Importance #
######################