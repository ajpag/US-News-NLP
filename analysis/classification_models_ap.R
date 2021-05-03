# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(anytime) # epoch to datetime
library(dplyr)
library(caret) # Gradient Boosting Machine  
library(gbm) # need for feature importance
library(lubridate)
library(readr)
library(pROC) # multi-class ROC and AUC
library(xgboost)
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
gbm_fit <- train(articles.source_name ~ . - articles.published_datetime 
                                          - articles.published_timestamp 
                                          - document
                                          - published_hour_et,
               data = articles_train,
               method = "gbm")

# plot variable importance
varImp(gbm_fit)

# predictions on test set
gbm_preds <- predict(gbm_fit, articles_test)
# probabilities on test set
gbm_prob <- predict(gbm_fit, articles_test, type = "prob")

######################
# Validation Metrics #
######################

gbm_accuracy <- mean(gbm_preds == articles_test$articles.source_name)
paste("GBM Accuracy:", round(gbm_accuracy, 3))

#######
# AUC #
#######

######################
# Feature Importance #
######################


###################### XGBoost #################################################

#############
# Prep data #
#############

# convert class labels to numeric
news_source_unique <- sort(unique(articles$articles.source_name))
class_label <- as.vector(1:length(news_source_unique)) - 1
names(class_label) <- news_source_unique

d <- dummyVars(" ~ .", 
               data = articles_train %>% 
                 select(articles.source_name))

class_label <- data.frame(predict(d, newdata = articles_train %>% select(articles.source_name)))

# convert day of week label to numeric

train_xgb <- xgb.DMatrix(data = as.matrix(
  articles_train %>%
    mutate(published_dow = as.factor(published_dow)) %>% 
    select(-c(articles.source_name, document,
              articles.published_datetime, articles.published_timestamp,
              published_hour_et)
    )
  ),
  label = as.matrix(class_label)
  )

#############
# Model Fit #
#############


# fit model

######################
# Validation Metrics #
######################

#######
# AUC #
#######

######################
# Feature Importance #
######################