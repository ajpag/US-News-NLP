# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(dplyr)
library(caret) # Gradient Boosting Machine  
library(gbm) # need for feature importance
library(readr)
library(pROC) # multi-class ROC and AUC
seed <- 14
set.seed(seed)

# Purpose: Predict the news source

data_dir <- "../data/"
figures_dir <- "../figures/"

articles <- read_csv(paste0(data_dir, "news_model_input.csv"))

# filter to columns for modeling
articles_input <- articles %>% 
  select(-c(datetime, timestamp, countArticles, articles.article_url,
            articles.title, articles.description, 
            articles.description_with_tag, articles.published_datetime,
            articles.published_timestamp, articles.image_url,
            articles.source_url, articles.source_domain, text,
            word_count, 
            word_count_sentiment, published_hour_et)) %>% 
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
       file = paste0(figures_dir, "avg_word_count.png"))


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

##################################
# GBM Model: Topic Probabilities #
##################################

gbm_train1 <- articles_train %>% 
  select(contains("topic") | 
       c(avg_sentiment_afinn_word, published_dow, articles.source_name))

# fit model
gbm_fit <- train(articles.source_name ~ .,
               data = gbm_train1,
               method = "gbm")

# predictions on test set
gbm_preds <- predict(gbm_fit, articles_test)
# probabilities on test set
gbm_prob <- predict(gbm_fit, articles_test, type = "prob")

###############################
# GBM Model: Keyword Features #
###############################

gbm_train2 <- articles_train %>% 
  select(contains("_sentiment") | c(published_dow, articles.source_name))

# fit model
gbm_fit2 <- train(articles.source_name ~ . - avg_sentiment_afinn_sent,
                 data = gbm_train2,
                 method = "gbm")

# predictions on test set
gbm_preds2 <- predict(gbm_fit2, articles_test)
# probabilities on test set
gbm_prob2 <- predict(gbm_fit2, articles_test, type = "prob")

#####################################
# GBM Model: Topic Keyword Features #
#####################################

gbm_train3 <- articles_train %>% 
  select(contains("_words") | 
           c(published_dow, articles.source_name, covid19,
             scientist, republican, democrat))

# fit model
gbm_fit3 <- train(articles.source_name ~ .,
                  data = gbm_train3,
                  method = "gbm")

# predictions on test set
gbm_preds3 <- predict(gbm_fit3, articles_test)
# probabilities on test set
gbm_prob3 <- predict(gbm_fit3, articles_test, type = "prob")

###########################
# GBM Model: All features #
###########################

gbm_train4 <- articles_train %>% select(-document)

# fit model
gbm_fit4 <- train(articles.source_name ~ .,
                  data = gbm_train4,
                  method = "gbm")

# predictions on test set
gbm_preds4 <- predict(gbm_fit4, articles_test)
# probabilities on test set
gbm_prob4 <- predict(gbm_fit4, articles_test, type = "prob")

######################
# Feature Importance #
######################

# plot variable importance
varImp(gbm_fit)
varImp(gbm_fit2)
varImp(gbm_fit3)
varImp(gbm_fit4)

######################
# Validation Metrics #
######################

gbm_accuracy <- mean(gbm_preds == articles_test$articles.source_name)
gbm_accuracy2 <- mean(gbm_preds2 == articles_test$articles.source_name)
gbm_accuracy3 <- mean(gbm_preds3 == articles_test$articles.source_name)
gbm_accuracy4 <- mean(gbm_preds4 == articles_test$articles.source_name)

paste("GBM Model 1 Accuracy (Topics):", round(gbm_accuracy, 3))
paste("GBM Model 2 Accuracy (Keywords):", round(gbm_accuracy2, 3))
paste("GBM Model 3 Accuracy (Topic Keywords):", round(gbm_accuracy3, 3))
paste("GBM Model 4 Accuracy (All Features):", round(gbm_accuracy4, 3))

#######
# AUC #
#######

# auc
auc_ <- multiclass.roc(articles_test$articles.source_name, gbm_prob)
auc2 <- multiclass.roc(articles_test$articles.source_name, gbm_prob2)
auc3 <- multiclass.roc(articles_test$articles.source_name, gbm_prob3)
auc4 <- multiclass.roc(articles_test$articles.source_name, gbm_prob4)

paste("GBM Model 1 AUC (Topics):", round(auc_$auc, 3))
paste("GBM Model 2 AUC (Keywords):", round(auc2$auc, 3))
paste("GBM Model 3 AUC (Topic Keywords):", round(auc3$auc, 3))
paste("GBM Model 4 AUC (Topic Keywords):", round(auc4$auc, 3))

#############################
# Training dataset accuracy #
#############################

# confusion matrix for training data
cm1 <- as.matrix(caret::confusionMatrix(gbm_fit)$table)
cm2 <- as.matrix(caret::confusionMatrix.train(gbm_fit2)$table)
cm3 <- as.matrix(caret::confusionMatrix.train(gbm_fit3)$table)
cm4 <- as.matrix(caret::confusionMatrix.train(gbm_fit4)$table)

# training accuracy
gbm1_train_acc <- sum(diag(cm1)) / sum(cm1)
gbm2_train_acc <- sum(diag(cm2)) / sum(cm2)
gbm3_train_acc <- sum(diag(cm3)) / sum(cm3)
gbm4_train_acc <- sum(diag(cm4)) / sum(cm4)

# results dataframe
gbm_results <- data.frame(model = c("gbm_topic_lda", "gbm_keywords",
                                    "gbm_topic_keywods", "gbm_all_features"),
                          train_accuracy = round(c(gbm1_train_acc, gbm2_train_acc,
                                             gbm3_train_acc, gbm4_train_acc), 2),
                          test_accuracy = round(c(gbm_accuracy, gbm_accuracy2,
                                       gbm_accuracy3, gbm_accuracy4), 2),
                          auc = round(c(auc_$auc, auc2$auc, auc3$auc, auc4$auc), 2))

# save results
write_csv(gbm_results, paste0(figures_dir, "gbm_results.csv"))

          