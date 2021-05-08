# not run
# setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

library(dplyr)
library(caret) # Gradient Boosting Machine  
library(gbm) # need for feature importance
library(nnet) # multinomial regression
library(ranger) # random forest
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

####################### Assess Bias Across News Sources #####################

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

# remove redundant features (used in "_sentiment" features)
articles_input <- articles_input %>% select(-avg_sentiment_afinn_sent)

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
gbm_fit2 <- train(articles.source_name ~ .,
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

######################################################
#          prediction: logistic regression
######################################################

#================= 2. logistic regression =================
formula <- 'articles.source_name ~ prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + avg_sentiment_afinn_word + published_dow +Biden_sentiment + Trump_sentiment + stock_market_sentiment + financial_sentiment + death_sentiment + pandemic_sentiment + disease_sentiment + illness_sentiment + covid19 + scientist + republican+ democrat + repub_words + demo_words + div_words1 + div_words2 + div_words3 + div_words4 + div_words5'

# Fit the model
model <- nnet::multinom(formula, data = articles_train)

# Make predictions
predicted.classes <- predict(model, newdata=articles_test) #, type="prob"
head(predicted.classes)

# confusion matrix
#table(predicted.classes , test$articles.source_name )
# confusionMatrix(factor(predicted.classes), factor(test$articles.source_name) )

# Model accuracy (overall)
lr_all_acc <- length( articles_test$articles.source_name[predicted.classes==articles_test$articles.source_name] ) / length(articles_test$articles.source_name )

# AUC
predicted.classes <- as.ordered(predicted.classes)
predicted.classes.all <- predict(model, newdata=articles_test, type="prob")
# predicted.classes.bbc <- as.numeric( predict(model, newdata=articles_test, type="prob")[,1] ) # bbc
# predicted.classes.cnn <- as.numeric(predict(model, newdata=articles_test, type="prob")[,2] )  # cnn
# predicted.classes.re <- as.numeric(predict(model, newdata=articles_test, type="prob")[,3] )   # re
# predicted.classes.wsj <- as.numeric(predict(model, newdata=articles_test, type="prob")[,4] )  # wsj
# print
lr_all_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.all)$auc[1]
# bbc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.bbc)$auc[1]
# cnn <- multiclass.roc(articles_test$articles.source_name, predicted.classes.cnn)$auc[1]
# re <- multiclass.roc(articles_test$articles.source_name, predicted.classes.re)$auc[1]
# wsj <- multiclass.roc(articles_test$articles.source_name, predicted.classes.wsj)$auc[1]
# print result
paste("AUC of all is:", lr_all_auc)
# paste("AUC of BBC is:", bbc)
# paste("AUC of CNN is:", cnn)
# paste("AUC of Reuters is:", re)
# paste("AUC of WSJ is:", wsj)


######################################################
#          prediction : Random Forest
######################################################

formula1 <- 'articles.source_name ~ prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 +avg_sentiment_afinn_word +Biden_sentiment + Trump_sentiment + stock_market_sentiment + financial_sentiment + death_sentiment + pandemic_sentiment + disease_sentiment + illness_sentiment + covid19 + scientist + republican+ democrat + repub_words + demo_words + div_words1 + div_words2 + div_words3 + div_words4 + div_words5'

rf_model <- ranger(formula = formula1,
                    num.trees=1000,
                    respect.unordered.factors=T, 
                    probability=T,
                    data=articles_train)
    
# Predict the testing set with the trained model
predictions2 <- predict(rf_model, articles_test, type ="response")
probabilities <- as.data.frame(predict(rf_model, data=articles_test)$predictions)
head(probabilities)

#pre-process
predict_class <- data.frame(max.col(probabilities) )
colnames(predict_class) <- "class"
predict_class <- predict_class %>%
  mutate( class = case_when(class==1 ~ "BBC News",
                            class==2 ~ "CNN",
                            class==3 ~ "Reuters",
                            class==4 ~ "The Wall Street Journal")
                  )
predict_class$class <- as.factor(predict_class$class)

# confusion matrix
rf_cm <- confusionMatrix(table(articles_test$articles.source_name, predict_class$class))$table

rf_all_acc <- sum(diag(rf_cm)) / sum(rf_cm)

# prediction and performance report
modeling_data_rf <- articles_test %>% 
  mutate(probability = probabilities[,2],
         #predict = max.col(probabilities)-1
         predict = predict_class$class         )
modeling_data_rf <- as.data.frame(modeling_data_rf)

# AUC
predicted.classes.all <- probabilities
# predicted.classes.bbc <- probabilities[,1]  # bbc
# predicted.classes.cnn <- probabilities[,2] # cnn
# predicted.classes.re <-  probabilities[,3]  # re
# predicted.classes.wsj <- probabilities[,4] # wsj
rf_all_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.all)$auc[1]
# bbc.rf <- multiclass.roc(test$articles.source_name, predicted.classes.bbc)$auc[1]
# cnn.rf <- multiclass.roc(test$articles.source_name, predicted.classes.cnn)$auc[1]
# re.rf <- multiclass.roc(test$articles.source_name, predicted.classes.re)$auc[1]
# wsj.rf <- multiclass.roc(test$articles.source_name, predicted.classes.wsj)$auc[1]

# print result
paste("AUC of all (RF) is:", rf_all_auc)
# paste("AUC of BBC (RF) is:", bbc.rf)
# paste("AUC of CNN (RF) is:", cnn.rf)
# paste("AUC of Reuters (RF) is:", re.rf)
# paste("AUC of WSJ (RF) is:", wsj.rf)

######################################################
#    prediction of "political_class" : liberal, middle, and conservative
#    Using Logistic-regression
######################################################

train.new <- articles_train %>% 
  # select(-datetime, -timestamp, -countArticles, -articles.article_url, -articles.description, -articles.description_with_tag, -articles.published_timestamp,  - articles.image_url, -articles.source_url, -articles.source_domain , -document) %>%
  mutate(political_class = as.factor(case_when(articles.source_name=="BBC News" ~ "liberal",
                                     articles.source_name=="CNN" ~ "liberal",
                                     articles.source_name=="Reuters" ~ "middle",
                                     articles.source_name=="The Wall Street Journal" ~ "conservative")))
         
test.new <- articles_test %>% 
  # select(-datetime, -timestamp, -countArticles, -articles.article_url, -articles.description, -articles.description_with_tag, -articles.published_timestamp,  - articles.image_url, -articles.source_url, -articles.source_domain , -document) %>%
  mutate(political_class = as.factor(case_when(articles.source_name=="BBC News" ~ "liberal",
                                     articles.source_name=="CNN" ~ "liberal",
                                     articles.source_name=="Reuters" ~ "middle",
                                     articles.source_name=="The Wall Street Journal" ~ "conservative")))
         
        
#head(df_new)
#table(df_new$political_class)
#================= 1. train and test =================
# train test split

# # check NA values / delete all
# colSums(is.na(df_new))
# df_new <- na.omit(df_new)
# 
# # 80% train 20% test
# smp_size <- floor(0.8 * nrow(df_new))
# train_ind <- sample(seq_len(nrow(df_new)), size = smp_size)
# train.new <- df_new[train_ind, ]
# test.new <- df_new[-train_ind, ]

#================= 2. logistic regression =================
# df_new$political_class <- as.factor(df_new$political_class)
formula2 <- 'political_class ~ prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + avg_sentiment_afinn_word + published_dow + Biden_sentiment + Trump_sentiment + stock_market_sentiment + financial_sentiment + death_sentiment + pandemic_sentiment + disease_sentiment + illness_sentiment + covid19 + scientist + republican+ democrat + repub_words + demo_words + div_words1 + div_words2 + div_words3 + div_words4 + div_words5'

# Fit the model
model4pol <- nnet::multinom(formula2, data = train.new)

# Make predictions
predicted.classes <- predict(model4pol, newdata=test.new) #, type="prob"

# confusion matrix
#table(predicted.classes , test$articles.source_name )
lr_pol_cm <- confusionMatrix(factor(predicted.classes), factor(test.new$political_class) )$table
#table( factor(predicted.classes), factor(test.new$political_class)  )

# accuracy
lr_pol_acc <- sum(diag(lr_pol_cm)) / sum(lr_pol_cm)

# AUC
predicted.classes <- as.ordered(predicted.classes)
predicted.classes.all <- predict(model4pol, newdata=test.new, type="prob")
# predicted.classes.cons <- as.numeric( predict(model4pol, newdata=test.new, type="prob")[,1] ) # convervative
# predicted.classes.liber <- as.numeric(predict(model4pol, newdata=test.new, type="prob")[,2] )  # liberal
# predicted.classes.mid <- as.numeric(predict(model4pol, newdata=test.new, type="prob")[,3] )   # middle
# #predicted.classes.wsj <- as.numeric(predict(model4pol, newdata=test.new, type="prob")[,4] )  # wsj
lr_pol_all_auc <- multiclass.roc(test.new$political_class, predicted.classes.all)$auc[1]
# cons <- multiclass.roc(test.new$political_class, predicted.classes.cons)$auc[1]
# lib <- multiclass.roc(test.new$political_class, predicted.classes.liber)$auc[1]
# mid <- multiclass.roc(test.new$political_class, predicted.classes.mid)$auc[1]

# print result
paste("AUC of overall is:", lr_pol_all_auc)
# paste("AUC of conservative is:", cons)
# paste("AUC of liberal is:", lib)
# paste("AUC of middle is:", mid)

# store results in dataframe
lr_rf_results <- data.frame(model = c("logreg_all", "rf_all", "logreg_all_political"),
                            test_accuracy = round(c(lr_all_acc, rf_all_acc, lr_pol_acc), 2),
                            auc = round(c(lr_all_auc, rf_all_auc, lr_pol_all_auc), 2))

#==================================================

gbm_results <- gbm_results %>% select(-train_accuracy)

# write results
write_csv(bind_rows(lr_rf_results, gbm_results), "../figures/model_results.csv")
