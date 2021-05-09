# not run
setwd("C:/Users/apagta950/Documents/NYU/Courses/Spring 2021/MDML/Final Project/US-News-NLP/analysis")

# library(dplyr)
library(caret) # Gradient Boosting Machine  
library(gbm) # need for feature importance
library(nnet) # multinomial regression
library(ranger) # random forest
library(readr)
library(pROC) # multi-class ROC and AUC
library(tidyr)
library(MASS)
library(klaR)
library(ROCR)
library(ROSE)
library(e1071)
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
formula_all <- 'articles.source_name ~ prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 +avg_sentiment_afinn_word + + published_dow +Biden_sentiment + Trump_sentiment + stock_market_sentiment + financial_sentiment + death_sentiment + pandemic_sentiment + disease_sentiment + illness_sentiment + covid19 + scientist + republican+ democrat + repub_words + demo_words + div_words1 + div_words2 + div_words3 + div_words4 + div_words5'
formula_topics <- 'articles.source_name ~ prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7'
formula_keywords <- 'articles.source_name ~ Biden_sentiment + Trump_sentiment + stock_market_sentiment + financial_sentiment + death_sentiment + pandemic_sentiment + disease_sentiment + illness_sentiment + published_dow'
formula_paper_topics <- 'articles.source_name ~ covid19 + scientist + republican+ democrat + repub_words + demo_words + div_words1 + div_words2 + div_words3 + div_words4 + div_words5'

# Fit the model
model1 <- nnet::multinom(formula_all, data = articles_train)
model2 <- nnet::multinom(formula_topics, data = articles_train)
model3 <- nnet::multinom(formula_keywords, data = articles_train)
model4 <- nnet::multinom(formula_paper_topics, data = articles_train)

# Make predictions
predicted.classes1 <- predict(model1, newdata=articles_test) #, type="prob"
predicted.classes2 <- predict(model2, newdata=articles_test)
predicted.classes3 <- predict(model3, newdata=articles_test)
predicted.classes4 <- predict(model4, newdata=articles_test)

# confusion matrix
#table(predicted.classes , test$articles.source_name )
# confusionMatrix(factor(predicted.classes), factor(test$articles.source_name) )

# Model accuracy (overall)
lr_all_acc <- length( articles_test$articles.source_name[predicted.classes1==articles_test$articles.source_name] ) / length(articles_test$articles.source_name )
lr_topic_acc <- length( articles_test$articles.source_name[predicted.classes2==articles_test$articles.source_name] ) / length(articles_test$articles.source_name )
lr_keywords_acc <- length( articles_test$articles.source_name[predicted.classes3==articles_test$articles.source_name] ) / length(articles_test$articles.source_name )
lr_paper_topic_acc <- length( articles_test$articles.source_name[predicted.classes4==articles_test$articles.source_name] ) / length(articles_test$articles.source_name )

# AUC
predicted.classes <- as.ordered(predicted.classes)
predicted.classes.all <- predict(model1, newdata=articles_test, type="prob")

predicted.classes.1 <- predict(model1, newdata=articles_test, type="prob")
predicted.classes.2 <- predict(model2, newdata=articles_test, type="prob")
predicted.classes.3 <- predict(model3, newdata=articles_test, type="prob")
predicted.classes.4 <- predict(model4, newdata=articles_test, type="prob")

# predicted.classes.bbc <- as.numeric( predict(model, newdata=articles_test, type="prob")[,1] ) # bbc
# predicted.classes.cnn <- as.numeric(predict(model, newdata=articles_test, type="prob")[,2] )  # cnn
# predicted.classes.re <- as.numeric(predict(model, newdata=articles_test, type="prob")[,3] )   # re
# predicted.classes.wsj <- as.numeric(predict(model, newdata=articles_test, type="prob")[,4] )  # wsj
# print
lr_all_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.all)$auc[1]
# model_all <- multiclass.roc(articles_test$articles.source_name, predicted.classes.1)$auc[1]
lr_model_topics_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.2)$auc[1]
lr_model_keywords_auc<- multiclass.roc(articles_test$articles.source_name, predicted.classes.3)$auc[1]
lr_model_paper_keywords_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.4)$auc[1]
# bbc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.bbc)$auc[1]
# cnn <- multiclass.roc(articles_test$articles.source_name, predicted.classes.cnn)$auc[1]
# re <- multiclass.roc(articles_test$articles.source_name, predicted.classes.re)$auc[1]
# wsj <- multiclass.roc(articles_test$articles.source_name, predicted.classes.wsj)$auc[1]
# print result
paste("AUC of all is:", lr_all_auc)
# paste("AUC of using all feature is:", model_all)
paste("AUC of using topic featuresis:", lr_model_topics_auc)
paste("AUC of using keywords feature is:", lr_model_keywords_auc)
paste("AUC of using paper keywords feature is:", lr_model_paper_keywords)
# paste("AUC of BBC is:", bbc)
# paste("AUC of CNN is:", cnn)
# paste("AUC of Reuters is:", re)
# paste("AUC of WSJ is:", wsj)


######################################################
#          prediction : Random Forest
######################################################
formula1 <- 'articles.source_name ~ prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 +avg_sentiment_afinn_word + published_dow +Biden_sentiment + Trump_sentiment + stock_market_sentiment + financial_sentiment + death_sentiment + pandemic_sentiment + disease_sentiment + illness_sentiment + covid19 + scientist + republican+ democrat + repub_words + demo_words + div_words1 + div_words2 + div_words3 + div_words4 + div_words5'

rf_model1 <- ranger(formula = formula_all, num.trees=1000,
                    respect.unordered.factors=T, probability=T, data=articles_train)
rf_model2 <- ranger(formula = formula_topics, num.trees=1000,
                    respect.unordered.factors=T, probability=T, data=articles_train)
rf_model3 <- ranger(formula = formula_keywords, num.trees=1000,
                    respect.unordered.factors=T, probability=T, data=articles_train)
rf_model4 <- ranger(formula = formula_paper_topics, num.trees=1000,
                    respect.unordered.factors=T, probability=T, data=articles_train)
    
# Predict the testing set with the trained model
predictions2.1 <- predict(rf_model1, articles_test, type ="response")
predictions2.2 <- predict(rf_model2, articles_test, type ="response")
predictions2.3 <- predict(rf_model3, articles_test, type ="response")
predictions2.4 <- predict(rf_model4, articles_test, type ="response")
probabilities1 <- as.data.frame(predict(rf_model1, data=articles_test)$predictions)
probabilities2 <- as.data.frame(predict(rf_model2, data=articles_test)$predictions)
probabilities3 <- as.data.frame(predict(rf_model3, data=articles_test)$predictions)
probabilities4 <- as.data.frame(predict(rf_model4, data=articles_test)$predictions)

#pre-process
#pre-process
predict_class1 <- data.frame(max.col(probabilities1) )
predict_class2 <- data.frame(max.col(probabilities2) )
predict_class3 <- data.frame(max.col(probabilities3) )
predict_class4 <- data.frame(max.col(probabilities4) )

colnames(predict_class1) <- "class"
colnames(predict_class2) <- "class"
colnames(predict_class3) <- "class"
colnames(predict_class4) <- "class"

predict_class1 <- predict_class1 %>%
  mutate( class = case_when(class==1 ~ "BBC News",
                            class==2 ~ "CNN",
                            class==3 ~ "Reuters",
                            class==4 ~ "The Wall Street Journal") )
predict_class2 <- predict_class2 %>%
  mutate( class = case_when(class==1 ~ "BBC News",
                            class==2 ~ "CNN",
                            class==3 ~ "Reuters",
                            class==4 ~ "The Wall Street Journal")  )
predict_class3 <- predict_class3 %>%
  mutate( class = case_when(class==1 ~ "BBC News",
                            class==2 ~ "CNN",
                            class==3 ~ "Reuters",
                            class==4 ~ "The Wall Street Journal") )
predict_class4 <- predict_class4 %>%
  mutate( class = case_when(class==1 ~ "BBC News",
                            class==2 ~ "CNN",
                            class==3 ~ "Reuters",
                            class==4 ~ "The Wall Street Journal"))

predict_class1$class <- as.factor(predict_class1$class)
predict_class2$class <- as.factor(predict_class2$class)
predict_class3$class <- as.factor(predict_class3$class)
predict_class4$class <- as.factor(predict_class4$class)

# confusion matrix
rf_all_cm <- confusionMatrix(table(articles_test$articles.source_name, predict_class1$class))$table
rf_topic_cm <- confusionMatrix(table(articles_test$articles.source_name, predict_class2$class))$table
rf_keywords_cm <- confusionMatrix(table(articles_test$articles.source_name, predict_class3$class))$table
rf_paper_cm <- confusionMatrix(table(articles_test$articles.source_name, predict_class4$class))$table


rf_all_acc <- sum(diag(rf_cm)) / sum(rf_cm)
rf_topic_acc <- sum(diag(rf_topic_cm)) / sum(rf_topic_cm)
rf_keywords_acc <- sum(diag(rf_keywords_cm)) / sum(rf_keywords_cm)
rf_paper_acc <- sum(diag(rf_paper_cm)) / sum(rf_paper_cm)


# prediction and performance report
modeling_data_rf <- articles_test %>% 
  mutate(probability = probabilities1[,2],
         #predict = max.col(probabilities)-1
         predict = predict_class1$class         )
modeling_data_rf <- as.data.frame(modeling_data_rf)

# AUC
# predicted.classes.all <- probabilities
predicted.classes.all <- probabilities1
predicted.classes.2 <- probabilities2
predicted.classes.3 <- probabilities3
predicted.classes.4 <- probabilities4

rf_all_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.all)$auc[1]
# rf_all_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.all)$auc[1]
rf_topic_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.2)$auc[1]
rf_keyword_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.3)$auc[1]
rf_paper_key_auc <- multiclass.roc(articles_test$articles.source_name, predicted.classes.4)$auc[1]


# print result
paste("AUC of all (RF) is:", rf_all_auc)
paste("AUC of all (RF) is:", rf_all_acc)
paste("AUC of topic (RF) is:", rf_topic_auc)
paste("AUC of keyword (RF) is:", rf_keyword_auc)
paste("AUC of paper keywords (RF) is:", rf_paper_key_auc)




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
lr_rf_results <- data.frame(model = c("logreg_all", "logreg_topic", "logreg_keyword",
                                      "logreg_paper", "rf_all", "rf_topic", "rf_keyword",
                                      "rf_paper", "logreg_all_political"),
                            test_accuracy = round(c(lr_all_acc,lr_topic_acc, lr_keywords_acc,
                                                    lr_paper_topic_acc,
                                                    rf_all_acc, rf_topic_acc, rf_keywords_acc,
                                                    rf_paper_acc,
                                                    lr_pol_acc), 2),
                            auc = round(c(lr_all_auc, lr_model_topics_auc, lr_model_keywords_auc,
                                          lr_model_paper_keywords_auc,
                                          rf_all_auc, rf_topic_auc, rf_keywords_acc,
                                          rf_paper_key_auc,
                                          lr_pol_all_auc), 2))

#==================================================

gbm_results <- gbm_results %>% select(-train_accuracy)

######################## Naive Bayes ###########################################

#Major assumption of Naive Bayes is that there are independence in the predictors


#Model 0:

#average sentiment by sentence
#prob topic 1:7
#Biden_sentiment
#Trump_sentiment
#stock_market_sentiment
#financial_sentiment
#death_sentiment
#pandemic_sentiment
#disease_sentiment
#illness_sentiment
#publish dow
#covid19
#scientist
#republican
#democrat
#repub_words
#demo_words
#div_words1
#div_words2
#div_words3
#div_words4
#div_words5

#Data for model 


mod0_data <- predictors %>% 
  dplyr::select(articles.source_name, avg_sentiment_afinn_sent,
                prob_topic_1, prob_topic_2, prob_topic_3, prob_topic_4,
                prob_topic_5, prob_topic_6, prob_topic_7, Biden_sentiment, Trump_sentiment, 
                stock_market_sentiment, financial_sentiment, death_sentiment,
                pandemic_sentiment, disease_sentiment, illness_sentiment, covid19, scientist, republican, 
                democrat, repub_words, demo_words, div_words1, div_words2, div_words3,
                div_words4, div_words5, published_dow) %>% #select variables for the model
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow)) #Code factor variables

mod0_data <- mod0_data[complete.cases(mod0_data), ]

#Split data to train and test subsets

split_size_mod0 <-  floor(nrow(mod0_data)*.80) #80% train dataset

mod0_train <- mod0_data %>%
  slice(1:split_size_mod1) 

mod0_test <- mod0_data %>%
  slice(split_size_mod0 + 1:n())

#Fit model 0 in train subset

mod0 <- klaR::NaiveBayes(articles.source_name ~ ., 
                         data = mod0_train, usekernel = FALSE, fL = 0)

#Predict test subset

pred_mod0 <- predict(mod0, mod0_test)

#Confusion matrix (accuracy, precision, recall)

mat_mod0 <- confusionMatrix(pred_mod0$class, mod0_test$articles.source_name)
accuracy_mod0 <- mat_mod0$overall[1]
#Precision by class compare: "Pos Pred Value"
#Recall by class compare: "Sensitivity"

#AUC

prob0 <- as.data.frame(pred_mod0$posterior)
AUC_0 <- multiclass.roc(mod0_test$articles.source_name, prob0)

#Model 1:

#average sentiment by sentence
#prob topic 1:7
#publish dow

#Data for model 


mod1_data <- predictors %>% 
  dplyr::select(articles.source_name, avg_sentiment_afinn_sent,
                prob_topic_1, prob_topic_2, prob_topic_3, prob_topic_4,
                prob_topic_5, prob_topic_6, prob_topic_7, published_dow) %>% #select variables for the model
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow)) #Code factor variables

mod1_data <- mod1_data[complete.cases(mod1_data), ]

#Split data to train and test subsets

split_size_mod1 <-  floor(nrow(mod1_data)*.80) #80% train dataset

mod1_train <- mod1_data %>%
  slice(1:split_size_mod1) 

mod1_test <- mod1_data %>%
  slice(split_size_mod1 + 1:n())

#Fit model 1 in train subset

mod1 <- klaR::NaiveBayes(articles.source_name ~ ., 
                         data = mod1_train, usekernel = FALSE, fL = 0)

#Predict test subset

pred_mod1 <- predict(mod1, mod1_test)

#Confusion matrix (accuracy, precision, recall)

mat_mod1 <- confusionMatrix(pred_mod1$class, mod1_test$articles.source_name)
accuracy_mod1 <- mat_mod1$overall[1]
#Precision by class compare: "Pos Pred Value"
#Recall by class compare: "Sensitivity"

#AUC

prob1 <- as.data.frame(pred_mod1$posterior)
AUC_1 <- multiclass.roc(mod1_test$articles.source_name, prob1)


#Model 2:

#Biden_sentiment
#Trump_sentiment
#stock_market_sentiment
#financial_sentiment
#death_sentiment
#pandemic_sentiment
#disease_sentiment
#illness_sentiment
#publish dow

#Data for model

mod2_data <- predictors %>% 
  dplyr::select(articles.source_name, Biden_sentiment, Trump_sentiment, 
                stock_market_sentiment, financial_sentiment, death_sentiment,
                pandemic_sentiment, disease_sentiment, illness_sentiment, published_dow) %>%
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow))

mod2_data <- mod2_data[complete.cases(mod2_data), ]

#Split data to train and test subsets

split_size_mod2 <-  floor(nrow(mod2_data)*.80)

mod2_train <- mod2_data %>%
  slice(1:split_size_mod2) 

mod2_test <- mod2_data %>%
  slice(split_size_mod2 + 1:n())

#Fit model 2 in train subset

mod2 <- klaR::NaiveBayes(articles.source_name ~ ., 
                         data = mod2_train, usekernel = FALSE, fL = 0)

#Predict test subset

pred_mod2 <- predict(mod2, mod2_test)


#Confusion matrix (accuracy, precision, recall)

mat_mod2 <- confusionMatrix(pred_mod2$class, mod2_test$articles.source_name)
accuracy_mod2 <- mat_mod2$overall[1]
#Precision by class compare: "Pos Pred Value"
#Recall by class compare: "Sensitivity"


#AUC

prob2 <- as.data.frame(pred_mod2$posterior)
AUC_2 <- multiclass.roc(mod2_test$articles.source_name, prob2)


#Model 3:
#covid19
#scientist
#republican
#democrat
#repub_words
#demo_words
#div_words1
#div_words2
#div_words3
#div_words4
#div_words5
#published_dow

#Data for model

mod3_data <- predictors %>% 
  dplyr::select(articles.source_name, covid19, scientist, republican, 
                democrat, repub_words, demo_words, div_words1, div_words2, div_words3,
                div_words4, div_words5, published_dow) %>%
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow))

mod3_data <- mod3_data[complete.cases(mod3_data), ]

#Split data to train and test subsets

split_size_mod3 <-  floor(nrow(mod3_data)*0.80)

mod3_train <- mod3_data %>%
  slice(1:split_size_mod3) 

mod3_test <- mod3_data %>%
  slice(split_size_mod3 + 1:n())

#Fit model 3 in train subset

mod3 <- klaR::NaiveBayes(articles.source_name ~ ., 
                         data = mod3_train, usekernel = FALSE, fL = 0)

#Predict test subset

pred_mod3 <- predict(mod3, mod3_test) #Warning probabilities predicted as zero

#Confusion matrix (accuracy, precision, recall)

mat_mod3 <- confusionMatrix(pred_mod3$class, mod3_test$articles.source_name)
accuracy_mod3 <- mat_mod3$overall[1]
#Precision by class compare: "Pos Pred Value"
#Recall by class compare: "Sensitivity"


#AUC

prob3 <- as.data.frame(pred_mod3$posterior)
AUC_3 <- multiclass.roc(mod3_test$articles.source_name, prob3)


#Model 4: Optimization model 1

#Data for model

mod4_data <- mod1_data %>% 
  dplyr::mutate(source_name1 = as.character(articles.source_name)) #Code new variable

mod4_data$source_name1[mod4_data$source_name1 != "BBC News"] <- "Others_not_BBC"

mod4_data$source_name1 <- as.factor(mod4_data$source_name1)

#Split data to train and test subsets

split_size_mod4 <-  floor(nrow(mod4_data)*0.80)

mod4_train <- mod4_data %>%
  slice(1:split_size_mod4) 

mod4_test <- mod4_data %>%
  slice(split_size_mod4 + 1:n())

#Balancing train dataset
#Using ROSE package to oversampling
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/

mod4_balanced_train <- ovun.sample(source_name1 ~., data = mod4_train, method = "over",
                                   N = sum(mod4_train$source_name1 == "Others_not_BBC")*2)$data 

mod4_balanced_train$source_name1 <- relevel(mod4_balanced_train$source_name1, "BBC News") 

#Fit model BBC against all others

mod4a <- klaR::NaiveBayes(source_name1 ~ avg_sentiment_afinn_sent + prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + published_dow, 
                          data = mod4_balanced_train, 
                          usekernel = FALSE, fL = 0)

#Predict classes for train and test datasets 

pred_mod4a_train <- predict(mod4a, mod4_balanced_train)
pred_mod4a_test <- predict(mod4a, mod4_test)


#Include predicted classes in the train dataset

mod4_balanced_train$pred4a <- pred_mod4a_train$class
mod4_test$pred4a <- pred_mod4a_test$class


#AUC A
test.pred4a <- prediction(pred_mod4a_test$posterior[ ,2], mod4_test$source_name1)
test.perf4a <- performance(test.pred4a, "auc")
cat('the test auc score is ', test.perf4a@y.values[[1]], "\n")

#Confusion matrix

confusionMatrix(pred_mod4a_test$class, mod4_test$source_name1)


#New train dataset to fit others except BBC News 

mod4_trainb <- mod4_balanced_train %>%
  filter(pred4a == "Others_not_BBC") %>%
  mutate(source_name2 = as.character(articles.source_name))

mod4_trainb$source_name2[mod4_trainb$source_name2 != "CNN"] <- "Others_not_CNN"
mod4_trainb$source_name2 <- as.factor(mod4_trainb$source_name2)

length(mod4_trainb$source_name2)
sum(mod4_trainb$source_name2 == "Others_not_CNN")

mod4_balanced_trainb <- ovun.sample(source_name2 ~., data = mod4_trainb, 
                                    method = "over",
                                    N = sum(mod4_trainb$source_name2 == "Others_not_CNN")*2)$data 
table(mod4_balanced_trainb$source_name2)

mod4_balanced_trainb$source_name2 <- relevel(mod4_balanced_trainb$source_name2, "CNN")

#Fit model CNN against all others

mod4b <- klaR::NaiveBayes(source_name2 ~ avg_sentiment_afinn_sent + prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + published_dow, 
                          data = mod4_balanced_trainb, usekernel = FALSE, fL = 0)

#New test dataset to predict

mod4_testb <- mod4_test %>%
  filter(pred4a == "Others_not_BBC") %>%
  mutate(source_name2 = as.character(articles.source_name))

mod4_testb$source_name2[mod4_testb$source_name2 != "CNN"] <- "Others_not_CNN"

mod4_testb$source_name2 <- as.factor(mod4_testb$source_name2)


#Predict classes

pred_mod4b_train <- predict(mod4b,mod4_balanced_trainb)
pred_mod4b_test <- predict(mod4b, mod4_testb)

#Include predicted classes in the train and test b dataset

mod4_balanced_trainb$pred4b <- pred_mod4b_train$class
mod4_testb$pred4b <- pred_mod4b_test$class

#AUC B
test.pred4b <- prediction(pred_mod4b_test$posterior[ ,2], mod4_testb$source_name2)
test.perf4b <- performance(test.pred4b, "auc")
cat('the test auc score is ', test.perf4b@y.values[[1]], "\n")

#Confusion matrix

confusionMatrix(pred_mod4b_test$class, mod4_testb$source_name2)

#New train dataset to fit others except CNN and BBC news

mod4_trainc <- mod4_balanced_trainb %>%
  filter(pred4b == "Others_not_CNN") %>%
  mutate(source_name3 = as.character(articles.source_name))

mod4_trainc$source_name3[mod4_trainc$source_name3 != "Reuters"] <- "Others_not_Reuters"
mod4_trainc$source_name3 <- as.factor(mod4_trainc$source_name3)

length(mod4_trainc$source_name3)
sum(mod4_trainc$source_name3 == "Others_not_Reuters")

mod4_balanced_trainc <- ovun.sample(source_name3 ~., data = mod4_trainc, method = "over",
                                    N = sum(mod4_trainc$source_name3 == "Others_not_Reuters")*2)$data 
table(mod4_balanced_trainc$source_name3)

#Fit model CNN against all others

mod4c <- klaR::NaiveBayes(source_name3 ~ avg_sentiment_afinn_sent + prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + published_dow, 
                          data = mod4_balanced_trainc, usekernel = FALSE, fL = 0)

#New test dataset to predict

mod4_testc <- mod4_testb %>%
  filter(pred4b == "Others_not_CNN") %>%
  mutate(source_name3 = as.character(articles.source_name))

mod4_testc$source_name3[mod4_testc$source_name3 != "Reuters"] <- "Others_not_Reuters"

mod4_testc$source_name3 <- as.factor(mod4_testc$source_name3)


#Predict classes

pred_mod4c_train <- predict(mod4c, mod4_balanced_trainc)
pred_mod4c_test <- predict(mod4c, mod4_testc)

#Include predicted classes in the train and test c dataset

mod4_balanced_trainc$pred4c <- pred_mod4c_train$class
mod4_testc$pred4c <- pred_mod4c_test$class


#AUC C
test.pred4c <- prediction(pred_mod4c_test$posterior[ ,2], mod4_testc$source_name3)
test.perf4c <- performance(test.pred4c, "auc")
cat('the test auc score is ', test.perf4c@y.values[[1]], "\n")

#Confusion matrix

mod4c_mat <- confusionMatrix(pred_mod4c_test$class, mod4_testc$source_name3)
precision <- mod4c_mat$byClass["Pos Pred Value"]
recall <- mod4c_mat$byClass["Sensitivity"]




#https://stats.stackexchange.com/questions/181318/r-caret-naive-bayes-untuned-results-differ-from-klar/181398
#different packages should give same results 

nb_results = data.frame(model = c("nb_all", "nb_topic", "nb_keywords", "nb_paper"),
                        test_accuracy = round(c(accuracy_mod0, accuracy_mod1, 
                                                accuracy_mod2, accuracy_mod3), 2),
                        auc = round(c(AUC_0$auc, AUC_1$auc, AUC_2$auc, AUC_3$auc), 2))

######################## Support Vector Machine ################################

#Model 0:

#average sentiment by sentence
#prob topic 1:7
#Biden_sentiment
#Trump_sentiment
#stock_market_sentiment
#financial_sentiment
#death_sentiment
#pandemic_sentiment
#disease_sentiment
#illness_sentiment
#publish dow
#covid19
#scientist
#republican
#democrat
#repub_words
#demo_words
#div_words1
#div_words2
#div_words3
#div_words4
#div_words5

#Data for model 


mod0_data <- predictors %>% 
  dplyr::select(articles.source_name, avg_sentiment_afinn_sent,
                prob_topic_1, prob_topic_2, prob_topic_3, prob_topic_4,
                prob_topic_5, prob_topic_6, prob_topic_7, Biden_sentiment, Trump_sentiment, 
                stock_market_sentiment, financial_sentiment, death_sentiment,
                pandemic_sentiment, disease_sentiment, illness_sentiment, covid19, scientist, republican, 
                democrat, repub_words, demo_words, div_words1, div_words2, div_words3,
                div_words4, div_words5, published_dow) %>% #select variables for the model
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow)) #Code factor variables

mod0_data <- mod0_data[complete.cases(mod0_data), ]

#Split data to train and test subsets

split_size_mod0 <-  floor(nrow(mod0_data)*.80)

mod0_train <- mod0_data %>%
  slice(1:split_size_mod0) 

mod0_test <- mod0_data %>%
  slice(split_size_mod0 + 1:n())

#Fit model 0 in train subset

mod0 <- e1071::svm(articles.source_name ~ ., data = mod0_train, probability = TRUE)

#plot(mod0, mod0_train, prob_topic_2 ~ prob_topic_4)

#Predict test subset

pred_mod0 <- predict(mod0, mod0_test, probability = TRUE)


#Confusion matrix

acc_mod0_svm <- sum(diag(confusionMatrix(pred_mod0, mod0_test$articles.source_name)$table)) / 
      sum(confusionMatrix(pred_mod0, mod0_test$articles.source_name)$table)

#AUC

prob0_svm <- attr(pred_mod0, "probabilities")

AUC_0_svm <- multiclass.roc(mod0_test$articles.source_name, prob0_svm)

#Model 1:

#average sentiment by sentence
#prob topic 1:7
#publish dow

#Data for model 

mod1_data <- predictors %>% 
  dplyr::select(articles.source_name, avg_sentiment_afinn_sent,
                prob_topic_1, prob_topic_2, prob_topic_3, prob_topic_4, prob_topic_5, 
                prob_topic_6, prob_topic_7, published_dow) %>% #select variables for the model
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow)) #Code factor variables

mod1_data <- mod1_data[complete.cases(mod1_data), ]

#Split data to train and test subsets

split_size_mod1 <-  floor(nrow(mod1_data)*.80)

mod1_train <- mod1_data %>%
  slice(1:split_size_mod1) 

mod1_test <- mod1_data %>%
  slice(split_size_mod1 + 1:n())

#Fit model 1 in train subset

mod1 <- e1071::svm(articles.source_name ~ ., data = mod1_train, probability = TRUE)

#plot(mod1, mod1_train, prob_topic_2 ~ prob_topic_4)

#Predict test subset

pred_mod1 <- predict(mod1, mod1_test, probability = TRUE)


#Confusion matrix

confusionMatrix(pred_mod1, mod1_test$articles.source_name)

acc_mod1_svm <- sum(diag(confusionMatrix(pred_mod1, mod0_test$articles.source_name)$table)) / 
  sum(confusionMatrix(pred_mod1, mod1_test$articles.source_name)$table)

#AUC

prob1_svm <- attr(pred_mod1, "probabilities")

AUC_1_svm <- multiclass.roc(mod1_test$articles.source_name, prob1_svm)


#Model 2:

#Biden_sentiment
#Trump_sentiment
#stock_market_sentiment
#financial_sentiment
#death_sentiment
#pandemic_sentiment
#disease_sentiment
#illness_sentiment
#publish dow

#Data for model

mod2_data <- predictors %>% 
  dplyr::select(articles.source_name, Biden_sentiment, Trump_sentiment, 
                stock_market_sentiment, financial_sentiment, death_sentiment,
                pandemic_sentiment, disease_sentiment, illness_sentiment, published_dow) %>%
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow))

mod2_data <- mod2_data[complete.cases(mod2_data), ]

#Split data to train and test subsets

split_size_mod2 <-  floor(nrow(mod2_data)*.80)

mod2_train <- mod2_data %>%
  slice(1:split_size_mod2) 

mod2_test <- mod2_data %>%
  slice(split_size_mod2 + 1:n())

#Fit model 1 in train subset

mod2 <- e1071::svm(articles.source_name ~ ., data = mod2_train, probability = TRUE)

#plot(mod2, mod2_train, Trump_sentiment ~ financial_sentiment)

#Predict test subset

pred_mod2 <- predict(mod2, mod2_test, probability = TRUE)


#Confusion matrix

confusionMatrix(pred_mod2, mod2_test$articles.source_name)

acc_mod2_svm <- sum(diag(confusionMatrix(pred_mod2, mod2_test$articles.source_name)$table)) / 
  sum(confusionMatrix(pred_mod2, mod2_test$articles.source_name)$table)

#AUC

prob2_svm <- attr(pred_mod2, "probabilities")

AUC_2_svm <- multiclass.roc(mod2_test$articles.source_name, prob2_svm)


#Model 3:

#covid19
#scientist
#republican
#democrat
#repub_words
#demo_words
#div_words1
#div_words2
#div_words3
#div_words4
#div_words5
#published_dow


#Data for model

mod3_data <- predictors %>% 
  dplyr::select(articles.source_name, covid19, scientist, republican, 
                democrat, repub_words, demo_words, div_words1, div_words2, div_words3,
                div_words4, div_words5, published_dow) %>%
  dplyr::mutate(articles.source_name = as.factor(articles.source_name),
                published_dow = as.factor(published_dow))

mod3_data <- mod3_data[complete.cases(mod3_data), ]

#Split data to train and test subsets

split_size_mod3 <-  floor(nrow(mod3_data)*.80)

mod3_train <- mod3_data %>%
  slice(1:split_size_mod3) 

mod3_test <- mod3_data %>%
  slice(split_size_mod3 + 1:n())

#Fit model 1 in train subset

mod3 <- e1071::svm(articles.source_name ~ ., data = mod3_train, probability = TRUE)

#plot(mod3, mod3_train, demo_words ~ repub_words)

#Predict test subset

pred_mod3 <- predict(mod3, mod3_test, probability = TRUE)


#Confusion matrix

confusionMatrix(pred_mod3, mod3_test$articles.source_name)

acc_mod3_svm <- sum(diag(confusionMatrix(pred_mod3, mod3_test$articles.source_name)$table)) / 
  sum(confusionMatrix(pred_mod3, mod3_test$articles.source_name)$table)

#AUC

prob3_svm <- attr(pred_mod3, "probabilities")

AUC_3_svm <- multiclass.roc(mod3_test$articles.source_name, prob3_svm)

#Model 4: Optimization model 2

#Data for model

mod4_data <- mod1_data %>% 
  dplyr::mutate(source_name1 = as.character(articles.source_name)) #Code new variable

mod4_data$source_name1[mod4_data$source_name1 != "BBC News"] <- "Others_not_BBC"

mod4_data$source_name1 <- as.factor(mod4_data$source_name1)

#Split data to train and test subsets

split_size_mod4 <-  floor(nrow(mod4_data)*0.80)

mod4_train <- mod4_data %>%
  slice(1:split_size_mod4) 

mod4_test <- mod4_data %>%
  slice(split_size_mod4 + 1:n())

#Balancing train dataset
#Using ROSE package to oversampling
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/

mod4_balanced_train <- ovun.sample(source_name1 ~., data = mod4_train, method = "over",
                                   N = sum(mod4_train$source_name1 == "Others_not_BBC")*2)$data 

mod4_balanced_train$source_name1 <- relevel(mod4_balanced_train$source_name1, "BBC News") 

#Fit model BBC against all others

mod4a <- e1071::svm(source_name1 ~ avg_sentiment_afinn_sent + prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + published_dow,
                    data = mod4_balanced_train, probability = TRUE)


#Predict classes for train and test datasets 

pred_mod4a_train <- predict(mod4a, mod4_balanced_train, probability = TRUE)
pred_mod4a_test <- predict(mod4a, mod4_test, probability = TRUE)


#Include predicted classes in the train dataset

mod4_balanced_train$pred4a <- pred_mod4a_train
mod4_test$pred4a <- pred_mod4a_test


#AUC A
test.pred4a <- prediction(attr(pred_mod4a_test, "probabilities")[ ,1], mod4_test$source_name1)
test.perf4a <- performance(test.pred4a, "auc")
cat('the test auc score is ', test.perf4a@y.values[[1]], "\n")

#Confusion matrix

confusionMatrix(pred_mod4a_test, mod4_test$source_name1)


#New train dataset to fit others except BBC News 

mod4_trainb <- mod4_balanced_train %>%
  filter(pred4a == "Others_not_BBC") %>%
  mutate(source_name2 = as.character(articles.source_name))

mod4_trainb$source_name2[mod4_trainb$source_name2 != "CNN"] <- "Others_not_CNN"
mod4_trainb$source_name2 <- as.factor(mod4_trainb$source_name2)

length(mod4_trainb$source_name2)
sum(mod4_trainb$source_name2 == "Others_not_CNN")

mod4_balanced_trainb <- ovun.sample(source_name2 ~., data = mod4_trainb, 
                                    method = "over",
                                    N = sum(mod4_trainb$source_name2 == "Others_not_CNN")*2)$data 
table(mod4_balanced_trainb$source_name2)

mod4_balanced_trainb$source_name2 <- relevel(mod4_balanced_trainb$source_name2, "CNN")

#Fit model CNN against all others

mod4b <- e1071::svm(source_name2 ~ avg_sentiment_afinn_sent + prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + published_dow, 
                    data = mod4_balanced_trainb, probability = TRUE)


#New test dataset to predict

mod4_testb <- mod4_test %>%
  filter(pred4a == "Others_not_BBC") %>%
  mutate(source_name2 = as.character(articles.source_name))

mod4_testb$source_name2[mod4_testb$source_name2 != "CNN"] <- "Others_not_CNN"

mod4_testb$source_name2 <- as.factor(mod4_testb$source_name2)


#Predict classes

pred_mod4b_train <- predict(mod4b,mod4_balanced_trainb, probability = TRUE)
pred_mod4b_test <- predict(mod4b, mod4_testb, probability = TRUE)

#Include predicted classes in the train and test b dataset

mod4_balanced_trainb$pred4b <- pred_mod4b_train
mod4_testb$pred4b <- pred_mod4b_test

#AUC B
test.pred4b <- prediction(attr(pred_mod4b_test, "probabilities")[ ,1], mod4_testb$source_name2)
test.perf4b <- performance(test.pred4b, "auc")
cat('the test auc score is ', test.perf4b@y.values[[1]], "\n")

#Confusion matrix

confusionMatrix(pred_mod4b_test, mod4_testb$source_name2)

#New train dataset to fit others except CNN and BBC news

mod4_trainc <- mod4_balanced_trainb %>%
  filter(pred4b == "Others_not_CNN") %>%
  mutate(source_name3 = as.character(articles.source_name))

mod4_trainc$source_name3[mod4_trainc$source_name3 != "Reuters"] <- "Others_not_Reuters"
mod4_trainc$source_name3 <- as.factor(mod4_trainc$source_name3)

length(mod4_trainc$source_name3)
sum(mod4_trainc$source_name3 == "Others_not_Reuters")

mod4_balanced_trainc <- ovun.sample(source_name3 ~., data = mod4_trainc, method = "over",
                                    N = sum(mod4_trainc$source_name3 == "Others_not_Reuters")*2)$data 
table(mod4_balanced_trainc$source_name3)

#Fit model CNN against all others

mod4c <- e1071::svm(source_name3 ~ avg_sentiment_afinn_sent + prob_topic_1 + prob_topic_2 + prob_topic_3 + prob_topic_4 + prob_topic_5 + prob_topic_6 + prob_topic_7 + published_dow, 
                    data = mod4_balanced_trainc, probability = TRUE)

#New test dataset to predict

mod4_testc <- mod4_testb %>%
  filter(pred4b == "Others_not_CNN") %>%
  mutate(source_name3 = as.character(articles.source_name))

mod4_testc$source_name3[mod4_testc$source_name3 != "Reuters"] <- "Others_not_Reuters"

mod4_testc$source_name3 <- as.factor(mod4_testc$source_name3)


#Predict classes

pred_mod4c_train <- predict(mod4c, mod4_balanced_trainc, probability = TRUE)
pred_mod4c_test <- predict(mod4c, mod4_testc, probability = TRUE)

#Include predicted classes in the train and test c dataset

mod4_balanced_trainc$pred4c <- pred_mod4c_train
mod4_testc$pred4c <- pred_mod4c_test


#AUC C
test.pred4c <- prediction(attr(pred_mod4c_test, "probabilities")[ ,2], mod4_testc$source_name3)
test.perf4c <- performance(test.pred4c, "auc")
cat('the test auc score is ', test.perf4c@y.values[[1]], "\n")

#Confusion matrix

mod4c_mat <- confusionMatrix(pred_mod4c_test, mod4_testc$source_name3)
precision <- mod4c_mat$byClass["Pos Pred Value"]
recall <- mod4c_mat$byClass["Sensitivity"]

# create dataframe

svm_results = data.frame(model = c("svm_all", "svm_topic", "svm_keywords", "svm_paper"),
                        test_accuracy = round(c(acc_mod0_svm, acc_mod1_svm, 
                                                acc_mod2_svm, acc_mod3_svm), 2),
                        auc = round(c(AUC_0_svm$auc, AUC_1_svm$auc, AUC_2_svm$auc, AUC_3_svm$auc), 2))

########## Write results ############

results <- bind_rows(lr_rf_results, svm_results, gbm_results, nb_results) %>% 
  arrange(desc(test_accuracy))

# write results
write_csv(results, "../figures/model_results.csv")

# plot results
p_results <- results %>% 
  pivot_longer(cols = c(test_accuracy, auc), names_to = "metric") %>% 
  ggplot(aes(x = reorder(model, value), y = value)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~metric) +
  coord_flip() + 
  labs(title = "Model Results")

ggsave(plot = p_results, file = paste0(figures_dir, "model_results.png"),
       width = 8, height = 4)
