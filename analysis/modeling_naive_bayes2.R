#Libraries
library(readr)
library(tidyverse)
library(MASS)
library(klaR)
library(caret)
library(pROC)
library(ROSE)
library(ROCR)

set.seed(0)

#Read raw data from github
predictors <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_model_input.csv?token=ATPY7SVJZK5RWFQG6M6252LAS4BKU"
predictors <- readr::read_csv(predictors)
predictors <-  predictors %>% 
  slice(sample(1:n())) #Shuffle data 

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
  slice(1:split_size_mod0) 

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
