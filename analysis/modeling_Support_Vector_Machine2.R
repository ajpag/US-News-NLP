#Libraries
library(readr)
library(tidyverse)
library(MASS)
library(e1071)
library(caret)
library(pROC)
library(ROCR)
library(ROSE)

set.seed(0)

#Read raw data from github
predictors <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_model_input.csv?token=ATPY7SX4262MWXJPOSABARDAS4TKS"
predictors <- readr::read_csv(predictors)
predictors <-  predictors %>% 
  slice(sample(1:n())) #Shuffle data 

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

confusionMatrix(pred_mod0, mod0_test$articles.source_name)

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

#AUC

prob3_svm <- attr(pred_mod3, "probabilities")

AUC_3_svm <- multiclass.roc(mod3_test$articles.source_name, prob3_svm)



