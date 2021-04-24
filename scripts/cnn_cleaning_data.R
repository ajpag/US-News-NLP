#Libraries
library(readr)
library(tidyverse)
library(tidytext)

#Read data from github
cnn_git <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_data_cnn.csv?token=ATPY7STZHRD24ZE4WR2MKU3AQQZDQ"
cnn <- readr::read_csv(cnn_git)

#Data missing analysis
cnn_clean <- cnn
cnn_clean$text[cnn_clean$text == "N/A"] <- NA #Replacing "N/A"
sum(is.na(cnn_clean$text))/length(cnn_clean$text) #26.6% missing <- not real news delete

#Dealing with missing data

cnn_clean <- cnn_clean[!is.na(cnn_clean$text), ]


#Delete "CNN" from the text in the articles

cnn_clean <- cnn_clean %>%
  mutate(text = str_remove_all(text, "CNN"))

cnn_clean <- cnn_clean %>%
  mutate(text = str_replace(text, fixed("()"), ""))

cnn_clean <- cnn_clean %>%
  mutate(text = str_replace(text, fixed("( Business)"), ""))

#Check there are no more "CNN" in the text 

sum(grepl("CNN", cnn_clean$text)) #No more CNN 


#Delete extra back slashes from the text in the articles

cnn_clean <- cnn_clean %>%
  mutate(text = str_replace_all(text, "[\n]", ""))

cnn_clean <- cnn_clean %>%
  mutate(text = gsub("\\\"", "", text, fixed = TRUE))

cnn_clean <- cnn_clean %>%
  mutate(text = gsub("\"", "", text, fixed = TRUE))


#Break in sentences 

cnn_sentences <- cnn_clean %>% 
  unnest_tokens(sentence, text, token = "sentences")
