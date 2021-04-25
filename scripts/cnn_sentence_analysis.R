#Libraries
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)

#Read data from github
cnn_git <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_data_cnn.csv?token=ATPY7SRZX7R4O7WOEMZYGK3AQTBDK"
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
  unnest_tokens(sentence, text, token = "sentences") %>% #unnest sentences
  mutate(sentence.no = 1:n()) #consecutive number for sentences

#Break in words

cnn_sentences <- cnn_sentences %>%
  unnest_tokens(word, sentence) #unnest words (words are the token by default)


#Adding sentiment column 

cnn_with_setiments <- cnn_sentences %>%
  inner_join(get_sentiments("afinn"))

#Average sentiment by sentence 

cnn_with_setiments_av <- cnn_with_setiments %>%
  mutate(date = lubridate::date(articles.published_datetime)) %>%
  group_by(sentence.no) %>%
  summarise(date = min(date),
            sentiment = mean(value))

#Average sentiment by date

cnn_av_by_date <- cnn_with_setiments_av %>%
  group_by(date) %>%
  summarise(sentiment = mean(sentiment))

#Plot sentiment by day

ggplot(cnn_av_by_date, aes(date, sentiment)) +
  geom_col() 

# Export data

write_csv(cnn_av_by_date, path = 'cnn_sentence_sentiment.csv') 



