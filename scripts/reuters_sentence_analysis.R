#Libraries
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)

#Read data from github
reuters_git <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_data_reuters.csv?token=ATPY7SV43DCGWJU3HMDSYBLAQS5YG"
reuters <- readr::read_csv(reuters_git)

#Break in sentences 

reuters_sentences <- reuters %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% #unnest sentences
  mutate(sentence.no = 1:n()) #consecutive number for sentences

#Break in words

reuters_sentences <- reuters_sentences %>%
  unnest_tokens(word, sentence) #unnest words (words are the token by default)


#Adding sentiment column 

reuters_with_setiments <- reuters_sentences %>%
  inner_join(get_sentiments("afinn"))

#Average sentiment by sentence 

reuters_with_setiments_av <- reuters_with_setiments %>%
  mutate(date = lubridate::date(articles.published_datetime)) %>%
  group_by(sentence.no) %>%
  summarise(date = min(date),
            sentiment = mean(value))

#Average sentiment by date

reuters_av_by_date <- reuters_with_setiments_av %>%
  group_by(date) %>%
  summarise(sentiment = mean(sentiment)) %>%
  mutate(source = "Reuters")

#Plot sentiment by day

ggplot(reuters_av_by_date, aes(date, sentiment)) +
  geom_col() 


# Export data

write_csv(reuters_av_by_date, path = 'reuters_sentence_sentiment.csv') 



