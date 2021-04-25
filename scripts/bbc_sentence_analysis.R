#Libraries
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)

#Read data from github
bbc_git <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_bbc.csv?token=ATPY7SSWVFLUUIRJXKIEG7DAQTAN6"
bbc <- readr::read_csv(bbc_git)

#Break in sentences 

bbc_sentences <- bbc %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% #unnest sentences
  mutate(sentence.no = 1:n()) #consecutive number for sentences

#Break in words

bbc_sentences <- bbc_sentences %>%
  unnest_tokens(word, sentence) #unnest words (words are the token by default)


#Adding sentiment column 

bbc_with_setiments <- bbc_sentences %>%
  inner_join(get_sentiments("afinn"))

#Average sentiment by sentence 

bbc_with_setiments_av <- bbc_with_setiments %>%
  mutate(date = lubridate::date(articles.published_datetime)) %>%
  group_by(sentence.no) %>%
  summarise(date = min(date),
            sentiment = mean(value))

#Average sentiment by date

bbc_av_by_date <- bbc_with_setiments_av %>%
  group_by(date) %>%
  summarise(sentiment = mean(sentiment)) %>%
  mutate(source = "BBC")

#Plot sentiment by day

ggplot(bbc_av_by_date, aes(date, sentiment)) +
  geom_col() 


# Export data

write_csv(bbc_av_by_date, path = 'bbc_sentence_sentiment.csv') 



