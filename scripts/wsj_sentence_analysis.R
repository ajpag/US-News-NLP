#Libraries
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)

#Read data from github
wsj_git <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_wsj.csv?token=ATPY7SU6PSKE4BGMV6VHAB3AQTA4E"
wsj <- readr::read_csv(wsj_git)

#Break in sentences 

wsj_sentences <- wsj %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% #unnest sentences
  mutate(sentence.no = 1:n()) #consecutive number for sentences

#Break in words

wsj_sentences <- wsj_sentences %>%
  unnest_tokens(word, sentence) #unnest words (words are the token by default)


#Adding sentiment column 

wsj_with_setiments <- wsj_sentences %>%
  inner_join(get_sentiments("afinn"))

#Average sentiment by sentence 

wsj_with_setiments_av <- wsj_with_setiments %>%
  mutate(date = lubridate::date(articles.published_datetime)) %>%
  group_by(sentence.no) %>%
  summarise(date = min(date),
            sentiment = mean(value))

#Average sentiment by date

wsj_av_by_date <- wsj_with_setiments_av %>%
  group_by(date) %>%
  summarise(sentiment = mean(sentiment)) %>%
  mutate(source = "Wall Street Journal")

#Plot sentiment by day

ggplot(wsj_av_by_date, aes(date, sentiment)) +
  geom_col() 


# Export data

write_csv(wsj_av_by_date, path = 'wsj_sentence_sentiment.csv') 



