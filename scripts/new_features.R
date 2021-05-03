#Libraries
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)

#Read raw data from github
all_news_raw <- "https://raw.githubusercontent.com/ajpag/US-News-NLP/main/data/news_all.csv?token=ATPY7SRUYDGW4OEMMIRCIUDAR5TTU"
all_news <- readr::read_csv(all_news_raw)

all_news <- all_news %>%
  mutate(article.no = 1:n()) #consecutive number for articles

#Classification variables by words

all_news_class <- all_news %>%
  mutate(text1 = tolower(text), #Transform text to lowercase
         Biden = grepl("biden", text1)*1, #Classification indexes
         Trump = grepl("trump", text1)*1,
         stock_market = grepl("stock market", text1)*1,
         financial = grepl("financial", text1)*1,
         death = grepl("death", text1)*1,
         pandemic = grepl("pandemic", text1)*1,
         disease = grepl("disease", text1)*1,
         illness = grepl("illness", text1)*1)

#Checking there are articles with the word
sum(all_news_class$Biden)
sum(all_news_class$Trump)
sum(all_news_class$stock_market)
sum(all_news_class$financial)
sum(all_news_class$death)
sum(all_news_class$pandemic)
sum(all_news_class$disease)
sum(all_news_class$illness)


#Splitting in words

all_news_words <- all_news_class %>%
  unnest_tokens(word, text1, token = "words")

#Adding sentiment

all_news_with_setiments <- all_news_words %>%
  inner_join(get_sentiments("afinn")) %>% #adding sentiment to all words
  mutate(Biden_sentiment = Biden*value, #Estimating sentiment by classification
         Trump_sentiment = Trump*value,
         stock_market_sentiment = stock_market*value,
         financial_sentiment = financial*value,
         death_sentiment = death*value,
         pandemic_sentiment = pandemic*value,
         disease_sentiment = disease*value,
         illness_sentiment = illness*value) #29 articles are lost when sentiment is added

table(all_news_with_setiments$value) #There are no zeros in the original words sentiment

#Creating new features

new_features <- all_news_with_setiments %>%
  group_by(article.no) %>%
  summarise(articles.published_datetime = max(articles.published_datetime),
            articles.title = max(articles.title),
            text = max(text),
            Biden_sentiment = mean(Biden_sentiment),
            Trump_sentiment = mean(Trump_sentiment),
            stock_market_sentiment = mean(stock_market_sentiment),
            financial_sentiment = mean(financial_sentiment),
            death_sentiment = mean(death_sentiment),
            pandemic_sentiment = mean(pandemic_sentiment),
            disease_sentiment = mean(disease_sentiment),
            illness_sentiment = mean(illness_sentiment))

write_csv(new_features, path = 'sentiment_by_words.csv')


