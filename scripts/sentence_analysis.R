#Libraries

library(tidyverse)

#Import data

cnn_sentiment <- read_csv("cnn_sentence_sentiment.csv")
cnn_sentiment <- cnn_sentiment %>%
  mutate(source = "CNN")

wsj_sentiment <- read_csv("wsj_sentence_sentiment.csv")

reuters_sentiment <- read_csv("reuters_sentence_sentiment.csv")

bbc_sentiment <- read_csv("bbc_sentence_sentiment.csv")


#Combine tibbles

news_sentence_sentiment <- rbind(cnn_sentiment, wsj_sentiment, 
                                 reuters_sentiment, bbc_sentiment)

#Plot facet

ggplot(news_sentence_sentiment, aes(date, sentiment, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 2)
