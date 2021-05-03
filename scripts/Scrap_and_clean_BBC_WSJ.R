
####################################
# Scrap and cleaning datasets
####################################

#========== 0.  import========================================
#install.packages('newsanchor')
#install.packages('robotstxt')
#install.packages('httr')
#install.packages('textdata')
library(newsanchor)  ; library(robotstxt)   ; library(httr)       
library(rvest)       ; library(dplyr)       ; library(stringr)    
library(tidytext)   ; library(textdata)  
#install.packages("rjson")
library("rjson")

#==========  1.scrap -> GNEWS  ========================================
#==========  BBC   ========================================
# https://documenter.getpostman.com/view/12365554/TVep87Q1#intro
# https://gnewsapi.net


#import requests
basis_url = 'https://gnewsapi.net/api/search?'
keyword <- 'notre-dame'
country <- 'us'
language <- 'en'
date <- '&from=2019-01-01&to=2019-06-30'
inurl <- 'www.bbc' # BBC 
page_limit <- '100' # 20 count
api_token <- '[input here]'
url <- paste0(basis_url, 'q=', keyword, '&country=', 
              country,'&language=', language, date, '&inurl=', inurl,
              '&limit=', page_limit, "&api_token=", api_token )

asjson <- content(GET(url), as = "text")
result <- fromJSON(asjson)
result <- as.data.frame( result )


# delete if it is not NEWS section
result_new <- result_new %>% filter( str_detect(result_new$articles.article_url, "news") )
nrow( result_new )

table = data.frame()
for (i in 1:nrow( result_new )){
  response <- read_html( result_new$articles.article_url[i] )
  # 1. crime
  head<- html_nodes(x = response, xpath = '//h1[contains(@id, "main-heading")]')
  head <- html_text(head, trim = T)
  # 2. body text
  body_text <- html_nodes(x = response, xpath = '//div[contains(@class, "RichTextContainer")]') 
  body_text<- html_text(body_text, trim = F)
  body_text <- paste(body_text, collapse = " ")
  # 3. tibble
  table[i,1] <- head
  table[i,2] <- body_text
}
result_new <- cbind(result_new, table[,2])
result_new

#==========  2.cleaning messy data : BBC ========================================
#load
news_bbc <- read.csv("./data/news_bbc.csv")
head(news_bbc$text)[1]

# cleaning messy 
# new line

erase_this <- "US-designated terrorist groups across the Middle East - including Lebanon's Hezbollah movement and Palestinian Islamic Jihad"
erase_this1 <-"by providing funding, training, weapons and equipment. The US designated Iran's Revolutionary Guards and its Quds Force as foreign terrorist organisations in April. "
erase_this2 <-"The US designated Iran's Revolutionary Guards and its Quds Force as foreign terrorist organisations in April"
erase_this3 <-"Why kill Soleimani now and what happens next? Who was Iran's Qasem Soleimani? Defiant Iranians mourn 'martyr' Soleimani Protesters withdraw from US embassy in Iraq US-Iran relations: A brief history Iran's network of influence in Mid-East 'growing' Indian hospitals send SOS as Covid toll surges Patients are dying because of a lack of medical oxygen supplies, doctors warn."
erase_this4 <-"Biden says Armenian mass killing was genocide A city where breathing has become a luxury Why Elon Musk's SpaceX has got a tiny island angry Have India's rallies helped spread coronavirus?"
erase_this5 <-"It's the Oscars - but who is watching? Music video and wedding filmed at erupting volcano."
erase_this6 <-"VideoMusic video and wedding filmed at erupting volcano Trauma, genocide and my invisible illness 'They wanted to tie me and my child to a horse' Palestinian leaders face reckoning as rare elections loom George Floyd: How Black Lives Matter protesters feel now."
erase_this7 <-"VideoGeorge Floyd: How Black Lives Matter protesters feel now 'As long as I can play in the Olympics, I'm happy' Video'As long as I can play in the Olympics, I'm happy' Football phrases 15 sayings from around the world Man arrested for infecting 22 people with Covid1 Gymnasts' outfits take on sexualisation in sport2 Indian hospitals send SOS as Covid toll surges3 "
erase_this8 <-"Biden says Armenian mass killing was genocide4 Missing Indonesian submarine sank off Bali5 ESL clubs 'cannot leave', says Perez6"

erase_this12 <- "Biden says Armenian mass killing was genocide4 Missing Indonesian submarine sank off Bali5 Sia named worst director for her film Music6 ESL clubs 'cannot leave', says Perez7 'They wanted to tie me and my child to a horse'8 A city where'"

erase_this9 <-"They wanted to tie me and my child to a horse'7 Sia named worst director for her film Music8 A city where"
erase_this10 <-"breathing has become a luxury'9 Second cryptocurrency platform closes in Turkey10 © 2021 BBC. The BBC is not responsible for the content of external sites. Read about our approach to external linking."
erase_this11 <- "Indian hospitals send SOS as Covid toll surges Patients are dying because of a lack of medical oxygen supplies, doctors warn. "



news_bbc<-  news_bbc %>%
  mutate(text_new = text) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this1), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this2), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this3), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this4), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this5), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this6), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this7), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this8), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this9), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this10), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this11), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_this12), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed("'"), "") )


# check data
(news_bbc$text_new)[15:20]

# ======== save as csv =================
write.csv(news_bbc, "news_bbc_new.csv")








#=========================  WSJ   ========================================
# WSJ
# Terms of Use
#https://www.wsj.com/public/resources/documents/reprints-wsj-terms.html

#You may download, reformat and print a limited amount of WSJ.com content for your personal, n
#on-commercial use. You may also include excerpts or limited portions of WSJ.com information in printed memos, 
#reports and presentations. We only ask that you attribute this information to us by including 
#"Used with permission from The Wall Street Journal, WSJ.com. Copyright 200__ D
#ow Jones & Company, Inc. All rights reserved."

# parameters
token <- 'oOPiNNaJs1dbpYD2fupQtpMYCj47vx1U6EDmDgaeMaOHEW7yhH8Tcvl3SrxB' 
topic <- "\"COVID-19\""
country <- "us"
language <- "en"
limit <- "20"
source_wsj <- "www.wsj.com/articles"

# CNN API dataframe
news_wsj <- get_api_data(topic, country, language, dates_start, 
                         dates_end, source = source_wsj, limit, token)

# scrap body text
# This is the one that I changed. It gives less messy data
get_wsj_text <- function(url) {
  url_get <- read_html(url)
  df_text <- url_get %>%
    html_nodes("p") %>%
    html_text(trim = TRUE) 
  df_text <- paste( df_text, collapse = " ")
  df_text }

get_article_text <- function(news_url, source) {
  text <- get_wsj_text(news_url)    
  return(text)  } 

text_wsj <- lapply(news_wsj$articles.article_url, 
                   get_article_text, 
                   source = "Wall Street Journal")

news_wsj$text <- NA
for (i in 1:length(text_wsj)) {
  news_wsj$text[i] <- text_wsj[i]
}

news_wsj$text <-  gsub("\\n", "" , news_wsj$text, ignore.case = TRUE)
news_wsj$text <-  gsub(" \"WSJ Membership\", \"Customer Service\", \"Tools & Features\", \"Ads\", \"More\", \"Dow Jones Products\"", " " , news_wsj$text, ignore.case = TRUE)
news_wsj$text <-  gsub("\"Copyright © 2021 Dow Jones & Company, Inc. All Rights Reserved\""   , "" , news_wsj$text, ignore.case = TRUE)
news_wsj$text <-  gsub(" \"WSJ Membership\", \"Customer Service\", \"Tools & Features\", \"Ads\", \"More\", \"Dow Jones Products\"", " " , news_wsj$text, ignore.case = TRUE)
news_wsj$text <-  gsub("\"WSJ Membership\", \"Customer Service\", \"Tools & Features\", \n\"Ads\", \"More\", \"Dow Jones Products\"", " " , news_wsj$text, ignore.case = TRUE)
news_wsj$text <-  gsub("\"Risk management, strategy and analysis from Deloitte\", \"Where new financial products proliferate, new opportunities for abusive practices soon follow. So it goes in the digital asset marketplace, where many regulators are considering what more to do. In the meantime, institutions could adopt proactive, transparent measures to protect investors.\",", " " , news_wsj$text, ignore.case = TRUE)
news_wsj$text <-  gsub("\"Please note: The Wall Street Journal News Department was not involved in the creation of the content above.\",", " " , news_wsj$text, ignore.case = TRUE)
news_wsj$text <-  gsub("WSJ Membership Customer Service Tools & Features Ads More Dow Jones Products", " " , news_wsj$text, ignore.case = TRUE)

#write to csv
write.csv(news_wsj, "news_wsj.csv")



#====================== cleaning part : WSJ   ========================================
news_wsj <- read.csv("./news_wsj.csv")

#check
#(news_wsj$text)[100]

#1. detecting irrelevant sentences
erase_wsj1 <- "Please note: The Wall Street Journal News Department was not involved in the creation of the content above."
erase_wsj2 <- "Copyright © 2021 Dow Jones & Company, Inc. All Rights Reserved"
erase_wsj3 <- "Risk management, strategy and analysis from Deloitte Where new financial products proliferate, new opportunities for abusive practices soon follow. So it goes in the digital asset marketplace, where many regulators are considering what more to do. In the meantime, institutions could adopt proactive, transparent measures to protect investors."
erase_wsj4 <- "Copyright ©2020 Dow Jones & Company, Inc. All Rights Reserved."
erase_wsj5 <- "Write to Mengqi Sun at mengqi.sun@wsj.com "
erase_wsj6 <- "Write to Christopher Weaver at christopher.weaver@wsj.com and Anna Wilde Mathews at anna.mathews@wsj.com "
erase_wsj7 <- "Write to Julie Bykowicz at julie.bykowicz@wsj.com and Catherine Lucey at catherine.lucey@wsj.com"
erase_wsj8 <- "Write to Agam Shah at agam.shah@wsj.com"
erase_wsj9 <- "CIO Insights and Analysis from Deloitte By definition, architectural design is a highly in-person process. How do you replace material samples, whiteboarding, prototypes, and walk-throughs with virtual equivalents? Global architectural firm Gensler had done enough advanced digital planning to make the transition quickly when the pandemic struck. Now, its leaders aren’t looking back."
erase_wsj10 <- "Write to Brianna Abbott at brianna.abbott@wsj.com"
erase_wsj11 <- "Appeared in the January "
erase_wsj12 <- "Write to Betsy McKay at betsy.mckay@wsj.com"
erase_wsj13 <- "print edition as '.'"
erase_wsj14 <- "“Global View” analyzes ongoing developments in foreign affairs, with a particular focus on American strategy and geopolitics. The column appears on the Wall Street Journal’s website every Monday evening and Tuesdays in print. Walter Russell Mead is the Ravenel B. Curry III Distinguished Fellow in Strategy and Statesmanship at Hudson Institute, the Global View Columnist at The Wall Street Journal and the James Clarke Chace Professor of Foreign Affairs and Humanities at Bard College in New York.  He is also a member of Aspen Institute Italy and board member of Aspenia. Before joining Hudson, Mr. Mead was a fellow at the Council on Foreign Relations as the Henry A. Kissinger Senior Fellow for U.S. Foreign Policy. He has authored numerous books, including the widely-recognized Special Providence: American Foreign Policy and How It Changed the World (Alfred A. Knopf, 2004). Mr. Mead’s next book is entitled The Arc of A Covenant: The United States, Israel, and the Future of the Jewish People"
erase_wsj15 <-"Write to Costas Paris at costas.paris@wsj.com"
erase_wsj16 <-"—Kersten Zhang contributed to this article. Write to Chao Deng at Chao.Deng@wsj.com"


# 2. remove the sentences
## create new text_new column to make comparison
news_wsj <- news_wsj %>%
  mutate(text_new = text) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj1), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj2), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj3), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj4), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj5), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj6), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj7), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj8), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj9), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj10), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj11), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj12), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj13), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj14), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj15), "") ) %>%
  mutate(text_new = str_replace(text_new, fixed(erase_wsj16), "") )


# save it as csv format
write.csv(news_wsj, "news_wsj_new.csv")




# the end
