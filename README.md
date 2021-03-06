# US-News-NLP
Natural language processing (sentiment analysis, topic modeling) and classification modeling on major news sources to measure bias on COVID-19 reporting.

*Andrew Pagtakhan, Kwan Bo Shim, Cinthia Jazmin Trejo Medina*


See ```/results/report.pdf``` for full analysis

## Directory Stucture
1.) ```/scripts```: 
- ```gnews_ingest_cnn_reuters.R``` and ```Scrap_and_clean_BBC_WSJ.R```
- ```combine_news_sources.R```

2.) ```/analysis```
- ```sentiment_analysis.R```
- ```feature_engineering.R```
- ```classification_models.R```

## Research Question

### Background
The means in which informaton is communicated with regards to the COVID-19 pandemic has had major
influence on how we read and learn about the virus through a multitude of media outlets. Some of these
major sources include television, YouTube, social media forums, and major news companies. Major news
companies in particular carry large influence based on the audiences it can reach. For example1, Fox News
Channel averaged 2.5 million primetime viewers (8pm - 11pm) in February 2021, and CNN averaged 1.7
million during the same time period.

According to King G.(1) & et. al., *“. . . the exposure to news media causes Americans to take public stands on
specific issues, join national policy conversation, and express themselves publicly”*. Furthermore, Holman E.(2) &
et. al, suggest a correlation between raising level of stress and prolonged media exposure to *“community-based
traumas (e.g., mass shootings, natural disasters)”*. Recently, Holman has suggested that COVID-19 is a
particular case to study since multiple stressors have arose at the same time. To mention a few stressors:
financial crisis, elections, and health crisis, among others. It can be said that news can influence the decisions,
general views and mental health of Americans.

The large influence that major news companies have on how information is commmunicated to its audiences,
and the impact news have on its readers makes it vital to quantify how different these sources are in relation
to COVID-19 news. By measuring potential bias in relation to each source, this analysis examines how
different major news sources are when reporting on COVID-19. It also explores underlying patterns such as
subtopics within COVID-19 that are published more in news sources over others. And if these patterns are
predictive of which news source the article came from.

(1) King, G., Schneer, B., & White, A. (2017). How the news media activate public expression and influence
national agendas. American Association for the Advancement of Science. Vol.358 (6364), pp.776-780.
https://science.sciencemag.org/content/358/6364/776

(2) Holman, E., Garfin, D., & Silver, R. (2014). Media’s role in broadcasting acute stress following the Boston
Marathon bombings. Proceedings of the National Academy of Sciences of the United States of America.
Vol.111 (1), pp.93- 98. https://doi.org/10.1073/pnas.1316265110

### Research Question
Are there underlying patterns in news articles related to COVID-19 across major news sources that are
suggestive of bias, and are these patterns predictive of which news source it is likely from?
### Applications
By quantifying underlying differences on COVID-19 reporting and examining the predictive power of these
patterns to identify the news source, this study can be useful for a number of cases. For example, understanding
biases in article text can help the reader understand inherent ideological leanings towards certain news sources.
This can help to equip them with greater understanding and critical examination of news consumption. In
the same way, it can help readers to be more selective when choosing news sources and reduce their stress
impact. 

From a policy perspective, greater impact and studies could be done to influence greater transparency6 in
reporting across the news companies. This can assist in making informed decisions on which news sources to
read or be aware of the bias different sources might have.

## Data sourcing
Pulled article urls and metadata from GNews API: https://gnewsapi.net/ (5,360 articles queried, 3,454 articles returned for the time period 1/1/2020 - 4/9/2021). Scraped full article text across the following news sources:
- BBC
- CNN
- Reuters
- The Wall Street Journal

## Key Data fields
Key fields pulled from the GNews API and web scraping of full article text:
- ```article url```
- ```article description```
- ```article published date and time```
- ```source name```
- ```full article text```

## Exploratory Data Analysis

Key visualizations of the data are shown below.

![words_per_article](https://user-images.githubusercontent.com/60185641/117580019-cda22900-b0c3-11eb-804a-e8f375804db3.png)
![top_words_with_sentiment](https://user-images.githubusercontent.com/60185641/117580022-d266dd00-b0c3-11eb-8cc3-f3c7ce6a25ca.png)
![sentence_afinn_sentiment_week](https://user-images.githubusercontent.com/60185641/117580027-d85cbe00-b0c3-11eb-883d-38ee9567057b.png)


## Feature Engineering

Engineered features using the following methodologies:
- average Afinn sentiment by word
- average Afinn sentiment by sentence
- word count
- word count for words with a sentiment
- Keyword features using prior knowledge of researchers based on the following topics: politics, business, and pandemic
- Keyword features based on prior studies from: Pew Research Center and SagePub
- Topic Modeling average probabilities (Latent Dirichlet Allocation)

LDA Topics (k = 7):
Topic Number | Topic
--- | --- |
1 | Politics 
2 | Reported cases and deaths
3 | China / Wuhan
4 | Outbreaks and infections by country
5 | Patients and symptoms
6 | Vaccines and research
7 | Business and economy

![lda_top_terms](https://user-images.githubusercontent.com/60185641/117580170-a730bd80-b0c4-11eb-99cd-4ce763fe32d9.png)


## Measuring Bias Across News Sources

Bias was measured by running chi-square tests on the following metrics, split by data source. 
The p-values were < 1% for each of these methods, suggesting there are significant differences across each news source:
- Topic Probabilities
- Average Afinn sentiment (sentence)
- Word count

![topic_probabilities_news_source](https://user-images.githubusercontent.com/60185641/117580042-ead6f780-b0c3-11eb-92ae-3736bde75dca.png)
![sentence_afinn_sentiment](https://user-images.githubusercontent.com/60185641/117580071-04783f00-b0c4-11eb-94ce-8bf273fb66b1.png)
![avg_word_count](https://user-images.githubusercontent.com/60185641/117580055-f4f8f600-b0c3-11eb-838d-57d1fc9afacd.png)

## Classification Modeling

For each algorithm listed below, four sets of models were run: a.) all features, b.) topic modeling features, c.) keyword features, d.) topic keyword features based on previous reseach papers:

- Naive Bayes
- Support Vector Machine
- Logistic Regression
- Random Forest
- Gradient Boosting Machine

Of all the models run, the random forest utilizing all features performed the best. Since the news sources have inherent topics they are more likely to write about, 
such as business for WSJ, the random forest can model these patterns more strongly vs. other models such as logistic regression.

Conversely, the Naive Bayes and Support Vector Machine models did not perform well due to the following factors:

- Strong assumptions: Naive Bayes assumes independence of features, which is not the case in our data

- Binary vs. Multi-class predictions: The two models are better suited for binary classifications, 
as opposed to multi-class problems

- Boundary splitting: Naive Bayes and SVM perform better when the data structure naturally splits 
the data to accurately classify the news source. Since the data does not have clear patterns that 
these two algorithms can split the data, it does not perform as well.

![model_results](https://user-images.githubusercontent.com/60185641/117589543-1a9ff280-b0f8-11eb-991f-919c42bafb51.png)



### Conclusion
*Refer to /results/report.pdf*

### Limitations
*Refer to /results/report.pdf*

### Next steps
*Refer to /results/report.pdf*
