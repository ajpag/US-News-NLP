# US-News-NLP
Natural language processing (sentiment analysis, topic modeling) and classification modeling on major news sources to measure bias on COVID-19 reporting.

## Directory Stucture
```/results```: See ```report.pdf``` for full analysis

Order (final batch script to be added):

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
companies in particular carry large influence based on the audiences it can reach. For example, Fox News
Channel averaged 2.5 million primetime viewers (8pm - 11pm) in February 2021, and CNN averaged 1.7
million during the same time period. Since major news companies have a large influence in how information is commmunicated to its audiences, 
it is vital to quantify how different these sources are in relation to COVID-19 news. By measuring potential
bias in relation to each source, this analysis examines how different major news sources are when reporting
on COVID-19. It is also insightful to see if there are underlying patterns such as subtopics within COVID-19
that are published more in news sources over others. This would help identify if there are patterns that are
predictive of which news source the article came from.
### Research Question
Are there underlying patterns in news articles related to COVID-19 across major news sources that are
suggestive of bias, and are these patterns predictive of which news source it is likely from?
### Use Cases
By quantifying underlying differences on COVID-19 reporting and examining the predictive power of these
patterns to identify the news source, this study can be useful for a number of use cases. For example,
understanding biases in article text can help the reader understand inherent idealogical leanings towards
certain news sources, which can help equip them with greater understanding and critical examination of
news consumption. From a policy perspective, greater impact and studies could be conducted to place more
standardized regulations in an effort to influence more objective reporting. We acknowledge this second
use case can be difficult from business and philosophical perspectives, especially in relation to the First
Amendement of the US Constitution in relation to the freedom of speech.

## Data sourcing
Pulled article urls and metadata from GNews API: https://gnewsapi.net/. Scraped full article text across the following news sources:
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

To assess which features were most predictive of identifying the correct news source for a given news article, the following models were run:
Model | Test Accuracy | AUC
--- | --- | ---
Support Vector Machine | |
Naive Bayes | |
Logistic Regression| |
Random Forest | |
Gradient Boosting Machine | |

## Results

### Conclusion and next steps

### Limitations

## To do:
- Try LDA tuning to further optimize on number of topics (already tried 5 and there was too much overlap in top words)
- Use Lasso Regression for feature selection
- Consider combining results of all models (hybrid model)
- Add more sources for Background section
