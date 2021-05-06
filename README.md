# US-News-NLP
Sentiment analysis and natural language processing on major news sources to measure bias on COVID-19 reporting.

## Research Question

### Background
The means in which informaton is communicated with regards to the COVID-19 pandemic has had major
influence on how we read and learn about the virus through a multitude of media outlets. Some of these
major sources include television, YouTube, social media forums, and major news companies. Major news
companies in particular carry large influence based on the audiences it can reach. For example, Fox News
Channel averaged 2.5 million primetime viewers (8pm - 11pm) in February 2021, and CNN averaged 1.7
million during the same time period. Source: https://www.foxnews.com/media/ fox-news-finishes-februarymost-
watched-primetime-network.
Since major news companies have a large influence in how information is commmunicated to its audiences, it
is vital to quantify how different these sources are in relation to COVID-19 news. By measuring potential
bias in relation to each source, this analysis examines how different major news sources are when reporting
on COVID-19. It is also insightful to see if there are underlying patterns such as subtopics within COVID-19
that are published more in news sources over others. This would help identify if there are patterns that are
predictive of which news source the article came from.
### Resarch Question
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
- Pulled article urls and metadata from GNews API: https://gnewsapi.net/
- Scraped > full article text across the following news sources:
- 1. BBC:
- 2. CNN:
- 3. Reuters
- 4. The Wall Street Journal

## Key Data fields
Key fields pulled from the GNews API and web scraping of full article text:
- ```article url```
- ```article description```
- ```article published date and time```
- ```source name```
- ```full article text```

## Feature Engineering

Engineered features using the following methodologies:
- average Afinn sentiment by word
- average Afinn sentiment by sentence
- word count
- word count for words with a sentiment
- Keyword features using prior knowledge of researchers based on the following topics: politics, business, and pandemic
- Keyword features based on prior studies from: Pew Research Center and SagePub
- Topic Modeling average probabilities (Latent Dirichlet Allocation)

To do:
- Measure bias from LDA topic probabilities, avg sentiment, and word count: Chi-Square test
- Try LDA tuning to further optimize on number of topics (already tried 5 and there was too much overlap in top words)
- Use Lasso Regression for feature selection
- Consider combining results of all models (hybrid model)
- Add more sources for Background section
