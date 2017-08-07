## Fetching Tweets ##
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
  destfile = "cacert.pem")
source('authenticate.R')

df_tweets <- twListToDF(searchTwitter('siggraph OR #siggraph', n = 2500, lang = 'en')) %>%
  #converting some symbols (emoji, etc)
  dmap_at('text', conv_fun)

# Number of unique Twitter accounts in the sample
length(unique(df_tweets$screenName))

# preprocessing and tokenization
it_tweets <- itoken(df_tweets$text,
  preprocessor = prep_fun,
  tokenizer = tok_fun,
  ids = df_tweets$id,
  progressbar = TRUE)

# create the vocabulary and document-term matrix
dtm_tweets <- create_dtm(it_tweets, vectorizer)

# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# loading classification model
glmnet_classifier <- readRDS('glmnet_classifier.RDS')

# predict probabilities of positiveness
preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]

# adding predicted probabilities to the initital dataset
df_tweets$sentiment <- preds_tweets