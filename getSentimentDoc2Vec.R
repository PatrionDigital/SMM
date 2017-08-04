# Trying a Deep Learning-based sentiment analysis method

#loading packages
library(magrittr)
library(twitteR)
library(ROAuth)
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

## loading and processing a training set of tweets
# convert symbols, such as emoji
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

## loading classified tweets ##
# source: http://htlp.sentiment140.com/for-students/
# 0 - the polarity of the tweet (0 = neg, 4 = pos)
# 1 - the id of the tweet
# 2 - the date of the tweet
# 3 - the query. itf there is no query, then this value is NO_QUERY
# 4 - the user that tweeted
# 5 - the text of the tweet

tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv', 
  col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replaceing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))
# there are some tweets with NA ids that we replace with dummies
tweets_classified_na <- tweets_classified %>%
  filter(is.na(id) == TRUE) %>%
  mutate(id = c(1:n()))
tweets_classified <- tweets_classified %>%
  filter(!is.na(id)) %>%
  rbind(., tweets_classified_na)

# data splitting on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8,
  list = FALSE,
  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

## Vectorization ##
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text,
  preprocessor = prep_fun,
  tokenizer = tok_fun,
  ids = tweets_train$id,
  progressbar = TRUE)
it_test <- itoken( tweets_test$text,
  preprocessor = prep_fun,
  tokenizer = tok_fun,
  ids = tweets_test$id,
  progressbar = TRUE)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train) #create_vocabulary(it = character_vectors, ngram = c(ngram_min, ngram_max), stopwords, sep_ngram = "_")
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit th emodel to the training data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

# train the model 
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet( x = dtm_train_tfidf,
  y = tweets_train[['sentiment']],
  family = 'binomial',
  # L1 penalty
  alpha = 1,
  # interested in the area uder ROC curve
  type.measure = "auc",
  # 5-fold cross-validation
  nfolds = 5,
  # high value is less accurate, but has faster training
  # changed from 1e-3 to 1e-8 (slower)
  thresh = 1e-8,
  # again lower number of iterations for faster training
  # changed from 1e3 to 1e8 (slower)
  maxit = 1e8)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
auc(as.numeric(tweets_test$sentiment), preds)

# save the model for future use
saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')
