catch.error = function(x) {
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

cleanTweets <- function(tweet){
  # Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all “#Hashtag”
  #tweet = gsub("#\\w+", " ", tweet)
  tweet = gsub("#", " ", tweet)
  # Then remove all “@people”
  #tweet = gsub("@\\w+", " ", tweet)
  tweet = gsub("@", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # remove emoji
  tweet = iconv(tweet, "latin1", "ASCII", sub = "")
  # remove specific terms
  tweet = gsub("(siggraph|siggraph2017)", "", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", "", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  
  # Next we'll convert all the word in lower case. This makes uniform pattern.
  tweet = catch.error(tweet)
  tweet
}

cleanTweetsAndRemoveNAs <- function (Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  TweetsCleaned = na.omit(TweetsCleaned)
  names(TweetsCleaned) = NULL
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

plotSentiments1 <- function (sentiment_dataframe,title) {
  require(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')
}

plotSentiments2 <- function (sentiment_dataframe,title) {
  require(ggplot2)
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')
}

siggraphTweetsCleaned = cleanTweetsAndRemoveNAs(siggraphTweets)

require(RSentiment)
# Calculates sentiment of each sentence. It classifies sentences into 6 categories: 
# Positive, Negative, Very Positive, Very Negative Sarcasm and Neutral.
siggraphClassEmo = calculate_sentiment(siggraphTweetsCleaned)

# Similar to above, we will classify polarity 
siggraphClassPol = calculate_score(siggraphTweetsCleaned)

# Let us now create a data frame with the above results 
siggraphSentimentDataFrame = data.frame(text=siggraphClassEmo$text, emotion=siggraphClassEmo$sentiment, polarity=siggraphClassPol, stringsAsFactors=FALSE)

# rearrange data inside the frame by sorting it
siggraphSentimentDataFrame = within(siggraphSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# with dataframes build, remove the polarity and sentiment objects
rm(siggraphClassEmo)
rm(siggraphClassPol)

plotSentiments1(siggraphSentimentDataFrame, 'Sentiment Analysis of #siggraph')

plotSentiments2(siggraphSentimentDataFrame, 'Polarity Analysis of #siggraph')