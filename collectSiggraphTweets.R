# Load the necessary packages
source('authenticate.R')
#source('utils.R')
# Clear the workspace if there are any objects remaining.
rm (list=ls())
# Search Twitter within 100km of Los Angeles Convention Center
LAConventionGeoCode = '34.0403022,-118.2695531,100km'
siggraph_tweets = searchTwitter("Siggraph OR siggraph2017 OR #siggraph", n=2000, lang="en", geocode = LAConventionGeoCode, retryOnRateLimit = 10)

rm(LAConventionGeoCode)

siggraphTweets = sapply(siggraph_tweets, function(x) x$getText())