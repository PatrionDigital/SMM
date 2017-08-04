cleanCorpus <- function(myCorpus){
  # Transformations, including 
  # 1. changing letters to lower case, 
  # 2. removing punctuations/numbers 
  # 3. removing stop words. 
  library(tm)
    # remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
  # remove stopwords
  myStopwords <- c(stopwords('english'), 'available', 'via')
  idx <- which(myStopwords == 'r')
  myStopwords <- myStopwords[-idx]
  myCorpus <- tm_map(myCorpus, myStopwords)
  myCorpus <- tm_map(myCorpus, stemDocument)
  # set to lowercase
  myCorpus <- tm_map(myCorpus, tolower)
}








