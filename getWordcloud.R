tweets <- paste(siggraphTweetsCleaned, collapse = " ")

require(tm)
require(wordcloud)
require(RColorBrewer)

tweets <- removeWords(tweets, c(stopwords("english"), "siggraph", "siggraph2017", "atsiggraph", "siggraphis", "forsiggraph", "los", "angeles", "convention"))
# create a corpus
corpus <- Corpus(VectorSource(tweets))
# create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
# convert to matrix
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

# create the wordcloud
set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 3, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
findFreqTerms(tdm, lowfreq = 4)

barplot(d[1:10,]$freq, names.arg = d[1:10,]$word, col = "lightblue", main = "Most frequest words", ylab = "Word Frequencies")
