require(tm)
require(wordcloud)
require(RColorBrewer)

# create a corpus
x <- as.vector(it_tweets$iterable)
corpus <- Corpus(VectorSource(x))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), 
  "siggraph", "siggraph2017",
  "atsiggraph", "siggraphis",
  "forsiggraph", "los", "new",
  "angeles", "convention",
  "https", "http", "2017"))
rm(x)
# create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
# convert to matrix
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
rm(m)
rm(v)

# create the wordcloud
set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 3, max.words = 250, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

barplot(d[1:10,]$freq, names.arg = d[1:10,]$word, col = "lightblue", main = "Most frequent words", ylab = "Word Frequencies", las=2)
#findFreqTerms(tdm, lowfreq = 4)

# colour palette
cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")

set.seed(932)
samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.1) # 10% for labelling

# plotting
ggplot(df_tweets, aes(x = created, y = sentiment, color = sentiment)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(0, 1),
  breaks = seq(0, 1, by = 1/4),
  labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
  guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = sentiment), alpha = 0.8) +
  geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  geom_label_repel(data = df_tweets[samp_ind, ],
    aes(label = round(sentiment, 2)),
    fontface = 'bold',
    size = 2.5,
    max.iter = 100) +
  theme(legend.position = 'bottom',
    legend.direction = "horizontal",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
    axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Tweets Sentiment rate (probability of positiveness)")