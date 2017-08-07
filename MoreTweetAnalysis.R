# Number of unique Twitter accounts in the sample
length(unique(df_tweets$screenName))

# Generate a column of random numbers to replace screen names
n <- length(unique(df_tweets$screenName))
randuser <- round(runif(n,1000,9999),0)
# Match up a random number to a username
screenName <- unique(df_tweets$screenName)
screenName <- sapply(screenName, as.character)
randuser <- cbind(randuser, screenName)
# Merge the random numbers with the rest of the Twitter data
rand.df <- merge(randuser, df_tweets, by="screenName")

# determine the frequency of tweets per account
counts <- table(rand.df$randuser)
# create an ordered data frame for further manipulation and plotting
countsSort <- data.frame(user = unlist(dimnames(counts)), count = sort(counts, decreasing = TRUE), row.names = NULL)
# create a subset of those who tweeted at least 5 times or more
countsSortSubset <- subset(countsSort,countsSort$count > 0)
# extract counts of how many tweets from each account were retweeted
# first clean the twitter messages by removing odd characters
rand.df$text <- sapply(rand.df$text,function(row) iconv(row,to = 'UTF-8')) 
# remove @ symbol from user names
trim <- function (x) sub('@','',x) 
# pull out who the message is to
require(stringr)
rand.df$to <- sapply(rand.df$text, function(name) trim(name)) 
# extract who has been retweeted
rand.df$rt <- sapply(rand.df$text, function(tweet) 
  trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2])) 
# replace names with corresponding anonymising number 
randuser <- data.frame(randuser)
rand.df["rt.rand"] <- NA
#this next line is not working.--
rand.df$rt.rand <- as.character(randuser$randuser)[match(as.character(rand.df$rt), 
                                                         as.character(randuser$screenName))]
# make a table with anonymised IDs and number of RTs for each account
countRT <- table(rand.df$rt.rand)
countRTSort <- sort(countRT)
# subset those people RT'd at least twice
countRTSortSubset <- subset(countRTSort, countRT > 2)
# create a data frame for plotting
countRTSortSubset.df <- data.frame(user = as.factor(unlist(dimnames(countRTSortSubset))), RT_count = as.numeric(unlist(countRTSortSubset)))
# combine tweet and retweet counts into one data fram
countUser <- merge(randuser, countsSortSubset, by.x = "randuser", by.y = "user")
TweetRetweet <- merge(countUser, countRTSortSubset.df, by.x = "randuser", by.y = "user", all.x = TRUE)
# create a Cleveland dot plot of tweet counts and retweet counts per Twitter account
# solid data point = number of tweets, letter R = number of retweets
require(ggplot2)
require(grid)
ggplot() +
  geom_point(data = TweetRetweet, mapping = aes(reorder(randuser, count), size = 3)) +
  geom_point(data = TweetRetweet, mapping = aes(randuser, RT_count), size = 4, shape = "R") +
  xlab("Author") +
  ylab("Number of messages") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
  theme(axis.title.y = element_text(size = 14, angle = 90)) +
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

# calculate the number of follower of each Twitter account
# extract the usernames from the non-anoymised dataset
users <- unique(df_tweets$screenName)
users <- sapply(users, as.character)
# make a data frame for further manipulation
users.df <- data.frame(users = users, follower = "", stringsAsFactors = FALSE)
# loop to populate users$followers with a follower count obtained from Twitter API
for (i in 1:nrow(users.df)) {
  # skip a user if account is protected
  result <- try(getUser(users.df$users[i]$followersCount, silent = FALSE));
  if (class(result) == "try-error") next;
  # get number of followers for each user
  users.df$followers[i] <- getUser(users.df$users[i])$followersCount
  # tell the loop to pause for 60 s between iterations
  print('Sleeping for 60 seconds..')
  Sys.sleep(60);
}

# merge follower count with number of tweets per author
followerCounts <- merge(TweetREtweet, users.df, by.x = "screenName", by.y = "users")
# convert to value to numeric for further anlysis
followerCounts$follower <- as.numeric(followerCounts$followers)
followerCounts$counts <- as.numeric(followerCounts$counts)

# create a plot
ggplot(data = followerCounts, aes(count, followers)) +
  geom_text(aes(label = randuser, size = RT_count)) +
              scale_size(range = c(3,10)) +
              scale_x_log10(breaks = c(10,20,40,60,80,100)) +
              scale_y_log10(breaks = c(10,100,seq(1000,7000,1000))) +
              xlab("Number of Messages") +
              ylab("Number of Followers") +
              theme_bw() +
              theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
              theme(axis.title.y = element_text(size = 14, angle = 90)) +
              theme(plot.margin = unit(c(1,1,2,2), "lines"))