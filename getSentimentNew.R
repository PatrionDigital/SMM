require(sentimentr)

sig2017Sentiment = sentiment(sig2017TweetsCleaned, polarity_dt = lexicon::hash_sentiment_jockers,
                             valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
                             amplifier.weight = 0.8, n.before = 5, n.after = 2,
                             question.weight = 1, adversative.weight = 0.85, missing_value = 0 )
siggraphSentiment = sentiment(siggraphTweetsCleaned, polarity_dt = lexicon::hash_sentiment_jockers,
                             valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
                             amplifier.weight = 0.8, n.before = 5, n.after = 2,
                             question.weight = 1, adversative.weight = 0.85, missing_value = 0 )
siggraphOfficialSentiment = sentiment(siggraphOfficialTweetsCleaned, polarity_dt = lexicon::hash_sentiment_jockers,
                             valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
                             amplifier.weight = 0.8, n.before = 5, n.after = 2,
                             question.weight = 1, adversative.weight = 0.85, missing_value = 0 )
sig2017Plot = plot(sig2017Sentiment,transformation.function = syuzhet::get_dct_transform)
siggraphPlot = plot(siggraphSentiment,transformation.function = syuzhet::get_dct_transform)
siggraphOfficialPlot = plot(siggraphOfficialSentiment,transformation.function = syuzhet::get_dct_transform)

