#install.packages(c("devtools", "rjson", "bit64", "httr"))
# RESTART R session!
# library(devtools)
# install_github("twitteR", username="geoffjentry")

require(twitteR)
api_key <- "LcOPPlyMAV3u8Ymy0wlvBEzxO"
api_secret <- "rrvUxwEandPEiBHPUloWQqgtz6L4NS0MhubMaBAFsWA08tC0nR"
access_token <- "8130922-UxTjJkB8ZDOOD4paanzUJtWjuFMOrnMKbfBEE2akTz"
access_token_secret <- "WvEQsYTahlKAbXp02bpO2hFlBaIhedwbKDsyzFsITm6x2"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
rm(api_key)
rm(api_secret)
rm(access_token)
rm(access_token_secret)