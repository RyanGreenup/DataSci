
# Load Packages -----------------------------------------------------------

if (require("pacman")) {
  library(pacman)
} else{
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap, parallel,
               dplyr, plotly, tidyverse, reticulate, UsingR, Rmpfr, swirl,
               corrplot, gridExtra, mise, latex2exp, tree, rpart, lattice, coin,
               primes, epitools, maps, clipr, ggmap, twitteR, ROAuth, tm,
               rtweet, base64enc, httpuv, SnowballC, RColorBrewer, wordcloud,
               ggwordcloud)

mise()


# Set up Tokens ===========================================================

options(RCurlOptions = list(
  verbose = FALSE,
  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
  ssl.verifypeer = FALSE
))

setup_twitter_oauth(
  consumer_key = "dE7HNKhwHxMqqWdBl1qo9OAkN",
  consumer_secret = "7ByyC6lR9d2aqLoXHHBkOtKRiU10cHhGguroiGKQ69AGDMMI9V",
  access_token = "1240821178014388225-sFWver0NDDY3BhPdPyg8d4mtQxnl0K",
  access_secret = "HLBWzHcemHzYJnw5ZLvKpEhQ5KaWwK6Nsj6cxBjUf51NJ"
)

# rtweet ==================================================================
tk <-    rtweet::create_token(
  app = "SWA",
  consumer_key    = "dE7HNKhwHxMqqWdBl1qo9OAkN",
  consumer_secret = "7ByyC6lR9d2aqLoXHHBkOtKRiU10cHhGguroiGKQ69AGDMMI9V",
  access_token    = "1240821178014388225-sFWver0NDDY3BhPdPyg8d4mtQxnl0K",
  access_secret   = "HLBWzHcemHzYJnw5ZLvKpEhQ5KaWwK6Nsj6cxBjUf51NJ",
  set_renv        = FALSE
)


# Political Tweets --------------------------------------------------------
n <- 100

# twitteR::userTimeline("billshortemp", n = n)
abtw <- rtweet::get_timeline("AdamBandt",       n = n, token = tk) ## Grn
altw <- rtweet::get_timeline("AlboMP",          n = n, token = tk) ## Lbr
smtw <- rtweet::get_timeline("ScottMorrisonMP", n = n, token = tk) ## Lib

# Create a Corpus ==============================================================

tweets <- c(altw$text, smtw$text, abtw$text)    # Get all the text
tweet_source <- tm::VectorSource(tweets)        # Create the
tweet_corpus <- tm::VCorpus(x = tweet_source)
tweet_corpus[[1]]$content
strwrap(tweet_corpus[[1]])

# Extract a Vector of Tweets ===================================================
## [[./05_Visualisation_PCA_MDS.R]]
## Clean the Corpus
sw <- c(stopwords(), "one")
clean_corp <- function(corpus, sw = stopowds()) {
  corpus <- tm_map(corpus, FUN = removeNumbers)
  corpus <- tm_map(corpus, FUN = removePunctuation)
  corpus <- tm_map(corpus, FUN = stripWhitespace)
  corpus <- tm_map(corpus, FUN = removeWords, sw)
  ## stopwords() returns characters and is fead as second argument
  corpus <- tm_map(corpus, FUN = stemDocument)
}
tweet_corpus <- clean_corp(tweet_corpus, sw)

wordcloud(tweet_corpus)

# Create a TDM #################################################################
tweet_tdm  = TermDocumentMatrix(tweet_corpus)
tweet_wtdm = weightTfIdf(tweet.tdm)
tweet_matrix <- as.matrix(tweet_wtdm)
empties = which(rowSums(abs(tweet_matrix)) == 0)
tweet_matrix = tweet.matrix[-empties,]
## remove empty tweets
colnames(tweet_matrix)[1:3, 1:3]
