# Load Packages -----------------------------------------------------------

  if(require("pacman")){
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }

    pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
                 parallel, dplyr, plotly, tidyverse, reticulate, UsingR, Rmpfr,
                 swirl, corrplot, gridExtra, mise, latex2exp, tree, rpart, lattice,
                 coin, primes, epitools, maps, clipr, ggmap, twitteR, ROAuth,
                 tm, rtweet, base64enc, httpuv, SnowballC, RColorBrewer, wordcloud, ggwordcloud)



   mise()


# Set up Tokens ===========================================================

     options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

   setup_twitter_oauth(
     consumer_key = "dE7HNKhwHxMqqWdBl1qo9OAkN",
     consumer_secret = "7ByyC6lR9d2aqLoXHHBkOtKRiU10cHhGguroiGKQ69AGDMMI9V",
     access_token = "1240821178014388225-sFWver0NDDY3BhPdPyg8d4mtQxnl0K",
     access_secret = "HLBWzHcemHzYJnw5ZLvKpEhQ5KaWwK6Nsj6cxBjUf51NJ")

# rtweet ==================================================================
tk <-    rtweet::create_token(
     app = "SWA",
     consumer_key    = "dE7HNKhwHxMqqWdBl1qo9OAkN",
     consumer_secret = "7ByyC6lR9d2aqLoXHHBkOtKRiU10cHhGguroiGKQ69AGDMMI9V",
     access_token    = "1240821178014388225-sFWver0NDDY3BhPdPyg8d4mtQxnl0K",
     access_secret   = "HLBWzHcemHzYJnw5ZLvKpEhQ5KaWwK6Nsj6cxBjUf51NJ",
     set_renv        = FALSE)



tweets.df = get_timelines(c("billshortenmp","ScottMorrisonMP", "RichardDiNatale"), n=100, token = tk)
tweets.df = get_timelines(c("AdamBandt","AlboMP", "ScotrrisonMP"), n=100, token = tk)

corpus = Corpus(VectorSource(tweets.df$text)) # create a corpus from tweet text
corpus = tm_map(corpus, function(x) iconv(x, to='ASCII')) # convert characters to ASCII


# create  term document matrix applying some transformations
tdm = TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE,

                #https://mathiasbynens.be/demo/url-regex
                stopwords = c(stopwords(),"https", '!"#$%&\'()*+,-./@:;<=>[\\]^_`{|}~'),
                #stopwords = TRUE,#use default stopwords
                removeNumbers = TRUE,
                tolower = TRUE,
                stemming=FALSE)) # No stemming
dim(tdm)

as.matrix(tdm) #see how the tdm is structured.
#Observe the terms are laid in rows and tweets are in columns.


empties = which(colSums(as.matrix(tdm)) == 0)
empties
length(empties)

if(length(empties)!=0){
  tdm = tdm[,-empties]
}

# Convert to a standard R matrix
M = as.matrix(tdm)

#-----------------------Activity 3 Principal Components Analysis-------------------
length(tweets1$text)
length(tweets2$text)
colours = c(rep("red", length(tweets1)),
            rep("blue", length(tweets2)),
            rep("green", length(tweets3)))
colours = c(rep("red", length(tweets1$text)),
            rep("blue", length(tweets2$text)),
            rep("green", length(tweets3$text)))
length(colours)
## remove colours associated to empty tweets
colours = colours[-empties]
T<-M
pcaT <- prcomp(t(T)) #transpose of T because prcomp expects rows to be documents (tweets), rather than terms
class(pcaT)
names(pcaT)
## plotting 1st and 2nd PC
plot(pcaT$x[,1], pcaT$x[,2], col=1:3, pch=16)#pch: see help
plot(pcaT$x[,1], pcaT$x[,2], col=colours, pch=16)#pch: see help
