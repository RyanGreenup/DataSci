#How to remove empty columns in a matrix
ex = matrix(c(1,2,3, 0,0,0, 4,5,6, 0,0,0), ncol=4)
ex
emptyCols = which(colSums(ex)==0)
emptyCols
length(emptyCols)
ex[,-emptyCols] 



ex = matrix(c(1,2,3, 10,20,30, 4,5,6, 40,50,60), ncol=4)
ex
emptyCols = which(colSums(ex)==0)
emptyCols
length(emptyCols)
ex[,-emptyCols] #caution: wrong result



ex = matrix(c(1,2,3, 10,20,30, 4,5,6, 40,50,60), ncol=4)
ex
emptyCols = which(colSums(ex)==0)
emptyCols
length(emptyCols)
if(length(emptyCols!=0)){
  ex[,-emptyCols]
}
ex
#end of illustration


#How to remove NAs in a vector
v= c(2,3, 10, NA, 4)
v=v[!is.na(v)]
v

#Cosine similarity is the dot product of unit vectors
#How to find unit vector of a vector

v = c(1, 2)
v
modv = sqrt(v[1]^2 +v[2]^2)
modv
#using sum function makes it look neater
modv2 = sqrt(sum(v^2))
modv2

unitV = v/modv
unitV
#verify
uvalue = sqrt(sum(unitV^2))
uvalue #should be 1

#How to find unit vectors in a matrix of vectors
m = matrix(c(1, 2, 3,4),ncol=2)
#[1 3]
#[2 4]
m
mUnit = m %*% diag(1/sqrt(colSums(m^2)))  
mUnit
#verify
mUnit = sqrt(colSums(mUnit^2))
mUnit # should be 1 each


#--------------Activity 1 Political tweets--------------------------
#[1] "Using direct authentication"
library("rtweet")


#app="Social Web Analytics 300958" #change this value  appropriately
#key = "your twitter API key"
#secret = "your twitter API secret"
#access_token = "your twitter API access token"
#access_secret = "your twitter API access token secret"


twitter_token=create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)

#Uncomment the following if you are succesful connecting to Twitter
tweets1 <- get_timeline("billshortenmp", n = 1000)## get most recent 1000 tweets posted by Bill Shorten's account 
#tweets2 <- get_timeline("ScottMorrisonMP", n = 1000)
#tweets3 <- get_timeline("RichardDiNatale", n = 1000)
#save(tweets1,tweets2, tweets3, file="rTweets.RData")
   


#Only run the following if you have no Twitter downloads
load("./rTweets.RData")
#At this stage: tweets1, tweets2 and tweets3 will be initialized
class(tweets1)#verify 
class(tweets2)#verify 
class(tweets3)#verify 

#you may put them all to gether using rbind
tweets.df = rbind(tweets1,tweets2,tweets3)
dim(tweets.df )

#Only run the following code if you are successful in connecting to Twitter
#Another way to download all tweets data.frame is shown below.
#or you may get a single data.frame with all the 3 users' tweets, directly using rtweet 
tweets.df2 = get_timelines(c("billshortenmp","ScottMorrisonMP", "RichardDiNatale"), n=1000)
dim(tweets.df2 )

tweets.df$text
#----------------------Activity 1.1 Build a term-document matrix.-------------
#See week 4 lecture notes for concepts on this
library(tm)
#tweets.df = twListToDF(tweets) # convert tweets to dataframe
length(tweets.df$text)
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







# remove empty tweets
# if a tweet contains all stop words, then after preprocessing, it will be empty
# We remove these from the tweet set so they don't effect the calculations (an
# empty tweet is represented as a vector with all zeros, the Cosine with this
# vector does not make sense.)
tdm
dim(tdm)
empties = which(colSums(as.matrix(tdm)) == 0)
empties
length(empties)

if(length(empties)!=0){
  tdm = tdm[,-empties]
}

# Convert to a standard R matrix
M = as.matrix(tdm)


#------------------------Activity 2 Draw a Wordle-esque word cloud------------------
# term frequencies correspond to row Sums in  tdm
library(wordcloud)
freqs = rowSums(M)
freqs
## remove any words that have count "NA".
freqs = freqs[!is.na(freqs)]
wordcloud(names(freqs), freqs, random.order=FALSE, min.freq=3)



# Word frequencies correspond to row Sums in this tdm.
#Word cloud based on TF-IDF.
tdmw = weightTfIdf(tdm)
T = as.matrix(tdmw)
freqsw = rowSums(T) 
#Each row element in T represents term weights corresponding 
#so rowSum represents the total term weights of each term
wordcloud(names(freqsw), freqsw, random.order=FALSE, min.freq=3)
#interpretation: Using term frequencies seems better than TF-IDF weights. 
#Words that appear in only one document (e.g. http://...) get a large IDF weight,
#which is not what we want for these word clouds.


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
pcaT <- prcomp(t(T)) #transpose of T because prcomp expects rows to be documents (tweets), rather than terms
class(pcaT)
names(pcaT)
## plotting 1st and 2nd PC
plot(pcaT$x[,1], pcaT$x[,2], col=colours, pch=16)#pch: see help

## plotting 1st and 3rd PC
plot(pcaT$x[,1], pcaT$x[,3], col=colours, pch=16)

#Using the square root transformation, see slide 18, week5 lecture notes.
#Check if square root transformations are better than tf-idf transformation
pcaM <- prcomp(t(sqrt(M)))
## plotting 1st and 2nd PC
plot(pcaM$x[,1], pcaM$x[,2], col=colours, pch=16)

#Examine the summaries
summary(pcaT)$importance[,1:5]
summary(pcaM)$importance[,1:5]
#We can see that even though the plot of PCA using TF-IDF looks terrible, 
#it explains more of the variance of the original data compared to when 
#using the square root transformation.


#------------------Activity 4 Multidimensional Scaling------------
#Verifying that MDS using Euclidean distance is the same as PCA. 
#We find that the results are the same when using PCA, 
#except for a rotation.

D = dist(t(T)) #dist expects each row to be a document, not term
mdsT <- cmdscale(D, k=2) 
plot(mdsT[,1], mdsT[,2], col=colours, pch=16)

#MDS of unweighted tweets, 
#using Binary distance. The distance is the number of elements that 
#are non-zero in only one of the vectors divided by the numberof 
#elements that are non-zero in at least one vector. See week5 lecture
#slide 30 for an example

D = dist(t(M), method = "binary")
mdsM <- cmdscale(D, k=2)
plot(mdsM[,1], mdsM[,2], col=colours, pch=16)






#MDS of unweighted tweets, using Cosine distance.
#Cosine distance measures how far are the tweets from each other 
#as opposed to full similarity (cos 0)
#See slide 32, week 5 lecture
CM = M %*% diag(1/sqrt(colSums(M^2)))   # convert M to matrix of unit vectors  
D = dist(t(CM), method = "euclidean")^2/2 #D = 1 - cos(tweeti, tweetj) =(d(tweeti,tweetj)^2)/2
mdsM <- cmdscale(D, k=2)
plot(mdsM[,1], mdsM[,2], col=colours, pch=16)

#MDS of TF-IDF tweets, using Binary distance.
D = dist(t(T), method = "binary")
mdsT <- cmdscale(D, k=2)
plot(mdsT[,1], mdsT[,2], col=colours, pch=16)

#MDS of TF-IDF tweets, using Cosine distance.
CT = T %*% diag(1/sqrt(colSums(T^2)))
D = dist(t(CT), method = "euclidean")^2/2
mdsT <- cmdscale(D, k=2)
plot(mdsT[,1], mdsT[,2], col=colours, pch=16)

#Using TF-IDF weights with Cosine distance seems to have produced 
#clustered results (all of the blue points are close to each other, 
#all of the green points are close to each other and all of the red 
#points are close to each other).
#The previous clusterings (using other metrics) have provided many
#"blobs" of points in each colour, while this clustering has provided 
#a single blob for each colour. We can see that the centre of the plot 
#(near 0,0) is covered by all colours, meaning that there is a set of
#points from all colours that have similar topics. We also see that blue
#and red branch out in their own directions, meaning that there is set of 
#blue and red tweets that have their own topics. Green seems to branch out
#down the plot, but is still close to red points.





