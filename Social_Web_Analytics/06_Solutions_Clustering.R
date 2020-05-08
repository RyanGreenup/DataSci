setwd("~/Dropbox/Studies/2020Autumn/Social_Web_Analytics/06_Solutions_Clustering.R")
d.t.m = matrix(c(1,2,4,8,0,3,5,7,8,3,5,7), byrow=TRUE,ncol = 4)
colnames(d.t.m) =c("itsy", "bitsy", "spider", "wall")
rownames(d.t.m) = c("doc1", "doc2", "doc3")
d.t.m
n.d.t.m = d.t.m %*% diag(1/sqrt(colSums(d.t.m^2)))
n.d.t.m
D.d.t.m= dist(t(n.d.t.m), method = "euclidean")^2/2
D.d.t.m
h.D.d.t.m = hclust(D.d.t.m, method="single") 
plot(h.D.d.t.m, xlab = "Single Linkage Cluster")


##------------1.1--- Preliminaries-------------------------


library("rtweet")
library("tm")
library("SnowballC")

##------------1.2----Prepare Data------------------------
load("../wk6/rTweets.RData")#assuming you are data is in wk6 folder
tweet.text = c(tweets1$text[1:500],tweets2$text[1:500], tweets3$text[1:500])
load("./resources/Download_1.Rdata")


tweet.corpus = Corpus(VectorSource(tweets.company$text))
tweet.corpus = Corpus(VectorSource(tweet.text))
tweet.corpus = tm_map(tweet.corpus, function(x) iconv(x, to='ASCII'))

#ignore url
tweet.corpus = tm_map(tweet.corpus,content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+","",x)))
#remove username
tweet.corpus = tm_map(tweet.corpus,content_transformer(function(x) gsub("@\\w+","",x)))
tweet.corpus = tm_map(tweet.corpus, removeNumbers)
tweet.corpus = tm_map(tweet.corpus, removePunctuation)
stopwordlist  = c(stopwords('english'), "thank", "also", "there", "see", "put")
tweet.corpus = tm_map(tweet.corpus, removeWords, stopwordlist)
tweet.corpus = tm_map(tweet.corpus, stemDocument)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus = tm_map(tweet.corpus,tolower)

# tweet.corpus = tm_map(tweet.corpus, removeNumbers)
# tweet.corpus = tm_map(tweet.corpus, removePunctuation)
# tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
# tweet.corpus = tm_map(tweet.corpus, tolower)
# tweet.corpus = tm_map(tweet.corpus, removeWords, stopwords('english'))
# tweet.corpus = tm_map(tweet.corpus, stemDocument)

# tweets.corpus = Corpus(VectorSource(tweets$text))
# #change to ASCII chars
# tweets.corpus = tm_map(tweets.corpus,function(x) iconv(x,to='ASCII'))






tweet.dtm = DocumentTermMatrix(tweet.corpus)
tweet.wdtm = weightTfIdf(tweet.dtm)
tweet.matrix = as.matrix(tweet.wdtm)
#observe the dim of tweet.matrix 
dim(tweet.matrix)
tweet.matrix[1,1:10] #observe a column in tweet.matrix represents a term in tweets
                     #so each row resprsents a tweet
## remove empty tweets
empties = which(rowSums(abs(tweet.matrix)) == 0)
length(empties)



if(length(empties)!=0){
  tweet.matrix = tweet.matrix[-empties,]
}
dim(tweet.matrix)
##------------------ 2.1 Clustering Iris------------
print(iris)
#We know this data is a sample of three types of irises, 
#so we will use k-means to obtain three clusters. The last
#column of the data is the type of iris, so we will remove 
#it so that k-means does not know it. 
#The function kmeans take the data and the number of clusters 
#as its arguments.
K = kmeans(iris[,-5],3)
names(K)
K$cluster
length(K$cluster)#Should match rows in iris
#To visualise the clustering, we will use multidimensional scaling to 
#project the data into a 2d space.
iris.2d = cmdscale(dist(iris[,-5]))
#We plot the data, using the computed clusters to colour the data,
#and set the plot symbols using the real iris types.
plot(iris.2d, col = K$cluster, pch = as.numeric(iris[,5]))
legend("bottomright", levels(iris[,5]), pch = c(1,2,3))
#How did k-means perform? Did it the iris types cluster well?


##--------------------2.2 Eucledian K-means--------------
#examine the capability of k-means clustering to produce meaningful
#clusters for our document term matrix.
#R provides the function kmeans to perform k-means clustering. 


#Write a for loop that performs k-means for 1 to 15 clusters and 
#stores the SSW value for each clustering in the variable SSW.
#K-means clustering :See slide 12 and 13, week 6 lecture
n = 15
SSW = rep(0, n)
for (a in 1:n) {
  #nstart option attempts multiple initial configurations and 
  #reports on the best one. For example,nstart=10 will generate 
  #10 initial random centroids and chooses the best one for 
  #the algorithm.
  set.seed(40)#seed for random number generator to ensure consistency in our results
  K = kmeans(tweet.matrix, a, nstart = 10) #
  SSW[a] = K$tot.withinss #total within cluster sum of squares
}

###----------------------2.3 Number of Clusters-----------
## plot the results, for  n clusters
plot(1:n, SSW, xlab="clusters", type = "b") #point and line plot
#Observe that the SSW decreases as the number of clusters increase
#The SSW appears to increase and then decrease after the elbow 
#at about 8 clusters. A likely cause of the poor clustering is 
#that k-means only uses Euclidean distance, which is not appropriate 
#for document vectors. We need to use k-means with the cosine 
#similarity function.So we must convert the Euclidenan distances 
#to Cosine distance. To do this, we use Multi-dimensional scaling (MDS).
#with K-means.

##-----------2.4 K-means with cosine distance----------

#Use the elbow method to predict the ideal number of clusters
#Determine the number of clusters when using Cosine distance 
#with tweet vectors.

## first normalise all tweets to unit length (divide by their norm)
#Since each row in tweet.matrix represents a tweet, rowSums provides 
#the sums of the squares of all term in the tweet

norm.tweet.matrix = diag(1/sqrt(rowSums(tweet.matrix^2))) %*% tweet.matrix
## then create the distance matrix
D =dist(norm.tweet.matrix, method = "euclidean")^2/2
#To visualise the clustering, we will use multidimensional 
#scaling to project the data into a 2d space
## perform MDS using 100 dimensions
mds.tweet.matrix <- cmdscale(D, k=100)
n = 5 #we assume elbow bends at 5 clusters  
SSW = rep(0, n)
for (a in 1:n) {
  ## use nstart to reduce the effect of the random initialisation
  set.seed(40)#seed for random number generator to ensure consistency in our results
  K = kmeans(mds.tweet.matrix, a, nstart = 20)
  SSW[a] = K$tot.withinss
}


## plot the results
plot(1:n, SSW, type = "b")
#We can see an elbow at three clusters? or is it 4?, 
#then a further drop indicating that there may
#be clusters in clusters (a hierarchy of clusters).
#So we will compute kmeans using three clusters.
##--------------2.5 Examining the cluster contents.-----------
clusters=5
set.seed(40)#seed for random number generator to ensure consistency in our results
K = kmeans(mds.tweet.matrix, clusters, nstart = 20)
#visualise the clusters by projecting the data into a 
#2D space and colouring the clusters.
mds2.tweet.matrix <- cmdscale(D, k=2)
plot(mds2.tweet.matrix, col = K$cluster)
#Observe that most of the tweets are still allocated 
#to one cluster



#Examine the words associated with each cluster.
cluster.number = 3
## find position of tweets in cluster
clusterTweetsId = which(K$cluster == cluster.number)
## extract tweets vectors for cluster
clusterTweets = tweet.matrix[clusterTweetsId,]
## combine the tweets into a mean tweet
clusterTermWeight = colMeans(clusterTweets)
## show the top 10 weighted words
sort(clusterTermWeight, decreasing = TRUE)[1:10]


cluster.number = 1
## find position of tweets in cluster
clusterTweetsId = which(K$cluster == cluster.number)
## extract tweets vectors for cluster
clusterTweets = tweet.matrix[clusterTweetsId,]
## combine the tweets into a mean tweet
clusterTermWeight = colMeans(clusterTweets)
## show the top 10 weighted words
sort(clusterTermWeight, decreasing = TRUE)[1:10]


cluster.number = 2
## find position of tweets in cluster
clusterTweetsId = which(K$cluster == cluster.number)
## extract tweets vectors for cluster
clusterTweets = tweet.matrix[clusterTweetsId,]
## combine the tweets into a mean tweet
clusterTermWeight = colMeans(clusterTweets)
## show the top 10 weighted words
sort(clusterTermWeight, decreasing = TRUE)[1:10]
##--------------------------------3 Hierarchical Clustering----------------
#Hierarchica clustering allows us 
#to visualise how clusters are formed in our data. 
#But, with a large number of data points, the hierarchy becomes 
#crowded and difficult to visualise. The default visualization method is 
#complete linkage clustering. 

#Perform clustering using single linkage clustering instead.
#Which of the two method provides more reasonable clusters?

#First find only the terms that appear in at least 50 tweets to reduce the dimensions
#for visualization
frequent.words = which(colSums(tweet.matrix > 0) > 50)
length(frequent.words)
term.matrix = tweet.matrix[,frequent.words]
term.matrix
dim(term.matrix)
#We want to compute the cosine distance between each of the terms (not documents),
#so we must make sure that all term vectors (columns of matrix)
#have a norm of 1.
#Remember each column in term.matrix is a term and we want the column vectors
#to have unit length
norm.term.matrix = term.matrix %*% diag(1/sqrt(colSums(term.matrix^2)))
norm.term.matrix
## preserve column names (terms associated with each column)
colnames(norm.term.matrix) = colnames(term.matrix)
colnames(norm.term.matrix)
#We then compute the Cosine distance between the columns of 
#the matrix 
#(IMPORTANT: Transposing the matrix is done since we want distances between terms
#for single linkage clustering and complete linkage clustering
t(norm.term.matrix)
D = dist(t(norm.term.matrix), method = "euclidean")^2/2

#Warning: Do not do this
#D = dist(norm.term.matrix, method = "euclidean")^2/2

#Then perform the hierarchical clustering using single linkage clustering.
h = hclust(D, method="single") 
plot(h)



#then complete linkage clustering.
## hierarchical clustering 
h = hclust(D, method="complete") 
plot(h)
#Single linkage shows a greater similarity between Labour 
# and Liberal compared to the Greens. 
#Complete linkage clustering shows that all 
#three parties are separate.





