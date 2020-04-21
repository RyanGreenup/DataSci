#Intro to some library functions

#Splitting text
#try strsplit() to split the text. 
s = "A Aaa rating"
strsplit(s, "[A-Z]")#""          " "         "aa rating"
strsplit(s, "[a-z]")#"A A" ""    " "   ""    ""    ""    ""    ""
strsplit(s, "[a-zA-Z]")#""  " " ""  ""  " " ""  ""  ""  ""  ""
strsplit("AB123", "[A-Z]")

s= "Western Sydney Stadium is now 70% complete. The next generation of sporting infrastructure is just around the corner! @GeoffLeeMP https://t.co/xpXeXAdEvz"
#Rows are tweets, columns are variables, first column is the text, 
#we are interested in text part only. 
strsplit(s,"[.]") #splits on ".""
## [1] "Western Sydney Stadium is now 70% complete" 
## [2] " The next generation of sporting infrastructure is just around the corner! @GeoffLeeMP https://t" 
## [3] "co/xpXeXAdEvz" 


strsplit(s,"[ ]") #splits on blank, always one blank
##[1] "Western"                 "Sydney"                  "Stadium"                 "is"                      "now"                     "70%"                    
##[7] "complete."               "The"                     "next"                    "generation"              "of"                      "sporting"               
##[13] "infrastructure"          "is"                      "just"                    "around"                  "the"                     "corner!"                
##[19] "@GeoffLeeMP"             "https://t.co/xpXeXAdEvz"
strsplit(s,"a") #splits on a
##[1] "Western Sydney St"                                     "dium is now 70% complete. The next gener"             
##[3] "tion of sporting infr"                                 "structure is just "                                   
##[5] "round the corner! @GeoffLeeMP https://t.co/xpXeXAdEvz"
strsplit(s,"[0-9]") #digits
##[1] "Western Sydney Stadium is now "                                                                                           
##[2] ""         
##[3] "% complete. The next generation of sporting infrastructure is just around the corner! @GeoffLeeMP https://t.co/xpXeXAdEvz"
strsplit(s,"[^A-Za-z]") # non alphabetic characters
strsplit(s,"[^A-Za-z]+") #removes empty strings
strsplit(s,"[A-Za-z]") #alphabetic characters. Matches any single character in the range from A to Z followed by any in the range a to z


#unlisting a list
test1 <- list(5, "b", 12)
unlist(test1) # character vector

test2 <- list(v1=5, v2=list(2983, 1890), v3=c(3, 119))
unlist(test2) #numeric vector with values (v1=5 v21=2983 v22=1890    v31=3  v32=119) 



#Explaining cross mutliplication
tf <- matrix(c(1,2,3,4), ncol=2)
#[1 3]
#[2 4]
bb <- matrix(c(10,20,30,40), ncol=2)
#[10 30]
#[20 40]
tf%*%bb
#[70  150]
#[100 220]
#method is shown below
#[1*10+3*20=70   1*30+3*40=150]
#[2*10+4*20=100  2*30+4*40=220]


#TF-IDF requires multiplication of each
#row in the term freq matrix by the idf vector
#Illustration of such a multication:
#multiply each row in matrix aa by a vector c(10, 20)

idf=c(10, 20)
idfMatrix <- diag(idf)
idfMatrix
#[1 3] * [10 0]
#[2 4]   [0 20]
tf%*%idfMatrix





############################################
#2.4 Accessing Twitter using rtweet package#
############################################

#Setting up libraries:

library("rtweet")
library("tm")

#rtweet package  rtweet is a relatively recent addition to the R package universe that 
#allows you to access both the REST and streaming APIs.

#Record your keys along with the application name to 
#create token as an environment variable
app="Social Web Analytics 300958" #change this value  appropriately
key= "1hvee7WA4badfzv63wTtw559F"  #change this value  appropriately
secret= "j5LfB67hdFlfDd8H6jWQ3Adwb0T4iyyWdmZevgRyO4B67Egb3P"#change this value  appropriately
access_token="1027052361456410624-nkS869ts91EnNLpA7VGKJaxY1tvP9p"#change this value  appropriately
access_secret="ruDrusozeXker7Gl3BEbaJzd1PgsdoKDmLZ4W4fHcZmtp"#change this value  appropriately

#authenticate 
twitter_token=create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)

#search for some tweets
tweets.rtweet=search_tweets('western sydney', n = 1000, type = "recent", token = twitter_token, 
                            include_rts = FALSE)
class(tweets.rtweet)


#Examine the column headings
names(tweets.rtweet)

#There are many columns that we can explore. 
#At this moment, we are interested in the text
tweets.rtweet$text[16] #lets see the sixteenth tweet
class(tweets.rtweet$text) #check the class 






#now that we know how to programmatically download tweets, 
#we will use the .csv files provided to us, instead, so that each of us 
#has the same data in the lab
tweets.df=read.csv("tweets.rtweet.csv", as.is=TRUE) 
names(tweets.df) #observe that "text" is one of the names
tweets.df$text[16] #lets see the sixteenth tweet
#"Western Sydney Stadium is now 70% complete. The next generation of sporting infrastructure is just around the corner! @GeoffLeeMP https://t.co/xpXeXAdEvz"
nchar(tweets.df$text[16]) #number of characters







#To examine the word frequencies, we must build a frequency table. 
# To do this, we must extract the words from the strings. We can 
# extract sequences of letters by splitting the strings on all 
# non-letter characters.
  
tweets.df$text[16]



tweet.words=strsplit(tweets.df$text,"https.*?1-\\d+,\\d+")
tweet.words=strsplit(as.character(tweet.words), "[ ]")
class(tweet.words) 
length(tweet.words)
tweet.words

#The variable tweet.words is a list, where each list item is a 
#vector of the words from a tweet. We want to combine all words
#to count them, so we remove the list and tabulate the resulting vector

word.table = table(unlist(tweet.words)) #table of counts of each word 
length(word.table)

word.table[1] 
word.table[2]
word.table[3]
max(word.table[1:10]) #find max frequency in the first 10 elements


#check the location 
which(word.table==6) #find the index of term whose freq is 6
word.table[169] #verify


#or more sophisticated! 
max(word.table[1:10])
which(word.table==max(word.table[1:10])) #Which word has the max repeats among the first 10 word

#To identify the top 20 occurring words, we must sort the table and examine 
#the top 20 items.
# top 20 occuring words 
s=sort(word.table, decreasing = TRUE)[1:20]
s






###############################################
#3          Text mining
###############################################
#Corpus is a collection of documents in the R environment. 
#Text mining package only works with corpus objects

tweet.corpus = Corpus(VectorSource(tweets.df$text)) 

#convert charts to UTF8 format: for Windows users only
tweet.corpus = tm_map(tweet.corpus, function(x) iconv(x, to='UTF8', sub='byte')) 
#mac users follow this instead
#tweet.corpus = tm_map(tweet.corpus, 
#                      function(x) iconv(x, to='UTF-8-MAC', sub='byte')) 

tweet.corpus$content[5:10]

# use tm_map to apply the given function to each document in the corpus. 
#Keep this in mind for the next section.
corpus = tm_map(tweet.corpus, function(x) iconv(x, to='ASCII', sub=' ')) # convert special characters 
corpus$content[5:10]  #see what happenned

############################################################
#3.1 Preprocessing the document set
# Now that we have our corpus, we want to
# 1. remove numbers 
# 2. remove punctuation 
# 3. remove whitespace 
# 4. case fold all characters to lower case 
# 5. remove a set of stop words 
# 6. reduce each word to its stem
##############################################################


corpus$content[5]
corpus = tm_map(corpus, removeNumbers) # remove numbers corpus$content[5] #see what happenned
corpus$content[5]  #see what happenned

corpus = tm_map(corpus, removePunctuation) # remove punctuation 
corpus$content[5] #see what happenned

corpus = tm_map(corpus, stripWhitespace) # remove whitespace 
corpus$content[5] #see what happenned

corpus = tm_map(corpus, tolower) # convert all to lowercase 
corpus$content[5] #see what happenned


corpus = tm_map(corpus, removeWords, c(stopwords(),"western","sydney")) # remove stopwords 
corpus$content[5] #see what happenned

library(SnowballC)
corpus = tm_map(corpus, stemDocument) # convert all words to their stems 
corpus$content[5] #see what happenned

#if you have to only use Plain text document then add the following code 
#corpus <- Corpus(VectorSource(corpus$content))
#corpus$content[5]



#create the DocumentTermMatrix object (This creates a list) 

tweet.dtm = DocumentTermMatrix(corpus) #create your document matrix 
tweet.matrix = as.matrix(tweet.dtm)    #convert to a matrix. This is the fd,t term in the week4 lecture slide  21
dim(tweet.matrix) #Number of rows is the number of Tweets
tweet.matrix[1,1:5]
tweets.df$text[1]
######################################################################
#3.2 Weighting the Document Matrix
####################################################################
#TF-IDF Matrix 
#TFIDF = ln(fd,t +1)*ln(N/ft) #N is the number of documents, ft is the number of documents containing term t
N=nrow(tweet.matrix) #number of documents 
N
ft=colSums(tweet.matrix>0) #in how many documents term t appeared in, 

ft
IDF=log(N/ft) 
IDF
length(IDF)
## [1] 776 

TF=log(tweet.matrix+1)


#Compute the weighted document term matrix tweet.weighted.matrix 
#containing the weighted values w of term t in document d =>(w d, t)

#weighted matrix requires crosss multiplication of TF and IDF. 





#to find weigthed matrix, you have to multiply term by term; 
weighted.matrix=TF%*%diag(IDF) 
#Note: This not matrix multiplication directly with IDF
#So convert IDF to a diagonal matrix, then do a matrix multiplication. 

dim(weighted.matrix)# 100*776 where each colum represents a term

w=colSums(weighted.matrix) #each colum shows a term
w
length(w)


#Locate the position of the top 20 words, according to the overall word weight 
o.w=order(w,decreasing = TRUE)[1:20] 
o.w #gives the positions #257 103 110 308 117 673 674 113 163  52 208  99 136 114 347  19 206 128  50  96
w[257]
w[103]
w[96]

#order function orders the data and gives the positions of the values 
#sort fuction sorts the data in the matrix and gives values, not the positions. 
s.w=sort(w,decreasing = TRUE)[1:20] 
s.w #gives the values (this is not we are looking for)

most.relevant.words=colnames(tweet.matrix)[o.w] 
most.relevant.words 





#Wordcloud
#Wordcloud works with corpus object 
library("wordcloud")
## Loading required package: RColorBrewer 
wordcloud(corpus)




