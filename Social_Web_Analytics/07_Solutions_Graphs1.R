## * ---------1 Requirements ------
#install.packages("igraph")
library("igraph")
library("rtweet")
## * kl-------------------------------------------------------------------------

#Later you will download  Twitter user information about Hollyhood actor
#Chris Hemsworth and some of his friends using rtweet.
#If you do not have Twitter access, you may download an equivalent data file:
#chris2019.RData. 

#app="Social Web Analytics 300958" #change this value  appropriately
#key= ""
#secret= ""
#access_token=""
#access_secret=""
#twitter_token=create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)

3+3

## * ----------------2 Creating a graph ------
## * -----------------2.1 Graph Formula--------
#We can create a graph by providing the graph.formula 
#function with the set of vertices, and how they are 
#connected (edges
g1 = graph.formula(A - B, A - C, A - D, B - D)
print(g1)
V(g1) #View vertices
E(g1) #View edges
plot(g1)# visualize graph






## * ---------------2.2 Adjacency Matrix----------
#To create a graph from an adjacency matrix, we must 
#first create the matrix. 
A = matrix(0, 4, 4) # 4 X 4 matrix, containing all zeros
#add the edges by allocating ones. We will make the same graph 
#shown above in g1. 
A
A[1, c(2, 3, 4)] = 1 #We want to connect the first vertex to 
                     #the second, third and fourth vertices
A
A[2, c(1, 4)] = 1    #We also want to connect the second vertex 
                     #to the first and fourth
A
A[3, 1] = 1         #connect the thrid vertex to the first
A[4, c(1, 2)] = 1

print(A)


g2 = graph.adjacency(A) # create the graph from adjacency matrix
plot(g2)
g3 = graph.adjacency(A, mode="undirected") #undirected graph
plot(g3)

## * ----------------2.3 Edge List-----------------
#We can also create a graph using an edge list. An edge list is an 
#M*2 matrix, containing M edges. Each row of the edge list provide
#the start and end vertices.
el = matrix(c("A", "A", "A", "B", "B", "C", "D", "D"), 4, 2)
print(el)
#[,1] [,2]
#[1,] "A"  "B" 
#[2,] "A"  "C" 
#[3,] "A"  "D" 
#[4,] "B"  "D"

g4 = graph.edgelist(el, directed = FALSE)
plot(g4)

## * --------------3 Creating Random Graphs -------------
#erdos.renyi is a random graph
set.seed(20)
g.er = erdos.renyi.game(n = 10, p = 1) #p=probability, n= nodes
plot(g.er, vertex.size = 40) 
g.er = erdos.renyi.game(n = 10, p =0) #p=probability, n= nodes
plot(g.er, vertex.size = 40)

#create a Barabasi-Albert Graph
#Here a new vertex prefers to link to  highly
#connected vertices

#we must provide n (the number of vertices)
set.seed(20)
g.ba = barabasi.game(n = 10, directed = FALSE)
plot(g.ba, vertex.size = 20) 









#We can also provide the k (the power) to change
#the probability of connecting to a vertex
#m (the number of edges to add to each new vertex)
ba = barabasi.game(5, 
                  m = 2,
                  power = 0.6, 
                  out.pref = TRUE,
                  zero.appeal = 0.5,
                  directed = FALSE)

plot(ba, vertex.size = 20) 

ba = barabasi.game(5, 
                   m = 2,
                   power = 1, 
                   out.pref = TRUE,
                   zero.appeal = 0.5,
                   directed = FALSE)

plot(ba, vertex.size = 20) 

## * -------------4 Examining the Graphs-----------
## ** --------------4.1 Density---------
#See slide 24, week7 for definition of density
#Density = no of edges/possible number of edges = 2E/(V(V-1))
#By visually examining the erdos.renyi and  Barab??si???Albert
#which looks denser? Use the function graph.density 
#to compute the density of each graph and compare 
#the results to your guess.
set.seed(20)
g.er = erdos.renyi.game(n = 100,p = 0.1)
plot(g.er, vertex.size = 12) 
set.seed(20)
g.ba = barabasi.game(n = 100, directed=FALSE)
plot(g.ba, vertex.size = 5) 
graph.density(g.er)
graph.density(g.ba)
#Barabasi-Albert has lower density since it has fewer edges
## ** -------------4.2 Diameter---------------
#The diameter is the longest shortest path. 
#Which of the two graphs do you expect to have the
#largest diameter? 

#We expect the ER graph to have a smaller diameter
#because there are many paths between each of the 
#vertices.

#Use the function diameter to compute
#the diameter of each graph.
diameter(g.er) 
diameter(g.ba)


## * ----------------4.3 Degree-------------------
#What do you expect the degree distribution of each 
#graph to look like? 

#The ER degree distribution should look mound shaped 
#(a mean with left and right tails). The BA degree 
#distribution should look exponentially decaying 
#(many vertices with low degree, a few with high degree).


#We can also compute the degree distribution
#Degree distribution, Pdeg(k)=fraction of nodes in the graph with degree k.
#of the graph using the function degree.distribution. 
#Use the help pages to understand the output.
degree(g.er)
degree.distribution(g.er)#a numeric vector of the same length as the maximum degree plus one. 
                         #The first element is the relative frequency zero degree vertices,
                         #the second vertices with degree one, etc.
degree(g.ba)
degree.distribution(g.ba)
#Which vertex is most central according to Degree Centrality?
#To find the most central, we order the vertices by their degree.
g.er.order =order(degree(g.er), decreasing=TRUE)
g.er.order
g.ba.order= order(degree(g.ba), decreasing=TRUE)
sort(degree(g.er), decreasing=TRUE)
## ** --------------4.4 Closeness Centrality---------------
#See slide 34, week 7
#The closeness centrality of a vertex v
#is the sum of the distance from  v
#to all other vertices. 

#Read the R help page for closeness to find what R is computing. 
#The R function closeness provides the reciprocal of the sum of path 
#lengths. 
#Therefore the sum of path lengths is:
1/closeness(g.ba)
#We want the vertex with the shortest path lengths, 
#therefore we want the maximum given by the R closeness function.
order(closeness(g.er), decreasing = TRUE)
order(closeness(g.ba), decreasing = TRUE)

## * -----------------------4.5 Betweenness---------------
#Betweenness centrality measures how often a vertex is used 
#in shortest paths
betweenness(g.er)
order(betweenness(g.er), decreasing = TRUE)
order(betweenness(g.ba), decreasing = TRUE)
#Is the centre the same for all three centrality measures? 
#Examine this for the Erd??s-Renyi graph and Barab??si???Albert graph.
#Compare the above orders.

## * --------------------5 Small Graph------------------------
#Create the following graph
g3 = graph.formula(A - B, A - C, A - D, B - D, B - E, E - D, C - E)
plot(g3)
#Calculate the Degree Distribution, Degree Centrality, 
#Closeness Centrality, Betweenness Centrality 
#using the methods shown in the lecture. 
#Then check your answer using the R functions.
degree.distribution(g3)
degree(g3)
1/closeness(g3)
betweenness(g3)

## * -----------------6 Twitter Graph------------
#Each twitter user has a set of friends and a set of followers:
#Followers of user x: 
#users who are following x

#Friends of user x
#users who x is following.
#Therefore, a user can choose their friends, but cannot choose their followers. 
#There are many users of Twitter, we want to find the interesting ones.
#Interesting users usually have many followers (because they are interesting).
#So when obtaining information about users on Twitter, we should note:
#Popular people have many followers.
#Using rTweet, we can download user information when given a screen name or ID. 

#Exec the following statement to download info from Twitter
user = lookup_users(c("chrishemsworth"))#examine all of the details for Chris Hemsworth

#The above function provides information such as the number of friends,
#the number of followers, if the account is protected and verified 
#and the owner's name and id.
names(user)
user$friends_count
user$screen_name
user$followers_count








#Alternatively, download the tweet file to your working directory
load("./chris2019.RData")
#It comes with user, friends and more.friends variables
#which are needed during the tutorial




# ** -----------------------6.2 Dowloading a user's friends from Twitter---------
#get the friends of user, Chris Hemsworth, directly from Twitter
t <- get_friends("chrishemsworth") #gets user id of friends of chrishemsworth
names(t)

#get Chris Hemsworht's friends, directly from Twitter
friends = lookup_users(t$user_id)


dim(friends)
names(friends)
friends$screen_name[1] #name of friend at index 1
friends$followers_count[1] #examine the follower count of the first friend
friends$screen_name[2]


#Find the 10 friends that have the most followers. What are their names? 
#Note the function, sort, will sort the vector of follower counts. 
#The function, order, will sort, but provide the position of the sort. 
#So to find the top 10, we use order with decreasing=TRUE and choose the 
#first ten values, giving us the positions of the top 10.
friendPosition = order(friends$followers_count, decreasing = TRUE)[1:10] 
friendPosition
#topFriends = friends$user_id[friendPosition] #ids of top 10 friends
topFriends = friends[friendPosition,] #ids of top 10 friends
topFriends

topFriends$user_id[1]
topFriends$screen_name
#Write a for loop to download 100 friends from the 10 most popular friends of 
#Chris Hemsworth and store them in more.friends.
#If you are using chris2019.RData, do not do this because it is already present in 
#the variable, more.friends


## * -----------Do the following to download directly from Twitter------
more.friends = list() #a place to store the friends of friends
#n = length(topFriends) 
n= nrow(topFriends)
t = get_friends(topFriends$user_id[1]) #get friends of each friend 
more.friends[[1]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[2]) #get friends of each friend 
more.friends[[2]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[3]) #get friends of each friend 
more.friends[[3]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[4]) #get friends of each friend 
more.friends[[4]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[5]) #get friends of each friend 
more.friends[[5]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[6]) #get friends of each friend 
more.friends[[6]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[7]) #get friends of each friend 
more.friends[[7]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[8]) #get friends of each friend 
more.friends[[8]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[9]) #get friends of each friend 
more.friends[[9]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[10]) #get friends of each friend 
more.friends[[10]]=lookup_users(t$user_id)
more.friends
class(more.friends[[1]])
dim(more.friends[[1]])
nrow(more.friends[[1]])

##-----------------Restrict to 100 records to manage big data----------------
for(a in 1:10){
  if(nrow(more.friends[[a]])>100){
    more.friends[[a]]=more.friends[[a]][1:100,]
  }
}

more.friends[[1]]$screen_name[1]
more.friends[[1]]$screen_name[2]
more.friends[[2]]$screen_name[2]
#save(user, friends, more.friends, file="chris2019.RData")

## ** ----------------------6.3 Creating the Twitter Graph--------------
#Now we have Chris Hemsworth, Chris Hemsworth's friends, 
#and  friends of the top 10 friends of Chris Hemsworth
#We can create a graph by constructing an edge list (who links to who). 
#We know Chris Hemsworth links to all of his  friends, 
#so the edge list will contain  rows beginning with Chris Hemsworth 
#and ending with the friend of Chris Hemsworth. First we must get the 
#names of all of Chris Hemsworth's friends.

#Write a for loop to store all 100 screen names in the variable friend.names.
#Not required to code this since rtweet gives this by default
friends$screen_name
nrow(friends)
#We can now build the edge list using:
chris = rep(user$screen_name, nrow(friends))  # repeat Chris Hemsworth's user name 100 times 
chris
el = cbind(chris, friends$screen_name)  # bind the columns to create a matrix
el
#Using what you have done above, write the function
user.to.edgelist <- function(user, friends) { 
  # create the list of friend screen names    
  user.name = rep(user$screen_name, nrow(friends))  # repeat user's name    
  el = cbind(user.name, friends$screen_name)  # bind the columns to create a matrix
  return(el) 
}

#We can use the created function user.to.edgelist to create the edge list for Chris Hemsworth:
el.chris = user.to.edgelist(user, friends)
el.chris
topFriends[1,4] #4th column is Screenname
nrow(more.friends[[4]])
topFriends[1,]
user.to.edgelist(topFriends[1,], more.friends[[1]])
#We can also build the edge list for the top 10 friends using a loop:
for (a in c(1:length(more.friends))) {     
  el.friend = user.to.edgelist(topFriends[a,], more.friends[[a]])     
  el.chris = rbind(el.chris, el.friend)  # append the new edge list to the old one. 
}
el.chris
#Now that we have the edge list, we can create the graph:
g = graph.edgelist(el.chris)
g
#Let's plot the graph. Since there are many vertices, 
#we will reduce the vertex size and use a special plot layout:
plot(g, layout = layout.fruchterman.reingold, vertex.size = 5)

#This graph contains many vertices that we did not examine. To remove these, 
#let's only keep the vertices with degree (in or out) greater than 1.
g2=induced_subgraph(g, which(degree(g, mode = "all") > 1))
#This graph is now easier to visualise:
plot(g2, layout = layout.fruchterman.reingold, vertex.size = 5)

#Who is at the centre of the graph? Use the centrality measures to examine this.
g2.centres=order(closeness(g2), decreasing=TRUE)
length(g2.centres)
g2[g2.centres][,1]#names of the centres
# Examine the graph density. Is it sparse or dense?
graph.density(g2)

#Examine the degree distribution. Is this graph more similar to an Erd??s-Renyi
#graph or a Barab??si???Albert graph?
degree.distribution(g2)









