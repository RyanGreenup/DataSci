## [[/home/ryan/Dropbox/Notes/DataSci/Visual_Analytics/Assessment/the-marvel-universe-social-network/adjmat.r]]
## Index value by socialability
# devtools::install_github("ropensci/plotly")
library(mise)
mise()
library(tidyverse)
library(plotly)
library(igraph)
c <- 3
## How much of the data to use #1 for all, 0.5 for half, 0 for none
n <- 1  # 0.01


setwd("/home/ryan/Dropbox/Notes/DataSci/Visual_Analytics/Assessment/the-marvel-universe-social-network/")
library(plotly)
library(reshape2)
start <- Sys.time()

load(file = "./hero_adjacency.Rdata")
data <- hero_adj

# Choose the root value
## Because this will descend by number of cross overs, I will choose the individual with the most crossovers (including doubles)
socialability <- apply(data, 2, sum)
(socialability <- socialability[order(-socialability)]) %>% head()
top <- names(socialability)[which(socialability == max(socialability))]; i <- 1
top <- names(socialability)[c]

## Subset the Data
data <- data[order(-socialability),order(-socialability)]
data <- data[1:(n*nrow(data)), 1:(n*ncol(data))]


sd <- data
# Build a Data Frame and set up the root value
bfdf   <- data.frame(parents = "", labels = top, values = max(sd)+1) # reset to 1 later, it's easier than reordering


##### Loop

while (i <= nrow(bfdf)) {
  ## Identify matches
  matches <- sd[,top] # Extract column of interest
  matches <- matches[matches>0] #Only consider characters that have met
  matches <- matches[!(names(matches) %in% bfdf$labels)] #Remove Characters already used
  
  (matches <- matches[order(-matches)]) %>% head() # Arrange the matches
  
  ## Apend the value to the data frame
  bfrow  <- data.frame(parents = rep(top, length(matches)), labels = names(matches), values = matches)
  (bfdf <- rbind(bfdf, bfrow)) %>% head()
  
  ## Remove Duplicates in df (failsafe)
  # n <- which(duplicated(bfdf$labels))
  # bfdf <- bfdf[-n,]
  
  ## Set the next top value
  i <- i+1
  top <- bfdf$labels[i]
  while (is.na(top) && i <= nrow(bfdf)) {
    i <- i+1
    top <- bfdf$labels[i]
  }
  print(i/nrow(bfdf))
  
  ## Inspect Parents
  # unique(bfdf$parents) %>% print()
}

bfdf$values[1] <- 1
## Test plotting
(fig <- plot_ly( labels  = bfdf$labels, parents = bfdf$parents, values  = bfdf$values, type = 'sunburst' ))

end <- Sys.time()
print(end-start)

# It's very interesting that most plots show less than 3 degrees of seperation,
# kind of a bummer for the ring chart, but, interesting all the same.


(fig <- plot_ly( labels  = bfdf$labels, parents = bfdf$parents, values  = bfdf$values, type = "treemap" ))


# This could also be viewed as a 2d heirarchical graph in plotly


# Family Friend Tree ------------------------------------------------------
mise()
load(file = "./hero_adjacency.Rdata")
data <- hero_adj
(most_social <- apply(data, 2, mean))
most_social <- most_social[order(-most_social)]


treedf <- data.frame()
n <- 2 # number of children # I really liked 2
l <- 8 # number of levels   # I liked 3 (actually n=3, l = 5 was good too)
en <- sum(n^(1:l)) #number of edges for corresponding level number

(top <- names(most_social)[1])
matches <- data[top,]
data <- data[-which(rownames(data)==top), -which(rownames(data)==top)]
matches <- matches[matches>0]
matches <- matches[order(-matches)]
matchdf <- data.frame(parent = rep(top,n ), labels = names(matches)[1:n], values = matches[1:n])
treedf <- rbind(treedf,matchdf)

for (i in 1:en) {
  #  i <- 1
  top <- treedf$labels[i]
  matches <- data[top,]    # This for which filter sometimes returns null and I don't know why
  #  data <- data[-which(rownames(data)==top), -which(rownames(data)==top)]
  matches <- matches[matches>0]
  matches <- matches[matches != top] # This is a simpler fix because R is slow
  matches <- matches[order(-matches)]
  if (length(matches)<n) {
    matchdf <- data.frame(parent = rep(top,length(matches)), labels = names(matches)[1:length(matches)], values = matches[1:length(matches)])
  } else if(length(matches)==0) {
    print("No matches for this iterance")
  } else {
    matchdf <- data.frame(parent = rep(top,n ), labels = names(matches)[1:n], values = matches[1:n])
  }
  treedf <- rbind(treedf,matchdf)
  print(i/en)
  #  i <- i+1
}
top <- treedf$labels[2]
matches <- data[top,]
matches <- matches[matches>0]
matches <- matches[order(-matches)]
matchdf <- data.frame(parent = rep(top,n ), labels = names(matches)[1:n], values = matches[1:n])
treedf <- rbind(treedf,matchdf)






Edges <- treedf
names(Edges) <- c("Source", "Target", "Value")


#{

# GGPlot (# Because of the way that I made the graph, the nodes come from the edges, hence, every node must be connected.)
G <- igraph::graph_from_data_frame(d = Edges, directed = FALSE)
laytg <- igraph::layout_as_tree(G)
colnames(laytg) <- c("xval", "yval")
laytg <- as_tibble(laytg)
laytg$node <- vertex_attr(G)[[1]]
laytg


ne <- nrow(Edges)
ys <- xe <- ye <- xs <- vector(length = ne)
for (i in seq_len(length(xs))) {
  xs[i] <- laytg$xval[laytg$node==Edges$Source[i]]
  ys[i] <- laytg$yval[laytg$node==Edges$Source[i]]
  xe[i] <- laytg$xval[laytg$node==Edges$Target[i]]
  ye[i] <- laytg$yval[laytg$node==Edges$Target[i]]
}
ne
nrow(Edges)

starts    <- data.frame("xval" = xs, "yval" = ys, edgenum = 1:ne) # TODO Make a factor
ends      <- data.frame("xval" = xe, "yval" = ye, edgenum = 1:ne)
Edges_val <- as_tibble(rbind(starts,ends))
Edges_val

p <-  ggplot(rbind(starts,ends), aes(x = xval, y = yval)) +
  geom_line(aes(group = edgenum), lty = 3, col = "royalblue", size = 0.5) +
  geom_point(data = laytg, aes(x = xval, y = yval, col = node), size = 4) +
  labs(x = "", y = "") +
  geom_label(data = laytg, aes(x = xval, y = yval, label = node), size = 1.5, nudge_x = 0.1, nudge_y = 0.2, ) +
  guides(col = FALSE) +
  theme_classic() +
  theme(axis.line = element_blank(),  # https://stackoverflow.com/a/6542792/12843551
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.text.x=element_blank())
 p
# ggplotly(p)

#}

# So the nodes are just laid out in such a way so as to create islands, 
# It is possible to lay them out so they all connect and it is possible to lay them out so they can create islands, 
# for example the fantastic 4

 # Because this is non-heirarchical popular characters are pushed into the centre.t

