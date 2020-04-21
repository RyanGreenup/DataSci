## [[/home/ryan/Notes/Org/Visual_Analytics_Assignment1.org]]
## [[/home/ryan/Dropbox/Notes/DataSci/Visual_Analytics/Assessment/the-marvel-universe-social-network/sunburst.r]]

# Preamble ----------------------------------------------------------------

setwd("/home/ryan/Dropbox/Notes/DataSci/Visual_Analytics/Assessment/the-marvel-universe-social-network/")
data       <- read.csv(file = "hero_small.csv")
index      <- sample(1:nrow(data), size = 1000)
data_small <- data[index,]
library(igraph)
library(tidyverse)
library(reshape2)


# Create Adjacency Matrix -------------------------------------------------
make_adj <- function(mat){
get.adjacency(graph.edgelist(as.matrix(mat), directed=FALSE))
}


# Plot Matrix -------------------------------------------------------------

# Rotate matrix 90-degrees clockwise.
rotate <- function(x) {
    t(apply(x, 2, rev))
}

# create the image
plot_mat <- function(mat){
image(rotate(mat), col = c(3, 6), axes = FALSE, frame.plot = TRUE, main = "Values > 10")
}



# Test Plot ---------------------------------------------------------------

mymat <- matrix(c(1:9, sample(1:9, replace = TRUE)), nrow = 9)
plot_mat(make_adj(mymat))

mymat[order(mymat[,2]),]


# Actual Data -------------------------------------------------------------
 hero_adj <- make_adj(data)
hero_adj <- as.matrix(hero_adj)
# Confirm all row names are column names
(colnames(hero_adj) == rownames(hero_adj)) %>% mean()

# write.csv(hero_adj, file = "./hero_adjacency.csv")
save(hero_adj, file = "./hero_adjacency.Rdata")

tony <- hero_adj[,"IRON MAN/TONY STARK "]
hero_adj_tc <- hero_adj[order(tony),]
hero_adj_tc_small <- hero_adj_tc[1:200, 1:200]
hero_adj_tc_small[,rownames(hero_adj_tc_small)==colnames(hero_adj_tc_small)]
dim(hero_adj_tc_small)
write.csv(hero_adj, file = "./hero_adjacency-tc-small.csv")

## Although this is a large file, the following will let you search through the available names in bash:
# cut -d, -f1 hero_adjacency.csv |fzf | xclip

# This represents the 200 Marvel Characters Most Connected to Tony Stark

# Unfourtunately due to a bug, it is not possible to account for weights from an adjacency matrix in gephy, hence an edge list will be used instead:

25*25*10
200/25
LETTERS[200/25]

# Choose a Multiple of H, now cut off at Letters
# Centred at Tony Stark
library(reshape2)

hl <- hero_adj_tc_small %>% melt()
hl <- hl[(hl$value != 0 & hl$Var2 != "A"), ]
(hl <- hl[rev(order(hl$value)),]) %>% head()
# Names Are Important, this MUST be imported as an Edge List
names(hl) <- c("Source", "Target", "Weight")
write.csv(hl, file = "./long_tc_small.csv", row.names = FALSE)


# TODO Export 30 as a Node Labed PDF using File -> export -> PDF
# TODO Visualise all
# TODO Make this also as a sunbirst and/or Tree map using d3 or one of the provided tools
  # TODO Consider doing an ugly thing with baobab?
# TODO Look At Another tool for a network graph.


hero_adj[1:5, 1:5]

sm <- matrix(sample(1:9999, 5^2), ncol = 5)
max(sm)
which(sm == max(sm), arr.ind = TRUE)

no1 <- which(hero_adj==max(hero_adj), arr.ind = TRUE)
no2 <- which(hero_adj==max(hero_adj[-c(425, 421), -c(421, 425)]), arr.ind = TRUE)
most_con <- rbind(no1, no2)

#biggest <- which(hero_adj==max(hero_adj[-most_con[,1], -most_con[,2]]), arr.ind = TRUE)
#most_con <- rbind(most_con, biggest)


n <- 18     # The number of times to find two more matches
n <- n/2-2 # How many times to loop

for(i in 1:(n)) {
    if(n < 1) {
        print("This loop wouldn't even run")
    } else {
        biggest <- which(hero_adj==max(hero_adj[-most_con[,1], -most_con[,2]]), arr.ind = TRUE)
        most_con <- rbind(most_con, biggest)
    }
    if(i==n) print(most_con)
}


hero_adj_most <- hero_adj[most_con[,1], most_con[,2]]
hero_adj_most <- melt(hero_adj_most)
names(hero_adj_most) <- c("Target", "Source", "Value")
hero_adj_most$Target <- as.character(hero_adj_most$Target)
hero_adj_most$Source <- as.character(hero_adj_most$Source)



hero_adj_most <- hero_adj_most[hero_adj_most$Source != hero_adj_most$Target,]
hero_adj_most <- hero_adj_most[hero_adj_most$Value > 0,]
hero_adj_most <- rbind(hero_adj_most[1,], hero_adj_most)

hero_adj_most$Source[1] <- ""
hero_adj_most$Target[1] <- hero_adj_most$Source[2]
hero_adj_most$Value[1] <- sum(hero_adj_most$Value[-1])

as_tibble(hero_adj_most) # Use df to avoid using `pull()`
