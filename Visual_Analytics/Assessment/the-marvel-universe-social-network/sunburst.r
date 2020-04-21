## [[/home/ryan/Dropbox/Notes/DataSci/Visual_Analytics/Assessment/the-marvel-universe-social-network/adjmat.r]]
setwd("/home/ryan/Dropbox/Notes/DataSci/Visual_Analytics/Assessment/the-marvel-universe-social-network/")
lts <- read.csv(file = "./long_tc_small.csv")

lts

library(plotly)
library(reshape2)

fig <- plot_ly(
    labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura", "John", "John"),
    parents = c("",    "Eve",  "Eve", "Seth", "Seth", "Eve",  "Eve",   "Awan", "Eve", "Enos", "Cain"),
    values = c(10,        14,      12,    10,      2,     6,      6,        4,
    4, 9, 9),
    type = 'sunburst'
)

fig

lts <- lts [1:20,]


fig <- plot_ly(
    labels  = lts$Source,
    parents = lts$Target,
    values  = lts$Weight,
    type = 'sunburst'
)

fig


## So Iron man goes to all these different people, but, I don't know who all
## those different people end up going to. I don't want an Iron Man Sentric
## Graph, I want a Graph of the most connected people.

# Only Iron Man's external characters
ltsf <- lts[lts$Source == lts$Source[3], ]

# Weight is already considered so remove duplicates
lts <- lts[lts$Source != lts$Target,]
duplicated(lts$Target) %>% sum()


plot_ly(
    labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura", "John", "John"),
    parents = c("",    "Eve",  "Eve", "Seth", "Seth", "Eve",  "Eve",   "Awan", "Eve", "Enos", "Cain"),
    values = c(10,        14,      12,    10,      2,     6,      6,        4,
    4, 9, 9),
    type = 'sunburst'
)

lts <- hero_adj_most
names(lts) <- c("labels", "parents", "values")
lts <- lts[,c(2,1,3)]
head(lts)

fig <- plot_ly(labels = lts$labels,
               parents = lts$parents,
               values = lts$values,
               type = 'sunburst')


fig


plot_ly(
    labels = c("Miss", "Cin", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura", "John", "John"),
    parents = c("",    "Miss",  "Miss", "Seth", "Seth", "Miss",  "Miss",   "Awan", "Miss", "Enos", "Cin"),
    values = c(10,        14,      12,    10,      2,     6,      6,        4,
    4, 9, 9),
    type = 'sunburst'
)


plot_ly(
  parents = c(""   , "Ymir","Ymir", "Odin"),
  labels = c("Ymir", "Odin","Loki", "Thor"),
  values = c(10, 7, 4, 2), 
  type = 'sunburst'
)



## So I really need more interconnections,
## Another issue with this also is that I can't have recursive connections
## So if in column a and in column B then remove the corresponding row for
## column B

## Note on Plotly
# If a parent equals a label plotly will fail silently.
# 0 values must also be removed


# Clean the Adjacency Matrix ----------------------------------------------

load(file = "./hero_adjacency.Rdata")
data <- hero_adj

# Find the Most Social Characters to Consider ===================================

#' With 6000 characters to visualise, it's not going to be practical to
#' visualise all of them, so instead, what we'll do is only consider the most
#' social characters for want of an effective visualisation, the argument might
#' be made that more popular characters should be the focus point but in the end
#' the most connected characters will make the best visualisation and it's all
#' fiction anyway.

no1 <- which(hero_adj==max(hero_adj), arr.ind = TRUE)
data.frame(parents = rownames(no1)[1], labels = rownames(no1)[2], values = data[no1[1,1], no1[1,2]])



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

# Subset the Data To Only Consider the top 10-30 ###########################
ss <- data[most_con[,1], most_con[,2]]
ss <- melt(ss, varnames = c("labels", "parents"))
(ss <- ss[order(-ss$value),]) %>% head()

# Make a SunBurst ---------------------------------------------------------

# All Labels Must have Parents ============================================
ss
#' No Child can be it's own parent
ss <- ss[(ss$parents != ss$labels),]

#' Each Child must come from a parent

#' The exption to this is genesis


# Child Cannot be its own parent =======================================
================================================================================
================================================================================

# Build a Best-Friend Matrix


# Build SunBirst Matrix ---------------------------------------------------

## Build the First Parent
sd <- data
no1 <- which(sd==max(sd), arr.ind = TRUE)
sb <- data.frame(parents = "", labels = rownames(no1)[1], values = data[no1[1,1], no1[1,2]])

## Build all the Children
sd[no1[1,1], ] > 0 


## Remove this candidate
sd <- sd[-no1[1,1], -no1[1,2]] # Gotta remove most because the direction of her bff was established by the order of the adjmat
sd <- sd[-no1[2,1], -no1[2,2]]




## Identify Next Closest Relationship
no1 <- which(sd==max(sd), arr.ind = TRUE)
sb <- data.frame(parents = "", labels = rownames(no1)[1], values = data[no1[1,1], no1[1,2]])

which(max(data$val))






# Build Best Friend Matrix ------------------------------------------------
## Try from different characters by re running, johnny storm is very diverse
i <- 1
sd <- data[-1,-1]

# Most Social
no1    <- which(sd==max(sd), arr.ind = TRUE)
no1_name <- rownames(no1)[1]

# Create the Data frame
bfdf   <- data.frame(parents = "", labels = rownames(no1)[1], values = max(sd)) # reset to 1 later, it's easier than reordering

# Add that onto the dataframe
bfrow  <- data.frame(parents = rownames(no1)[1], labels = rownames(no1)[2], values = as.numeric(sd[no1[1,1], no1[1,2]]))
bfdf <- rbind(bfdf, bfrow)

# Find Closest Associates
(sd_melt <- melt(sd, varnames = c("labels", "parents"))) %>% head(); names(sd_melt)[3] <- "values"
no1f <- sd_melt[sd_melt$parents==rownames(no1)[1],]
no1f <- no1f[no1f$values>0,]

# Put Closest Associates in Data Frame
bfdf <- rbind(bfdf, no1f)

# Remove Duplicate Labels (children)
bfdf <- bfdf[!duplicated(bfdf$labels),]

# Sort by socialability
bfdf <- bfdf[order(-bfdf$values),] # weird ordering to keep root at top because value is 1

# Remove all most social from data
sd <- sd[rownames(sd)!=no1_name, colnames(sd)!=no1_name] # Remove Parent
sd <- sd[-which(rownames(sd) %in% bfdf$labels), -which(colnames(sd) %in% bfdf$labels)] # Remove all labels (children)

top <-  bfdf[2,"labels"]

(fig <- plot_ly( labels  = bfdf$labels, parents = bfdf$parents, values  = bfdf$values, type = 'sunburst' ))
# Fill in the first layer Children =======================================================

l1_end <- length(bfdf$labels)

for (i in seq_len(length(bfdf$labels[-1]))) {
#  no1_name <- bfdf$labels[i+1]
  no1_name <- top
  
  # Find matches
  matches <- sd[no1_name,] #Only Associates of no1_name
  matches <- matches[matches>0]
  matches <<- matches[order(-matches)] # Put in order
  
  
  # Add that onto the dataframe
  bfrow  <- data.frame(parents = rep(no1_name, length(matches)), labels = names(matches), values = matches)
  bfdf <- rbind(bfdf, bfrow)
  
  # Remove all most social from data
  sd <- sd[rownames(sd)!=no1_name, colnames(sd)!=no1_name] # Remove Parent
  sd <- sd[-which(rownames(sd) %in% bfdf$labels), -which(colnames(sd) %in% bfdf$labels)] # Remove all labels (children)
      # Integer 0 when no match bug, fix, don't use which 
  
  top <- names(matches)[1] # This match needs to also be removed
  sd <- sd[rownames(sd)!=top, colnames(sd)!=top] # Remove Parent
}

l2_end <- length(bfdf$labels)  

# Now how do I do the second layer?
# well the second layer goes from 25 to now
# I can't loop above because it's looking for broken values.


