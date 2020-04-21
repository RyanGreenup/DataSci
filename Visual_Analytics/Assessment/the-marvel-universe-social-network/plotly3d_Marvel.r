# How many data points
library(mise)
mise()
library(tidyverse)
library(plotly)
library(rjson)
library(igraph)
library(reshape2)
library(tidyverse)

# Load the Data
setwd("/home/ryan/Dropbox/Notes/DataSci/Visual_Analytics/Assessment/the-marvel-universe-social-network")
 # data <- read.csv(file = "./hero_adjacency.csv") # Bad for column names
# load(file = "./hero_adjacency.Rdata")
# data <- hero_adj
# data <- melt(data)
# data <- data[data$value!=0,]
# names(data) <- c("Source", "Target", "Value")
# data <- data[order(-data$Value),]
# save(data, file = "edges_list_from_adj_with_values.r")


load(file = "edges_list_from_adj_with_values.r")
head(data)
data <- data[1:999,] # 100 looked really nice

# Get the number of nodes and edges
nodes <- factor(c(as.character(data$Source), as.character(data$Target)))
nodes <- nodes[!duplicated(nodes)]
nn <- length(nodes)
ne <- nrow(data)

# Build the Graph
Edges <- data
G <- igraph::graph_from_data_frame(Edges, directed = FALSE)

# Get the node positions set by the lKamada-Kawai layout for 3D graphs 
layt <- as.data.frame(igraph::layout.kamada.kawai(G, dim = 3))
names(layt) <- c("xval", "yval", "zval")
layt$label <- vertex_attr(G)[[1]]
head(layt)
   
## The order is not preserved
(factor(vertex_attr(G)[[1]])==factor(nodes)) %>% mean()


#   layt <- igraph::layout.auto(G, dim = 3)
   
   # Set data for the Plotly plot of the graph:
   
   xn <- layt[,1] # X co-ordinate of nodes
   yn <- layt[,2]
   zn <- layt[,3]
   
   # Add the end co-ordinates to the edges data frame
   # ends <- matrix(nrow = ne, ncol = 3)
   # for (i in 1:ne) {
   #   ends[i,1] <- layt[which(layt$label==Edges$Target[i]),1]
   #   ends[i,2] <- layt[which(layt$label==Edges$Target[i]),2]
   #   ends[i,3] <- layt[which(layt$label==Edges$Target[i]),3]
   # }
   # ends %>% head()
   # 
   # 
   # starts <- matrix(nrow = ne, ncol = 3)
   # for (i in 1:ne) {
   #   starts[i,1] <- layt[which(layt$label==Edges$Source[i]),1]
   #   starts[i,2] <- layt[which(layt$label==Edges$Source[i]),2]
   #   starts[i,3] <- layt[which(layt$label==Edges$Source[i]),3]
   # }
   # starts %>% head()
   # 
   # 
   #  # Layout is list of nodes in order 
   # Xe <- ends[,1] # Coordinate of Target/end Node for Edge
   # Ye <- ends[,2]
   # Ze <- ends[,3]
   # 
   # Xs <- starts[,1] # Coordinate of Target/end Node for Edge
   # Ys <- starts[,2]
   # Zs <- starts[,3]
   # 
   # # So lines will draw lines between all the given endpoints, you just need to list them
   # Xe <- c()
   # Ye <- c()
   # Ze <- c()
   # for (i in 1:ne) {
   #   Xe <- c(Xe, layt[which(layt$label==Edges$Target[i]), 1], layt[which(layt$label==Edges$Source[i]), 1])
   #   Ye <- c(Ye, layt[which(layt$label==Edges$Target[i]), 2], layt[which(layt$label==Edges$Source[i]), 2])
   #   Ze <- c(Ze, layt[which(layt$label==Edges$Target[i]), 3], layt[which(layt$label==Edges$Source[i]), 3])
   # }
   # Xe %>% head()
   # 
   # 
   # Try and build the edges again, what's above is wrong (ggplot is correct)
   
   
   Zs <- Ys <- Xs <- Ze <- Ye <- Xe <- vector(length = ne)
   for (i in seq_len(ne)) {
     Xs[i] <- layt$xval[layt$label==Edges$Source[i]]
     Ys[i] <- layt$yval[layt$label==Edges$Source[i]]
     Zs[i] <- layt$zval[layt$label==Edges$Source[i]]
     Xe[i] <- layt$xval[layt$label==Edges$Target[i]]
     Ye[i] <- layt$yval[layt$label==Edges$Target[i]]
     Ze[i] <- layt$zval[layt$label==Edges$Target[i]]
   }
   
   starts    <- data.frame("xval" = Xs, "yval" = Ys, "zval" = Zs, edgenum = 1:ne) # TODO Make a factor
   ends      <- data.frame("xval" = Xe, "yval" = Ye, "zval" = Ze, edgenum = 1:ne)
   Edges_val <- as_tibble(rbind(starts,ends))
   Edges_val
   
   
   
   # Start Building the Plotly
   
   # Plotly
ponts <- plot_ly(x = layt[,1],
                 y = layt[,2],
                 z = layt[,3],
                 text = layt$label,
                 color = layt$label,
                # text = 0:(nn-1),
                type = "scatter3d",
                mode = "markers"); ponts
   
  # I can'g get edge grouping to work so I use a loop instead
lines <- plot_ly( x = Edges_val$xval, 
                  y = Edges_val$yval, 
                  z = Edges_val$zval,
                  groups = Edges_val$edgenum,   # The group labels match the order from x, y ,z.
                  mode = 'lines',
                  hoverinfo = 'none',
                 showbackground = FALSE
                 ); lines
line_plots <- list()   # Does not support groups so you must create a seperate line plot so it doesn't just join everything
for (i in 1:ne) {                # the idea is to create a different sub plot for every line.
line_plots[i] <- plot_ly(
    x = c(starts$xval[i], ends$xval[i]),
    y = c(starts$yval[i], ends$yval[i]),
    z = c(starts$zval[i], ends$zval[i]),
    mode = 'lines',
    showlegend = FALSE) # You don't want each edge to be labelled, only the nodes
}
lines_grouped <- subplot(line_plots)

# https://plotly.com/python/v3/3d-network-graph/

# Pull out background.
noback <- list(showticks = FALSE,
               showgrid = FALSE,
               showaxis = FALSE,
               gridcolor = "white",
               showline=FALSE,
               zeroline=FALSE,
               showticklabels=FALSE, title='')


lines <- lines %>% layout(scene = list(xaxis=noback,yaxis=noback,zaxis=noback))
lines_grouped <- lines_grouped %>% layout(scene = list(xaxis=noback,yaxis=noback,zaxis=noback))
ponts <- ponts %>% layout(scene = list(xaxis=noback,yaxis=noback,zaxis=noback)) # you need to do this for points to get rid of x,y,z labels

# subplot(ponts, lines)
 subplot(lines_grouped, ponts)
   
 # A good discussion might be on the colours and the legend
    # Useless on paper, super handy for interactive because it gives the option to query a few independent nodes
      # While browsing in response to the overlay and in conjuction with the physical seperation.
   
   # GGPlot (# Because of the way that I made the graph, the nodes come from the edges, hence, every node must be connected.)
  G <- igraph::graph_from_data_frame(d = Edges, directed = FALSE, vertices = nodes)
   laytg <- igraph::layout.circle(G, dim = 2)
   laytg <- igraph::layout.davidson.harel(G)
   colnames(laytg) <- c("xval", "yval")
   laytg <- as_tibble(laytg)
   laytg$node <- vertex_attr(G)[[1]]
   laytg
   
   
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
        geom_line(aes(group = edgenum)) +
        geom_point(data = laytg, aes(x = xval, y = yval, col = node), size = 2) +
        theme_classic()
  ggplotly(p)
   
   # So the nodes are just laid out in such a way so as to create islands, 
     # It is possible to lay them out so they all connect and it is possible to lay them out so they can create islands, 
         # for example the fantastic 4
   
