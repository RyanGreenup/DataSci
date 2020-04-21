
# igraph ------------------------------------------------------------------

library(igraph)
library(ape)


g <- barabasi.game(1000, power=1)
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

g <- barabasi.game(1000, power=1)
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)



# ggplot2 -----------------------------------------------------------------
library(GGally)
library(ggnet)
library(network)
library(sna)
library(ggplot2)


# random graph
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]

