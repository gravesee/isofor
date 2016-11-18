

data(titanic, package="binnr2")

titanic$Age[is.na(titanic$Age)] = median(titanic$Age, na.rm=TRUE)

library(isofor)

x = mtcars
x = titanic[,c("Sex","Pclass","Embarked")]
x = titanic
for (i in 1:8) x = rbind(x, x)

mod = iForest(x[,-1], 500, phi=128)
p1 = predict(mod, x, nodes=TRUE)

p2 = predict(mod, x, method="r")
p3 = predict(mod, x)


## cluster based on nodes
library(cluster)

p1 = predict(mod, x[,-1], nodes=TRUE)

f <- function(x, y) 1 - sum(x == y)/length(x)
s = proxy::dist(p1, method=f)
clus = hclust(s)
k = cutree(clus, k = 10)
tmp = svd(as.matrix(s), nu = 2, nv = 2)

library(igraph)
g = graph.adjacency(1-as.matrix(s), weighted = TRUE, diag = FALSE, mode = "lower")

library(rgexf)
sink("test.gexf")
rgexf::igraph.to.gexf(g)
sink("")

nodes_df <- data.frame(ID = c(1:vcount(g)), NAME = V(g)$name)
# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(g, c(1:ecount(g))))

write.gexf(nodes_df, edges_df, output = "test.gexf", nodesAtt = titanic, edgesWeight = E(g)$weight)






p = predict(mod, d, nodes=TRUE)
f <- function(x, y) 1-sum(x == y)/length(x)
s = proxy::dist(p, method=f)
clus = hclust(s)
k = cutree(clus, k = 10)


library(tsne)
plt = tsne(s, k = 2)


mod = iForest(x[,-1], 100, phi = 32)
p = predict(mod, x[,-1], nodes=TRUE)
## turn each tree into a one hot matrix

sparse = matrix(0, nrow(p), ncol(p)*isofor:::max_nodes(ceiling(log2(32))))






