library(isofor)
library(mjollnir)

data(titanic, package="binnr2")

x = titanic
x$Age[is.na(x$Age)] <- median(x$Age, TRUE)

mod <- iForest(x[-1], phi = 64, nt = 500)

as <- predict(mod, x[-1])
ids <- predict(mod, x[-1], nodes=TRUE)
nodes <- predict(mod, x[-1], sparse=TRUE)

## create inverted list of node members
il <- lapply(unname(split(nodes@i + 1, factor(nodes@j + 1, levels=seq.int(ncol(nodes))), drop=F)), as.integer)
qs <- lapply(unname(split(nodes@j + 1, nodes@i + 1)), as.integer)

## grab all nearest neighbors
knns <- lapply(qs, isofor:::top_k_distances, il, as.integer(nrow(nodes)), 10L)

nn10 <- do.call(rbind, knns)
titanic[nn10[200,],]


library(igraph)
g <- graph.adjlist(knns, mode = "all")
g <- simplify(g, remove.loops = TRUE)

V(g)$label <- NA

## create clusters
V(g)$color <- x$Survived + 1

library(rgexf)

sink("titanic.gexf")
rgexf::igraph.to.gexf(g)
sink()







r1 <- roc(x$Survived[-s], p)
r2 <- roc(x$Survived[-s], p2[-s])
r3 <- roc(x$Survived[-s], p3)


plot(r1)
par(new=TRUE)
plot(r2, col="blue")
par(new=TRUE)
plot(r3, col="red")

top_k_distances(q, il, as.integer(nrow(nodes)), 10L)

object.size(il)/1024^3

qs <- lapply(1:nrow(nodes), function(i) which(nodes[i,] == 1L))
knns <- lapply(qs, top_k_distances, il, as.integer(nrow(nodes)), 891L)


find_knn <- function(i, k=10) {
  #out <- integer(nrow(nodes))
  ids <- which(nodes[i,] == 1L)

}
knns <- sapply(1:nrow(titanic), find_knn)

z <- nodes
for (i in 1:5) z <- rbind(z, z)
il <- lapply(1:ncol(z), function(j) which(z[,j] == 1))
qs <- lapply(1:nrow(z), function(i) which(z[i,] == 1L))


knns <- lapply(qs, top_k_distances, il, as.integer(nrow(z)), 10L)
knns2 <- lapply(knns, function(x) order(-x)[2:10])

dists <- proxy::dist(as.matrix(nodes), function(a, b) sum(a!=b))

p <- rowMeans(matrix(titanic$Survived[t(knns)], ncol=10))
r <- pROC::roc(titanic$Survived, p)

titanic[c(800, find_knn(800)),]

find_nn <- function(q, k=10) {

}


x[as>quantile(as, 0.99),]
x[as<=quantile(as, 0.01),]




