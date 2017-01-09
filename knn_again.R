library(FNN)
library(isofor)

## implement this in c!

data(titanic, package="binnr2")

x = titanic
x$Age[is.na(x$Age)] <- median(x$Age, TRUE)

mod <- iForest(x[-1])
nodes <- predict(mod, x[-1], sparse=TRUE)

q = which(nodes[1,] == 1L)

il <- lapply(1:ncol(nodes), function(j) which(nodes[,j] == 1))
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




