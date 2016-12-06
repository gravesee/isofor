data(titanic, package="binnr2")
titanic$Age[is.na(titanic$Age)] = median(titanic$Age, na.rm=TRUE)
library(isofor)

load("F:/INFD1604_9797/data/inferred_fraud_analysis.rda")

d = df[22:247]

x = titanic

mod = iForest(d)
nodes = predict(mod, d, nodes=TRUE)


## randomly sample from columns and hash them
k = 10
cols = replicate(20, sample(1:ncol(nodes), k), simplify = F)
vecs = replicate(20, rnorm(k), simplify = F)


s = c(sample(which(df$nd_fraud == 1), 250), sample(which(df$nd_fraud == 0), 250))
dst = proxy::dist(nodes[-s,], nodes[s,], function(a,b) sum(a != b))

knn = apply(dst, 1, order)
K = 50

p = matrix(df$nd_fraud[s][t(knn)[,1:K]], ncol=K)
p2 = rowMeans(p)

library(mjollnir)

ks.table(-p2, df$nd_fraud[-s])
ks.table(df$fp_3[-s], df$nd_fraud[-s])

fit = glm(df$nd_fraud[-s] ~ p2 + df$fp_2[-s], family="binomial")

tmp = predict(fit)

ks.table(-tmp, df$nd_fraud[-s])
ks.table(-p2, df$nd_fraud[-s])
ks.table(df$fp_2[-s], df$nd_fraud[-s])


k = 5
cols = replicate(50, sample(1:ncol(nodes), k), simplify = F)
vecs = replicate(50, rnorm(k), simplify = F)
hash_funcs = Map(function(a, b) round(a %*% b, 2), lapply(cols, function(x) nodes[s,x]), vecs)

row = nodes[-s,][1,]


query = function(row) {
  cmp = Map(function(a, b) round(a %*% b, 2), lapply(cols, function(x) t(row[x])), vecs)
  do.call(c, Map(function(a, b) which(a == cmp), hash_funcs, cmp))
}


nns = lapply(seq.int(nrow(nodes))[-s], function(i) {
  if (i %% 10000 == 0) print(i)
  query(nodes[i,])
})

rerun = which(lengths(nns) < 10)

ords = Map(function(i, b) {
  if (i %% 10000 == 0) print(i)
  if (length(b) < 2) return(-1)
  proxy::dist(t(nodes[i,]), nodes[s,][b,], method = function(a, b) sum(a!=b))
  },
  seq.int(nrow(nodes))[-s], nns)


p = sapply(nns, function(i) mean(df$nd_fraud[s][i]))


ks.table(-p, df$nd_fraud[-s])

table(lengths(nns))


gen1 = nodes[,1:4]
for (i in 1:5) gen1 = (gen1 - (gen1 %% 2))/2

gen2 = nodes[,1:4]
for (i in 1:4) gen2 = (gen2 - (gen2 %% 2))/2

gen3 = nodes[,1:4]
for (i in 1:3) gen3 = (gen3 - (gen3 %% 2))/2


## hash gen 1
g1_hash = round(gen1 %*% rnorm(4), 2)
g2_hash = round(gen2 %*% rnorm(4), 2)
g3_hash = round(gen3 %*% rnorm(4), 2)

tmp = cbind(g1_hash, g2_hash, g3_hash)
i = order(tmp[,1], tmp[,2], tmp[,3])

## nodes to binary
m = matrix(0, nrow(nodes), max(nodes[,1]))
m[cbind(seq.int(nrow(nodes)), nodes[,1])] = 1



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






