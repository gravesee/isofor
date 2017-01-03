library(isofor)

data(titanic, package="binnr2")
x = titanic
x$Age[is.na(x$Age)] = median(x$Age, na.rm=TRUE)

mod = iForest(x[-1], 100, phi = 128)

as <- predict(mod, x[-1])


nodes = predict(mod, x[-1], nodes=TRUE)

## binary nodes

sparse = sapply(data.frame(nodes), function(x) {
  u = sort(unique(x))
  i = numeric(max(x))
  i[u] = seq_along(u)
  m = matrix(0, nrow(nodes), length(u))
  m[cbind(seq.int(nrow(nodes)), i[x])] = 1
  m
})

X = do.call(cbind, sparse)

## try to hash it with random projection
n_hashes = 1
nbits = 32
vs = replicate(n_hashes, matrix(rnorm(ncol(X) * nbits), ncol=nbits), simplify = F)
hs = lapply(vs, function(v) X %*% v > 0)
hashes = lapply(hs, apply, 1, packBits, type="integer")

h = hashes[[1]]

s = sample(nrow(x), nrow(x)/2)

hw = t(sapply(h[-s], function(b) isofor:::bit_count(h[s], b)))

i = t(apply(hw, 1, order))

KNN = rowMeans(matrix(titanic$Survived[s[i[,1:10]]], ncol=10))

r = pROC::roc(titanic$Survived[-s], KNN)

s = sample(nrow(titanic), nrow(titanic)/2)
sample_hashes = lapply(hashes, `[`, s)
test_hashes = lapply(hashes, `[`, -s)

## get test results
f = function(i) {
  knn = Map(function(a, b) which(a == b), sample_hashes, lapply(hashes, '[', i))
  ids = sapply(knn, function(idx) s[idx])
  u = unique(unlist(ids))
  z = proxy::dist(t(nodes[i,]), nodes[u,], method=function(a,b) sum(a!=b))
  u[order(z)]
}

K = 10
# val = lapply(seq.int(nrow(nodes))[-s], f)
p = t(sapply(val, function(i) x$Survived[i][1:K]))


plt = data.frame(sample_hashes[[1]], sample_hashes[[2]], sample_hashes[[3]])

scl = scale(plt)

prcomp(scl)

q1 = t(sapply(test_hashes[[1]], function(q) s[order(abs(q - sample_hashes[[1]]))]))
q2 = t(sapply(test_hashes[[2]], function(q) s[order(abs(q - sample_hashes[[2]]))]))
q3 = t(sapply(test_hashes[[3]], function(q) s[order(abs(q - sample_hashes[[3]]))]))
q4 = t(sapply(test_hashes[[4]], function(q) s[order(abs(q - sample_hashes[[4]]))]))


K = 10
KNN1 = q1[,1:K]
KNN2 = q2[,1:K]
KNN3 = q3[,1:K]
KNN4 = q4[,1:K]
# p = t(sapply(knns, function(i) x$Survived[i]))
# knns = lapply(hash[-s], function(q) s[order(abs(q - hash[s]))][1:K])

phat1 = rowMeans(matrix(x$Survived[KNN1], ncol=K))
phat2 = rowMeans(matrix(x$Survived[KNN2], ncol=K))
phat3 = rowMeans(matrix(x$Survived[KNN3], ncol=K))
phat4 = rowMeans(matrix(x$Survived[KNN4], ncol=K))

r1 = pROC::roc(x$Survived[-s], phat1)
r2 = pROC::roc(x$Survived[-s], phat2)
r3 = pROC::roc(x$Survived[-s], phat3)
r4 = pROC::roc(x$Survived[-s], phat4)
rc = pROC::roc(x$Survived[-s], rowMeans(cbind(phat1, phat2, phat3, phat4)))
rc = pROC::roc(x$Survived[-s], pmax(phat1, phat2, phat3))




# KNN using the hashes ...
i = hashes[[1]] %in% c(710, 1990)
d = proxy::dist(nodes[i,], method=function(a,b) sum(a!=b))
clust = hclust(d)
k = cutree(clust, 2)






