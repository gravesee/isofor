

data(titanic, package="binnr2")

titanic$Age[is.na(titanic$Age)] = median(titanic$Age, na.rm=TRUE)

library(isofor)

x = mtcars
x = titanic[,c("Sex","Pclass","Embarked")]
x = titanic
for (i in 1:8) x = rbind(x, x)

mod = iForest(x, 100, phi=128)
p1 = predict(mod, x)

p2 = predict(mod, x, method="r")
p3 = predict(mod, x)



f = mod$forest[[1]]
i = f[1,"SplitAtt"]
f1 = isofor:::iTreeFilter_factor(as.integer(x[,i]), -1, f, mod)
f2 = isofor:::iTreeFilter.factor(x[,i], 1, f, mod)

## verify this works... yes
isofor:::which_eq_one(2)
which(intToBits(2) == 1) - 1


microbenchmark::microbenchmark(
  predict(mod, x, type="cpp"),
  predict(mod, x, type="r"), times = 5)



library(IsolationForest)


ifor = IsolationForest::IsolationTrees(x, 100, hlim = 8, rowSamp = TRUE, nRowSamp = 256)

as = AnomalyScore(x, ifor)$outF
p1 = predict(mod, x, method="cpp")



mod = iForest(x, phi=128)

t1 = mod$forest[[1]]


library(igraph)

f = which(t1[,1] == 1)

al = cbind(as.character(c(0, 0, rep(f, 2))), as.character(c(1, 2, t1[f,3:4])))

g = igraph::graph_from_edgelist(al, directed = T)


#g = set_edge_attr(g, "label", E(g), "+")
# g = set_vertex_attr(g, "size", which(t1[,1] == -1), value = t1[which(t1[,1] == -1),"Size"])
plot(g, layout=layout_as_tree(g), vertex.size=4, vertex.label=NA, edge.arrow.mode="-")
g = igraph::graph_from_adjacency_matrix()

