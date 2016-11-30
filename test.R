

data(titanic, package="binnr2")

titanic$Age[is.na(titanic$Age)] = median(titanic$Age, na.rm=TRUE)

library(isofor)

x = mtcars
x = titanic[,c("Sex","Pclass","Embarked")]
x = titanic
for (i in 1:8) x = rbind(x, x)

mod = iForest(x[,-1], 500, phi=128)
p1 = predict(mod, x[,-1], nodes=TRUE)


pca = prcomp(p1, center = TRUE, scale. = TRUE)

library(glmnet)

s = sample(nrow(p1), nrow(p1)/2)
fit = cv.glmnet(x=pca$x, y=titanic$Survived, family="binomial", alpha=1)
fit2 = cv.glmnet(x=p1[s,], y=titanic$Survived[s], family="binomial", alpha=1)
lr = glm(Survived~., titanic, family="binomial")

library(gbm)



p = predict(fit2, p1, s="lambda.1se")

library(mjollnir)

ks.table(-p[s], titanic$Survived[s])
ks.table(-p[-s], titanic$Survived[-s])
ks.table(-predict(lr), titanic$Survived)

library(igraph)


t1 = mod$forest[[1]]
f = which(t1[,1] == 1)

al = cbind(rep(f, 2), as.character(t1[f,3:4]))

g = graph_from_edgelist(al, directed = TRUE)

plot(g, layout=layout_as_tree(g), vertex.size=4, edge.arrow.mode="-")

plot(g)
