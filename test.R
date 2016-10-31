

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





