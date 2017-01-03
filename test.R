library(isofor)

data(titanic, package="binnr2")
x = titanic
x$Age[is.na(x$Age)] = median(x$Age, na.rm=TRUE)

mod = iForest(x[-1], 100, phi = 64)

nodes = predict(mod, x[-1], sparse=TRUE)
as = predict(mod, x[-1])

## drop those with insufficient counts
d <- colSums(nodes) <= 25

library(glmnet)

fit <- cv.glmnet(nodes[,!d], x$Survived, alpha=1, family="binomial")
plot(fit)

## which nodes are most important?
coefs <- coef(fit, s = "lambda.1se")

top10 <- head(order(-abs(coefs[-1])), 10)

invisible(lapply(top10, function(i) {
  cat("\n")
  cat(isofor:::pretty_node(mod, which(!d)[i]), sep="\n")
}))

cat(isofor:::pretty_node(mod, which(!d)[658]), sep="\n")
cat(isofor:::pretty_node(mod, which(!d)[319]), sep="\n")


p <- predict(fit, nodes[,!d], s="lambda.min")
hist(p)
library(mjollnir)
ks.table(-p, x$Survived)
