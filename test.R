library(isofor)
library(Matrix)

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
  pretty_node(mod, which(!d)[i])
}))

cat(isofor:::pretty_node(mod, which(!d)[658]), sep="\n")
cat(isofor:::pretty_node(mod, which(!d)[319]), sep="\n")

p <- predict(fit, nodes[,!d], s="lambda.min")
hist(p)
library(mjollnir)
ks.table(-p, x$Survived)



## test node

## take a node, split it by attributes and send to the proper combine function

n <- isofor:::get_node_from_model(mod, 1, 10)
atts <- sapply(n, function(x) x$var)


l <- split(n, atts)


## create summary stats for nodes
f <- function(mod, nodes, i, y) {
  n <- xtabs(~factor(nodes[,i], levels = 0:1)+factor(y, levels = 0:1))
  pcts <- prop.table(n, margin = 1)

  out <- cbind(addmargins(n, 2), pcts[,2])[,c(3,1,2,4)]
  colnames(out) <- c("Total","# 0s","# 1s","1s Rate")
  rownames(out) <- c("Not in Node", "In Node")

  cat(c(
    "\nNode Logic",
    "----------------------",
    get_node_rules(mod, i),
    "\nPerformance",
    "----------------------"),
    sep="\n")
  print(out)
}

## got it working!

best <- which(!d)[top10]

for (i in best) f(mod, nodes, i, x$Survived)



