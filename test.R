
library(isofor)
library(microbenchmark)

data(titanic, package="binnr2")

titanic$Age[is.na(titanic$Age)] <- 0

microbenchmark(
  new=iForest(titanic, 100, 256, multicore = TRUE),
  old=iForest(titanic, 100, 256, multicore = FALSE), times = 5L)

microbenchmark(
  new = predict(mod, x, iterative = TRUE),
  old = predict(mod, x, iterative = FALSE), times=5L)

p1 <- predict(mod, titanic, iterative = TRUE)
p2 <- predict(mod, titanic, iterative = FALSE)

x <- titanic
for (i in 1:3) x <- rbind(x, x)




x <- mtcars

for (i in 1:3) x <- rbind(x, x)

microbenchmark(
  new=predict(mod, titanic, iterative = TRUE),
  old=predict(mod, titanic, iterative = FALSE), times = 5L)
