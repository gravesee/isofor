
library(isofor)
library(microbenchmark)

data(titanic, package="binnr")

titanic$Age[is.na(titanic$Age)] <- 0

mod <- iForest(titanic, 500, 32)
mod <- iForest(mtcars, 2, 8)

p1 <- predict(mod, titanic, iterative = TRUE)

microbenchmark(
  new = predict(mod, x, iterative = TRUE),
  old = predict(mod, x, iterative = FALSE), times=5L)

p1 <- predict(mod, titanic, iterative = TRUE)
p2 <- predict(mod, titanic, iterative = FALSE)

x <- titanic

for (i in 1:10) x <- rbind(x, x)

mod <- iForest(x, 500, 64, multicore = TRUE)
p1 <- predict(mod, x, n.cores = 4)


x <- mtcars

for (i in 1:3) x <- rbind(x, x)

microbenchmark(
  new=predict(mod, titanic, iterative = TRUE),
  old=predict(mod, titanic, iterative = FALSE), times = 5L)
