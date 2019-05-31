#' @useDynLib isofor
#' @importFrom Rcpp sourceCpp
NULL

#' @import Matrix
NULL


## helper to calculate average path length
cn <- function(n) {
  if (n == 2) {
    1
  } else if (n < 2) {
    0
  } else {
    H = log(n - 1) + 0.5772156649
    2 * H - (2*(n - 1)/n)
  }
}

## used to calculate maximum possible nodes for given tree height
max_nodes <- function(l, b=2) (b ^ (l) - 1) + b ^ l

#' Isolation Forest Demo
#' Shiny app demonstrating isolation forest parameters and
#' their affect on the anomaly score.
#' @export
isofor_demo = function() {
  appDir = system.file("shiny-examples", "isofor-demo", package = "isofor")
  shiny::runApp(appDir)
}

kurtosis <- function(x, sentinel) {
  f <- !(is.na(x) | x == sentinel)
  mn <- mean(x[f])
  ss <- x[f] - mn
  m4 <- mean(ss^4)
  m2 <- mean(ss^2)
  m4/m2^2 - 3  
}

entropy <- function(x, base=log2) {
  prop <- prop.table(table(x))
  -sum(prop * base(prop))
}

sample_cols_ <- function(df, sentinel) {
  i <- sapply(df, function(x) if (is.factor(x)) entropy(x) else kurtosis(x, sentinel))
  order(-abs(i))
}

