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

#' @export
isofor_demo = function() {
  appDir = system.file("shiny-examples", "isofor-demo", package = "isofor")
  shiny::runApp(appDir)
}


top_k_distances <- function(q, il, n, k) {
  .Call("top_k_distances", as.integer(q), il, as.integer(n), as.integer(k))
}


