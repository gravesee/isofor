## helper to calculate average path length
cn <- function(n) {
  H = log(n - 1) + 0.5772156649
  2 * H - (2*(n - 1)/n)
}

## used to calculate maximum possible nodes for given tree height
max_nodes <- function(l) (2 ^ (l) - 1) + 2 ^ l
