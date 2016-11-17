#' @include util.R

## put splitting function in its own function
split_on_var <- function(x, ...) UseMethod("split_on_var")

split_on_var.numeric <- function(x, ...) {
  v = do.call(runif, as.list(c(1, range(x))))
  list(value = v, filter = x < v)
}

## sample the levels from this partition and map them back to the full levels
split_on_var.factor <- function(x, ..., idx=integer(32)) {
  l = which(levels(x) %in% unique(x)) ## which observed levels are present?
  s = sample(2^(length(l)) - 1, 1)
  i = l[which(intToBits(s) == 1)]
  idx[i] = 1L
  list(value = packBits(idx, type="integer"), filter = x %in% levels(x)[i])
}

## pull the recursive function out
# X = data, e = current depth, l = max depth, ni = node index
recurse <- function(X, e, l, ni=0, env) {
  ## Base case
  if (e >= l | NROW(X) <= 1) {
    env$mat[ni,c("Type", "Size")] <- c(-1, NROW(X))
    return()
  }

  ## randomly select attribute
  i = sample(1:NCOL(X), 1)

  ## check if factor with <= 32 levels
  res = split_on_var(X[, i, TRUE])
  f = res$filter

  ## modify matrix in place
  env$mat[ni, c("Left")] <- nL <- 2 * ni
  env$mat[ni, c("Right")] <- nR <- 2 * ni + 1

  env$mat[ni, c("SplitAtt", "SplitValue", "Type")] <- c(i, res$value, 1)
  env$mat[ni, "AttType"] <- ifelse(is.factor(X[,i,T]), 2, 1)

  ## recurse
  recurse(X[f,,drop=FALSE] , e + 1, l, nL, env)
  recurse(X[!f,,drop=FALSE], e + 1, l, nR, env)
}


compress_matrix <- function(m) {
  m = cbind(seq.int(nrow(m)), m)[m[,1] != 0,,drop=FALSE]
  m[,4] = match(m[,4], m[,1], nomatch = 0)
  m[,5] = match(m[,5], m[,1], nomatch = 0)
  m[,-1,drop=FALSE]
}

iTree <- function(X, l) {
  env = new.env()
  env$mat = matrix(0,
    nrow = max_nodes(l),
    ncol = 7,
    dimnames = list(NULL,
      c("Type","Size","Left","Right","SplitAtt","SplitValue","AttType")))

  recurse(X, e=0, l=l, ni=1, env)
  compress_matrix(env$mat)
}

#' @title iForest
#'
#' @description Build an Isolation Forest of completely random trees
#'
#' @param X a matrix or data.frame of numeric or factors values
#' @param nt the number of trees in the ensemble
#' @param phi the number of samples to draw without replacement to construct each tree
#'
#' @details An Isolation Forest is an unsupervised anomaly detection algorithm. The requested
#' number of trees, \code{nt}, are built completely at random on a subsample of size \code{phi}.
#' At each node a random variable is selected. A random split is chosen from the range of that
#' variable. A random sample of factor levels are chosen in the case the variable is a factor.
#'
#' Records from \code{X} are then filtered based on the split criterion and the tree building
#' begins again on the left and right subsets of the data. Tree building terminates when the
#' maximum depth of the tree is reached or there are 1 or fewer observations in the filtered
#' subset.
#'
#' @return an \code{iForest} object
#' @references F. T. Liu, K. M. Ting, Z.-H. Zhou, "Isolation-based anomaly detection",
#' \emph{ACM Trans. Knowl. Discov. Data}, vol. 6, no. 1, pp. 3:1-3:39, Mar. 2012.
#'
#' @export
iForest <- function(X, nt=100, phi=256) {
  l = ceiling(log(phi, 2))

  if (!is.data.frame(X)) X <- as.data.frame(X)

  forest = vector("list", nt)
  for (i in 1:nt) {
    s = sample(nrow(X), phi)
    forest[[i]] = iTree(X[s,], l)
  }

  structure(
    list(
      forest  = forest,
      phi     = phi,
      l       = l,
      nTrees  = nt,
      nVars   = NCOL(X),
      vNames  = colnames(X),
      vLevels = sapply(X, levels)),
    class = "iForest")
}

#' @export
print.iForest <- function(x, ...) {
  txt = sprintf("Isolation Forest with %d Trees and Max Depth of %d", x$nTrees, x$l)
  cat(txt)
}
