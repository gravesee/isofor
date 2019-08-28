#' @include util.R

## put splitting function in its own function
split_on_var <- function(x, ...) UseMethod("split_on_var")

split_on_var.numeric <- function(x, ...) {
  v = do.call(stats::runif, as.list(c(1, range(x))))
  list(value = v, filter = x < v)
}

## sample the levels from this partition and map them back to the full levels
split_on_var.factor <- function(x, ..., idx=integer(32)) {
  l = which(levels(x) %in% unique(x)) ## which observed levels are present?

  if (length(l) < 1) print("zero length!?")
  ## don't sample 0 or all
  s = sample(max(1, 2^(length(l)) - 2), 1)

  i = l[which(intToBits(s) == 1)]
  idx[i] = 1L
  list(value = packBits(idx, type="integer"), filter = x %in% levels(x)[i])
}

## pull the recursive function out
# X = data, e = current depth, l = max depth, ni = node index
recurse <- function(idx, e, l, ni=0, env, sentinel) {

  ## don't sample columns with all dups
  dups <- sapply(env$X[idx,,drop=F], function(x) all(duplicated(x)[-1L]))

  ## Base case
  if (e >= l || length(idx) <= 1 || all(dups)) {
    env$mat[ni,c("Type", "Size")] <- c(-1, length(idx))
    return()
  }

  ## randomly select attribute
  if (identical(sum(!dups), 1L)) {
    i <- which(!dups)
  } else {
    i <- sample(which(!dups), 1)
  }

  ## check if factor with <= 32 levels
  res = split_on_var(env$X[idx, i, TRUE])
  f = res$filter

  ## modify matrix in place
  env$mat[ni, c("Left")] <- nL <- 2 * ni
  env$mat[ni, c("Right")] <- nR <- 2 * ni + 1

  env$mat[ni, c("SplitAtt", "SplitValue", "Type")] <- c(i, res$value, 1)
  env$mat[ni, "AttType"] <- ifelse(is.factor(env$X[,i,T]), 2, 1)

  ## recurse
  recurse(idx[which(f)] , e + 1, l, nL, env, sentinel)
  recurse(idx[which(!f)], e + 1, l, nR, env, sentinel)
}


## lots of allocated matrix space goes unused because max_nodes is an upper bound
## on how large the tree can be. This function subsets to only the rows needed
## for prediction
compress_matrix <- function(m) {
  m = cbind(seq.int(nrow(m)), m)[m[,"Type"] != 0,,drop=FALSE]
  m[,"Left"] = match(m[,"Left"], m[,1], nomatch = 0)
  m[,"Right"] = match(m[,"Right"], m[,1], nomatch = 0)
  m[m[,"Type"] == -1,"TerminalID"] = seq.int(sum(m[,"Type"] == -1))
  m[,-1,drop=FALSE]
}

iTree <- function(X, l) {
  env = new.env() ## pass everything in this environment to avoid copies
  env$mat = matrix(0,
   nrow = max_nodes(l),
   ncol = 8,
   dimnames = list(NULL, c("TerminalID", "Type","Size","Left","Right","SplitAtt","SplitValue","AttType")))
  env$X = X

  recurse(seq.int(nrow(X)), e=0, l=l, ni=1, env)
  compress_matrix(env$mat)
}

#' @title iForest
#'
#' @description Build an Isolation Forest of completely random trees
#'
#' @param X a matrix or data.frame of numeric or factors values
#' @param nt the number of trees in the ensemble
#' @param phi the number of samples to draw without replacement to construct each tree
#' @param seed random seed to ensure creation of reproducible foresets
#' @param replace_missing if TRUE, replaces missing factor levels with "." and missing
#' numeric values with the \code{sentinel} argument
#' @param sentinel value to use as stand-in for missing numeric values
#' @param ncolsample if not NULL, the default, `ncolsample` features are subsampled for each
#' tree. See details for more information.
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
#' If \code{ncolsample} is not null, the algorithm will subsample a number of features equal
#' to \code{ncolsample} to construct each tree. The features are not sampled randomly, but use
#' an appropriate measure for the column's class. This measure is first applied to all columns.
#' The decreasing order is then calculated and the first \code{ncolsample} columns are taken 
#' from this ordering. Numeric fields are ordered using kurtosis while factors use Shannon
#' entropy.
#'
#' @return an \code{iForest} object
#' @references F. T. Liu, K. M. Ting, Z.-H. Zhou, "Isolation-based anomaly detection",
#' \emph{ACM Trans. Knowl. Discov. Data}, vol. 6, no. 1, pp. 3:1-3:39, Mar. 2012.
#'
#' @examples
#' library(isofor)
#' mod1 <- iForest(iris, phi = 16, nt=100)
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#' @export
iForest <- function(X, nt=100, phi=256, seed=1234, replace_missing=TRUE, sentinel=-9999999999, ncolsample=NULL) {

  set.seed(seed)

  l = ceiling(log(phi, 2))

  if (!is.data.frame(X)) X <- as.data.frame(X)
  
  if(nrow(X) < phi) phi = nrow(X)

  ## impute missing values
  if (replace_missing) {
    for (i in seq_along(X)) {
      if (is.numeric(X[[i]])) {
        X[[i]][is.na(X[[i]])] <- sentinel
      } else if (is.factor(X[[i]])) {
        levels(X[[i]]) <- c(levels(X[[i]]), ".")
        X[[i]][is.na(X[[i]])] <- "."
      }
    }
  }

  # Check that no single factor has > 32 levels
  factor32 <- sapply(X, function(x) class(x) == "factor" & nlevels(x) > 32)
  if(sum(factor32) > 0) stop("Can not handle categorical predictors with more than 32 categories.")

  ## check for missing values
  na.cols <- sapply(X, function(col) any(is.na(col)))
  if (any(na.cols)) stop("Missing values found in: ", paste0(names(X[na.cols]), collapse = ", "))
  
  ## check ncolsample is valid
  if (!is.null(ncolsample)) {
    if (!is.numeric(ncolsample) || ncolsample < 0) stop("Invalid value for ncolsample.")
    if (ncolsample >= ncol(X)) {
      ncolsample <- NULL
      warning("ncolsample >= ncol(X) resulting in no subsampling.")
    }
  }

    forest <- vector("list", nt)
    for (i in 1:nt) {
      s <- sample(nrow(X), phi)
      Xs <- X[s,,drop=FALSE]
      if (!is.null(ncolsample)) {
        cols <- sample_cols_(Xs, sentinel)
        Xs[,-head(cols, ncolsample),drop=FALSE] <- NA
      }
      forest[[i]] <- iTree(Xs, l)
    }

  structure(
    list(
      forest  = forest,
      phi     = as.integer(phi),
      l       = l,
      nTrees  = as.integer(nt),
      nVars   = NCOL(X),
      nTerm   = sapply(forest, function(t) max(t[,"TerminalID"])),
      vNames  = colnames(X),
      vLevels = sapply(X, levels)),
    class = "iForest")
}

#' Printing iForest Objects
#' 
#' @param x object of class iForest.
#' @param ... ignored
#' @method print iForest
#' @export
print.iForest <- function(x, ...) {
  txt = sprintf("Isolation Forest with %d Trees and Max Depth of %d", x$nTrees, x$l)
  cat(txt)
}
