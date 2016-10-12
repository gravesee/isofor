#' @include util.R

## put splitting function in its own function
split_on_var <- function(x, ...) UseMethod("split_on_var")

split_on_var.numeric <- function(x, ...) {
  v = do.call(runif, as.list(c(1, range(x))))
  list(value = v, filter = x < v)
}

## sample the levels from this partition and map them back to the full levels
split_on_var.factor <- function(x, ...) {
  L = levels(x) ## overall levels
  l = which(L %in% unique(x)) ## which observed levels are present?
  N = length(L)
  idx = rep(0, N)
  s = sample(2^(length(l))-1, 1)
  i = which(intToBits(s)[1:N] == 1)
  idx[l[i]] <- 1
  v = sum(2^(which(idx == 1) - 1))
  list(value = v, filter = x %in% L[l[i]])
}

iTree <- function(X, l) {
  mat = matrix(0, max_nodes(l), 7, dimnames =
    list(NULL, c("Type","Size","Left","Right","SplitAtt","SplitValue","AttType")))

  # X = data, e = current depth, l = max depth, ni = node index
  recurse <- function(X, e, l, ni=1) {
    ## Base case
    if (e >= l | NROW(X) <= 1) {
      mat[ni,c("Type", "Size")] <<- c(-1, NROW(X))
      return()
    }

    ## randomly select attribute
    i = sample(1:NCOL(X), 1)

    ## check if factor with <= 32 levels
    res = split_on_var(X[, i, TRUE])

    v = res$value
    f = res$filter

    ## modify matrix in place
    mat[ni, c("Left")] <<- nL <- ni + 2 ^ e
    mat[ni, c("Right")] <<- nR <- ni + 2 ^ (e + 1)
    mat[ni, c("SplitAtt", "SplitValue", "Type")] <<- c(i, v, 1)
    mat[ni, "AttType"] <<- ifelse(is.factor(X[,i,T]), 2, 1)

    ## recurse
    recurse(X[f,,drop=FALSE], e + 1, l, nL)
    recurse(X[!f,,drop=FALSE], e + 1, l, nR)
  }
  recurse(X, 0, l)
  mat
}

#' @export
iForest <- function(X, nt, phi) {
  l = ceiling(log(phi, 2))

  forest = vector("list", nt)
  for (i in 1:nt) {
    s = sample(nrow(X), phi)
    forest[[i]] = iTree(X[s,], l)
    sz = forest[[i]][,"Size"]
    forest[[i]][,"Size"] = suppressWarnings(ifelse(sz <= 1, 0, cn(sz)))
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

pl_factor <- function(x, i, v, Forest) {
  l = Forest$vLevels[[i]]
  f = which(intToBits(v)[1:length(l)] == 1)
  x[,i,TRUE] %in% l[f]
}

iTreeFilter <- function(x, ni, Tree, Forest) UseMethod("iTreeFilter")

iTreeFilter.numeric <- function(x, ni, Tree, Forest) {
  x < Tree[ni,"SplitValue"]
}

iTreeFilter.factor <- function(x, ni, Tree, Forest) {
  i = Tree[ni,"SplitAtt"]
  v = Tree[ni,"SplitValue"]
  l = Forest$vLevels[[i]]
  f = which(intToBits(v)[1:length(l)] == 1)
  x %in% l[f]
}

pathLength <- function(x, Tree, Forest, e=0, ni=1) {
  if (Tree[ni,"Type"] == -1) return(e + Tree[ni,"Size"])
  i  = Tree[ni,"SplitAtt"]

  ifelse(
    iTreeFilter(x[,i,TRUE], ni, Tree, Forest),
    pathLength(x, Tree, Forest, e + 1, Tree[ni,"Left"]),
    pathLength(x, Tree, Forest, e + 1, Tree[ni,"Right"]))
}

#' @export
predict.iForest <- function(object, newdata, ...) {
  pls = sapply(object$forest, function(f) pathLength(newdata, f, object))
  2^(-rowMeans(pls)/cn(object$phi))
}

print.iForest <- function(x, ...) {
  txt = sprintf("Isolation Forest\n |- %d Trees\n |- Max Depth %d", x$nTrees, x$l)
  cat(txt)
}


## optimize the prediction routine
N = 1e3
x = c(rnorm(N, 0, 0.5), rnorm(N*0.05, -1.5, 1))
y = c(rnorm(N, 0, 0.5), rnorm(N*0.05,  1.5, 1))
data = data.frame(x, y)

mod = iForest(X = data, 100, 32)
p = predict(mod, data)
col = ifelse(p > quantile(p, 0.95), "red", "blue")
plot(x, y, col=col)

km = kmeans(data, 2)
plot(x, y, col=km$cluster+1)


