iTree <- function(X, l) {
  mat = matrix(0, max_nodes(l), 6, dimnames=list(NULL, c(
    "Type","Size","Left","Right","SplitAtt","SplitValue")))

  # X = data, e = current depth, l = max depth, ni = node index
  recurse <- function(X, e, l, ni=1) {
    ## Base case
    if (e >= l | NROW(X) <= 1) {
      mat[ni,c("Type", "Size")] <<- c(-1, NROW(X))
      return()
    }

    ## randomly select attribute
    i = sample(1:NCOL(X), 1)
    v = do.call(runif, as.list(c(1, range(X[,i]))))

    ## create filter
    f = X[,i] < v

    ## modify matrix in place
    mat[ni, c("Left")] <<- nL <- ni + 2 ^ e
    mat[ni, c("Right")] <<- nR <- ni + 2 ^ (e + 1)
    mat[ni, c("SplitAtt", "SplitValue", "Type")] <<- c(i, v, 1)

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
      forest = forest,
      phi    = phi,
      l      = l,
      nTrees = nt,
      nVars  = NCOL(X),
      vNames = colnames(X)), class = "iForest")
}

pathLength <- function(x, Tree, e=0, ni=1) {
  if (Tree[ni,"Type"] == -1) return(e + Tree[ni,"Size"])

  ifelse(
    x[,Tree[ni,"SplitAtt"]] < Tree[ni,"SplitValue"],
    pathLength(x, Tree, e + 1, Tree[ni,"Left"]),
    pathLength(x, Tree, e + 1, Tree[ni,"Right"]))
}

predict.iForest <- function(object, newdata, ...) {
  pls = sapply(object$forest, function(f) pathLength(newdata, f))
  2^(-rowMeans(pls)/cn(object$phi))
}

print.iForest <- function(x, ...) {
  txt = sprintf("Isolation Forest\n |- %d Trees\n |- Max Depth %d", x$nTrees, x$l)
  cat(txt)
}

data("iris")
X = titanic
for (i in 1:5) X = rbind(X, X)

X$Age[is.na(X$Age)] = median(X$Age, na.rm=TRUE)
mf = model.matrix(~.-1-Survived, X)

# data(titanic, package="binnr2")
mod1 = iForest(mf, 100, phi = 256)

p = predict(mod1, mf)


x1 = c(rnorm(1000), rnorm(50, -3, 0.1))
x2 = c(rnorm(1000), rnorm(50, 3, 0.1))

data = cbind(x1, x2)
iForest(data, 20)







