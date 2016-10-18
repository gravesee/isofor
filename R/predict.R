pl_factor <- function(x, i, v, Forest) {
  l = Forest$vLevels[[i]]
  f = which(intToBits(v)[1:length(l)] == 1)
  x[,i,TRUE] %in% l[f]
}

iTreeFilter <- function(x, ni, Tree, Forest, i) UseMethod("iTreeFilter")

iTreeFilter.numeric <- function(x, ni, Tree, Forest, i) {
  x < Tree[ni,"SplitValue"]
}

iTreeFilter.factor <- function(x, ni, Tree, Forest, i) {
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
    iTreeFilter(x[,i,TRUE], ni, Tree, Forest, i),
    pathLength(x, Tree, Forest, e + 1, Tree[ni,"Left"]),
    pathLength(x, Tree, Forest, e + 1, Tree[ni,"Right"]))
}


findNode <- function(x, Tree, Forest, e=0, ni=1) {
  if (Tree[ni,"Type"] == -1) return(ni)
  i  = Tree[ni,"SplitAtt"]

  ifelse(
    iTreeFilter(x[,i,TRUE], ni, Tree, Forest, i),
    findNode(x, Tree, Forest, e + 1, Tree[ni,"Left"]),
    findNode(x, Tree, Forest, e + 1, Tree[ni,"Right"]))
}

nodeMembership <- function(x, Tree, Forest) {
  res = findNode(x, Tree, Forest)
  id = seq.int(nrow(x))
  flags = matrix(0, nrow(x), max_nodes(Forest$l))
  flags[cbind(id, res)] <- 1
  while (any(res > 0)) {
    flags[cbind(id, p(res))] <- 1
    res <- p(res)
  }
  flags
}

#' @title predcit.iForest
#'
#' @description predict.iForest is a method of the predict generic function.
#'
#' @param object an \code{iForest} object
#' @param newdata a dataset to predict
#'
#'
#' @export
predict.iForest <- function(object, newdata, ..., type=c("score","nodes")) {
  type = match.arg(type)

  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

  i = match(object$vNames, names(newdata))

  if (any(is.na(i))) {
    m = object$vNames[!object$vNames %in% names(newdata)]
    stop(strwrap(c("Variables found in model not found in newdata: ",
      paste0(m, collapse = ", ")), width = 80, prefix = " "), call. = F)
  }

  switch(
    type,
    "score" = {
      pls = sapply(object$forest, function(f) pathLength(newdata, f, object))
      2^(-rowMeans(pls)/cn(object$phi))
    },
    "nodes" = {
      lapply(object$forest, function(f) nodeMembership(newdata, f, object))
    }
  )
}
