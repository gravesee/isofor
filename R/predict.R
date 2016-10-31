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

pathLength2 <- function(x, Tree, Forest, e=0, ni=0) {
  pathLength_cpp(x, Tree, Forest, e=0, ni=0, as.integer(nrow(x)))
}

#' @title predcit.iForest
#' @description predict.iForest is a method of the predict generic function.
#' @param object an \code{iForest} object
#' @param newdata a dataset to predict
#' @param type predict can export the anamoly score, a list of nodes, or the terminal nodes
#' @export
predict.iForest <- function(object, newdata, ..., method = c("cpp","r","terminal")) {
  type = match.arg(type)

  # TODO: check for factors and numeric only, error otherwise
  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

  i = match(object$vNames, names(newdata))

  if (any(is.na(i))) {
    m = object$vNames[!object$vNames %in% names(newdata)]
    stop(strwrap(c("Variables found in model not found in newdata: ",
      paste0(m, collapse = ", ")), width = 80, prefix = " "), call. = F)
  }

  switch(
    method,
    "cpp" = {
      pls = sapply(object$forest, function(f) pathLength2(newdata, f, object))
      2^(-rowMeans(pls)/cn(object$phi))
    },
    "r" = {
      pls = sapply(object$forest, function(f) pathLength(newdata, f, object))
      2^(-rowMeans(pls)/cn(object$phi))
    }
  )
}
