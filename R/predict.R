#' @title predict.iForest
#'
#' @description return predictions of various types for the isolation forest
#' object
#'
#' @param object an \code{iForest} object
#'
#' @param newdata a dataset to predict
#'
#' @param n.cores number of cores to use for prediction of anomaly score. Must
#' be compiled with openmp. Defaults to 1.
#'
#' @param nodes if true return nobs x ntrees dim matrix with terminal node ids
#'
#' @param sparse if true return sparse Matrix of dimension nobs x nTerminalNodes.
#' Each column represents a terminal node. There are as many ones in each row
#' as there are trees in the forest. Each observation can only belong to one
#' terminal node per tree. Useful for further modeling or to identify predictive
#' interactions.
#'
#' @details By default the predict function returns an anomaly score. The
#' anomaly score is a [0,1] scaled measure of isolation. Higher scores
#' correspond to more isolated observations. If sparse or nodes are set to TRUE,
#' a matrix of the requested type is returned.
#'
#' @import Matrix
#'
#' @importFrom parallel detectCores
#'
#' @export
#'
predict.iForest <- function(object, newdata, ..., n.cores=1, nodes = FALSE,
  sparse = FALSE, replace_missing=TRUE, sentinel=-9999999999) {

  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

  ## check column types
  classes = unlist(lapply(newdata, class))
  if (!all(classes %in% c("numeric","factor","integer", "ordered"))) {
    stop("newdata contains classes other than numeric, factor, and integer")
  }

  ## impute missing values
  if (replace_missing) {
    for (i in seq_along(newdata)) {
      if (is.numeric(newdata[[i]])) {
        newdata[[i]][is.na(newdata[[i]])] <- sentinel
      } else if (is.factor(newdata[[i]])) {
        levels(newdata[[i]]) <- c(levels(newdata[[i]]), ".")
        newdata[[i]][is.na(newdata[[i]])] <- "."
      }
    }
  }

  ## check for missing values
  for (k in seq_along(newdata)) {
    if (any(is.na(newdata[k]))) stop("Missing values found in newdata")
  }

  ## check for column name mismatches
  i = match(object$vNames, names(newdata))
  if (any(is.na(i))) {
    m = object$vNames[!object$vNames %in% names(newdata)]
    stop(strwrap(c("Variables found in model not found in newdata: ",
      paste0(m, collapse = ", ")), width = 80, prefix = " "), call. = F)
  }

  ## dispatch to requested prediction method
  if (sparse){
	  predict_iForest_sparse_nodes(newdata, object)
  } else if (nodes) {
	  predict_iForest_nodes_cpp(newdata, object)
  } else {
    num_cores = as.integer(max(1, min(n.cores, detectCores())))
    predict_iForest_pathlength_cpp(newdata, object, num_cores)
  }
}
