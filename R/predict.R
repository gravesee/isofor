pathLength <- function(x, Tree, e=0, ni=0) {
  pathLength_cpp(x, Tree, e=0, ni=0)
}

#' @title predcit.iForest
#' @description predict.iForest is a method of the predict generic function.
#' @param object an \code{iForest} object
#' @param newdata a dataset to predict
#' @param multicore true/false value indicating if prediction should be run in parallel
#' @param type predict can export the anamoly score, a list of nodes, or the terminal nodes
#' @param iterative exposed for testing. If set to TRUE uses the iterative prediction
#' method (instead of recrusion). If built with openmp nets a ~n-cores X speedup.
#' @import parallel
#' @export
predict.iForest <- function(object, newdata, ..., multicore=FALSE, nodes = FALSE, sparse = FALSE, iterative=TRUE) {

  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

  ## check column types
  classes = unlist(lapply(newdata, class))
  if (!all(classes %in% c("numeric","factor","integer", "ordered"))) {
    stop("newdata contains classes other than numeric, factor, and integer")
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

  if(multicore && !iterative){
	ncores <- detectCores()
  	chunks <- split(newdata, (seq(nrow(newdata))-1) %/% round(nrow(newdata)/ncores))

  	cl <- makeCluster(getOption("cl.cores", ncores))

  	if (sparse) {
    		yh <- parLapply(cl, chunks, predict_iForest_sparse_nodes, object)

  	} else if (nodes) {
    		yh <- parLapply(cl, chunks, predict_iForest_nodes_cpp, object)

  	} else {
    		yh <- parLapply(cl, chunks, predict_iForest_pathLength_cpp, object)

  	}

	stopCluster(cl)
  	return(unlist(yh, use.names = FALSE))
  } else if(iterative) {

    predict_iterative(newdata, object)

	} else {

    if (sparse){
		  predict_iForest_sparse_nodes(newdata, object)
	  } else if (nodes) {
		  predict_iForest_nodes_cpp(newdata, object)
	  } else {
		  predict_iForest_pathLength_cpp(newdata, object)
	  }
}

}
