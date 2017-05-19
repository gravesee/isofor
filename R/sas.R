## convert isolation forest to sas code
recurse_sas <- function(t, d=1) {
  ## base case
  if (t[d,'Type'] == -1) {
    return(list(size=t[d,'Size'], leaf=TRUE))
  }

  return(c(
    list(
      type=t[d,'AttType'],
      att=t[d,'SplitAtt'],
      val=t[d,'SplitValue'],
      children = list(
        left=recurse_sas(t,  t[d,'Left']),
        right=recurse_sas(t,  t[d,'Right'])
        )
      )
    )
  )
}

get_tree_data <- function(mod) {
  lapply(mod$forest, recurse_sas)
}

.s <- function(n) {
  paste0(rep("  ", n), collapse = "")
}

Sprintf <- function(fmt, ..., depth=0) {
  fmt <- paste0(.s(depth), fmt)
  if (!length(list(...))) return(fmt)
  sprintf(fmt, ...)
}

sas_numeric <- function(l, mod, d) {
  Sprintf("if %s < %f then do;", mod$vNames[l$att], l$val, depth=d)
}

sas_factor <- function(l, mod, d) {

  lvls <- mod$vLevels[[l$att]]
  v <- lvls[which(intToBits(l$val) == 1)]

  Sprintf("if %s in (\"%s\") then do;", mod$vNames[l$att],
    paste(v, collapse = '","'), depth=d)

}

cn <- function(n) {
  if (n == 2) {
    return(1)
  } else if (n < 2) {
    return(0);
  } else {
    H = log(n - 1) + 0.5772156649;
    return(2 * H - (2*(n - 1)/n));
  }
}

sas_leaf <- function(l, pfx="iso", i=0, d) {
  Sprintf("%s_size_%d = %f;", pfx, i, d + cn(l$size), depth=d)
}

## recursively walk tree data list and print out SAS statement
iso_tree_to_sas <- function(l, mod, pfx="iso", i=0, d=0) {

  ## base case
  if (!is.null(l$leaf)) return(sas_leaf(l, pfx, i, d))

  if (l$type == 1) {
    code <- sas_numeric(l, mod, d)
  } else if (l$type == 2) {
    code <- sas_factor(l, mod, d)
  }

  ## recurse
  return(
    c(code,
      iso_tree_to_sas(l$children$left, mod, pfx, i, d+1),
        Sprintf("else do;", depth=d),
      iso_tree_to_sas(l$children$right, mod, pfx, i, d+1),
        Sprintf("end;", depth=d)
    ))
}

#' Export isolation forest to SAS code
#' @param mod an isolation forest model
#' @param pfx the prefix for each isolation forest tree
#' @param nt optional number of trees to output. Defaults to all trees.
#' @export
isofor_to_sas <- function(mod, pfx="iso", nt=NULL) {
  if (is.null(nt)) nt <- mod$nTrees
  stopifnot(nt <= length(mod$forest))

  code <- list()
  for (i in seq.int(nt)) {
    td <- recurse_sas(mod$forest[[i]]) ## tree data
    code[[i]] <- c(
      sprintf("\n/*** Tree: %d ***/", i),
      iso_tree_to_sas(td, mod, pfx=pfx, i))
  }

  avg <- cn(mod$phi)
  c(unlist(code),
    sprintf("\n%s_avg_path_length = sum(of: %s_size_:) / %d;", pfx, pfx, nt),
    sprintf("%s_anomaly_score = 2 ** ((-1 * %s_avg_path_length) / %f);",
      pfx, pfx, avg)
  )
}