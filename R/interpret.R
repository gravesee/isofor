make_continuous_rule <- function(tree, idx, dir) {
  val <- unname(tree[idx, "SplitValue"])
  rng <- if (dir == 1) c(Inf, Inf) else c(-Inf, -Inf)
  rng[dir] <- val
  structure(
    list(
      #dir = dir,
      var = tree[idx, "SplitAtt"],
      val = val,
      rng = rng),
    class = c("rule", "continuous"))
  }

make_discrete_rule <- function(tree, idx, dir, mod) {
  var <- tree[idx, "SplitAtt"]
  val <- tree[idx, "SplitValue"]
  lvls <- mod$vLevels[[var]]
  i <- which(as.integer(intToBits(val)) == 1L)
  structure(
    list(
      #dir = dir,
      var = var,
      val = lvls[i * dir * -1] ),
    class = c("rule", "discrete"))
}

get_node_from_model <- function(mod, tree_num, node_num) {

  tree = mod$forest[[tree_num]]

  ## row index of terminal node
  idx = which(tree[,"TerminalID"] == node_num)

  ## go up the tree gathering information along the way
  res = vector("list", mod$l)
  k = 1
  while (idx > 1) {
    mask = tree[,c("Left","Right")] == idx
    idx = row(mask)[which(mask)]
    dir = (col(mask)[which(mask)] - 1) * 2 - 1

    if (tree[idx, "AttType"] == 1)
      res[[k]] <- make_continuous_rule(tree, idx, dir)
    else
      res[[k]] <- make_discrete_rule(tree, idx, dir, mod)

    k = k + 1
  }

  rev(res[lengths(res)>0])
}

#' @export
node_to_character <- function(n, mod) UseMethod("node_to_character", n)

#' @export
node_to_character.rule <- function(n, mod) NextMethod()

#' @export
node_to_character.discrete <- function(n, mod) {
    sprintf("if %s in ('%s')", mod$vNames[n$var], paste0(n$val, collapse="','"))
}

#' @export
node_to_character.continuous <- function(n, mod) {

  v <- mod$vNames[n$var]
  ## check which ranges are infinity
  if (is.infinite(n$rng[1]))
    sprintf("if %s < %f", v, n$rng[2])
  else if (is.infinite(n$rng[2]))
    sprintf("if %s >= %f", v, n$rng[1])
  else
    sprintf("if %1$s >= %2$f & %1$s < %3$f", v, n$rng[1], n$rng[2])

}


## function to get the tree and node from a sparse node index
get_node_location <- function(mod, i) {
  if (i <= mod$nTerm[1]) return(list(tree_id=1, node_id=i))
  cuml_nodes = cumsum(mod$nTerm)
  tree_id = findInterval(i, cuml_nodes, left.open = TRUE) + 1
  node_id = i - cuml_nodes[(tree_id - 1)]
  list(tree_id=tree_id, node_id=node_id)
}

#' @export
`+.rule` <- function(e1, e2) {
  stopifnot(e1$var == e2$var)
  NextMethod()
}

#' @export
`+.continuous` <- function(e1, e2) {
  structure(list(
    dir = 0,
    var = e1$var,
    val = NA,
    rng = c(
      max(e1$rng[1], e2$rng[1]),
      min(e1$rng[2], e2$rng[2]))),
    class = c("rule", "continuous"))
}

#' @export
`+.discrete` <- function(e1, e2) {
  structure(list(
    dir = 0,
    var = e1$var,
    val = intersect(e1$val, e2$val)),
  class = c("rule", "discrete"))
}


#' @export
get_node_rules <- function(mod, i) {
  pos <- get_node_location(mod, i)
  n <- get_node_from_model(mod, tree_num = pos$tree_id, node_num = pos$node_id)
  v <- sapply(n, function(x) x$var)

  i <- unique(v)
  idx <- match(sort(i), i)

  grps <- split(n, v)[idx] ## reorder to match original ordering

  lapply(grps, function(x) if (length(x) > 1) Reduce(`+`, x) else x[[1]])
}

## put it all together
#' @export
pretty_node <- function(mod, i) {
  rules <- get_node_rules(mod, i)
  to_print <- vapply(rules, node_to_character, "", mod = mod)
  cat(to_print, sep="\n")
}

filter <- function(r, d) UseMethod("filter", r)
filter.discrete <- function(r, d) d[,r$var] %in% r$val
filter.continuous <- function(r, d) d[,r$var] >= r$rng[1] & d[r$var] < r$rng[2]

traverse_logic <- function(mod, i, data, y) {
  rules <- get_node_rules(mod, i)

  ## initial filter is all TRUE
  res <- list()
  f <- TRUE
  for (i in seq_along(rules)) {
    f <- f & filter(rules[[i]], data)

    n <- xtabs(~factor(f, levels = c(FALSE, TRUE), labels = 0:1) + factor(y, levels = 0:1))
    pct <- prop.table(n, margin = 2)
    woe <- log(apply(pct, 1, function(x) x[2]/x[1]))
    iv  <- apply(pct, 1, diff) %*% woe

    ## get stats based on this filter

    res[[i]] <- list(rule=rules[[i]], n=n, pct=pct, woe=woe, iv=iv)
  }
  res
}

#' @export
node_summary <- function(mod, i, data, y) {
  logic <- traverse_logic(mod, i, data, y)
  rules <- sapply(logic, function(n) node_to_character(n$rule, mod))

  node <- t(sapply(logic, function(x) rowSums(x$n)))
  perf <- t(sapply(logic, function(x) x$n[,2]))
  iv <- sapply(logic, "[[", "iv")
  gain <- iv - c(0, head(iv, -1))

  res <- data.frame(rules)
  res <- cbind(
    format(res, justify = "left"),
    prettyNum(node[,2], big.mark = ","),
    prettyNum(perf[,2], big.mark = ","),
    round(perf[,2]/node[,2],  4),
    round(iv, 4),
    round(cumsum(gain), 4))

  colnames(res) <- c("Logic", "Node N", "N 1s", "Rate 1s", "IV", "Cuml. Gain")
  res
}
