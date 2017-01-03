## functions to gain insight into a particular node

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


    res[[k]] = list(
      dir  = dir,
      var  = tree[idx, "SplitAtt"],
      val  = tree[idx, "SplitValue"],
      type = tree[idx, "AttType"])

    k = k + 1
  }

  rev(res[lengths(res)>0])
}


node_to_character <- function(node, mod) {
  vapply(node, function(n) {
    #browser()
    ## get value based on split var type
    if (n$type == 2) {
      idx = which(as.integer(intToBits(n$val)) == 1L)
      value = mod$vLevels[[n$var]][idx]
      value <- sprintf('"%s"', paste0(value, collapse = '","'))
      dir = if (n$dir == -1) "in" else "not in"
    } else {
      value = n$val
      dir = if (n$dir == -1) "<" else ">="
    }

    ## set direction
    sprintf("if %s %s %s", mod$vNames[n$var], dir, value)
  }, FUN.VALUE = "")
}

## function to get the tree and node from a sparse node index
get_node_location <- function(mod, i) {
  if (i <= mod$nTerm[1]) return(list(tree_id=1, node_id=i))
  cuml_nodes = cumsum(mod$nTerm)
  tree_id = findInterval(i, cuml_nodes, left.open = TRUE) + 1
  node_id = i %% cuml_nodes[(tree_id - 1)]
  list(tree_id=tree_id, node_id=node_id)
}



## put it all together
pretty_node <- function(mod, i) {
  pos = get_node_location(mod, i)
  node = get_node_from_model(mod, tree_num = pos$tree_id, node_num = pos$node_id)
  node_to_character(node, mod)
}





