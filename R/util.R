## helper to calculate average path length
cn <- function(n) {
  H = log(n - 1) + 0.5772156649
  2 * H - (2*(n - 1)/n)
}

## used to calculate maximum possible nodes for given tree height
max_nodes <- function(l, b=2) {
  (b ^ (l) - 1) + b ^ l
}


## get parent node given a child node
p = function(n) {
  pow = ceiling(log(n, 2)) - 1
  res = n - 2 ^ (pow + 1)
  ifelse(n == 1, 0, ifelse(res > 0, res, n - 2 ^ (pow)))
}

#' @export
isofor_demo = function() {
  appDir = system.file("isofor-demo", package = "isofor")
  #shinyApp(ui = ui, server = server, )
  shiny::runApp(appDir)
}

