library(isofor)

# TODO: add tabs with different examples
# TODO: move dummy data into dataset stored with package

## create dummy data
N = 1e3
x = c(rnorm(N, 0, 0.25), rnorm(N*0.05, -1.5, 1))
y = c(rnorm(N, 0, 0.25), rnorm(N*0.05,  1.5, 1))
pch = c(rep(0, N), rep(1, (0.05*N))) + 2
d = data.frame(x, y)
rngs = lapply(d, range)
ex = do.call(expand.grid, lapply(rngs, function(x) seq(x[1], x[2], diff(x)/20)))

## create isolation forest

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  mod = reactive({
    iForest(d, as.integer(input$ntree), phi=2^as.integer(input$depth))
  })

  p = reactive({
    predict(mod(), d, n.cores=4L)
  })

  output$Score <- renderPlot({
    col = ifelse(p() > input$threshold, "red", "blue")
    plot(d, col = col, pch = pch, cex=2)
    title("Dummy Data with Outliers")
  })

  output$Depth <- renderPlot({
    p = predict(mod(), ex, iterative=TRUE)
    plt = cbind(ex, z=p)
    lattice::contourplot(z~x+y, plt, cuts=10, labels=TRUE, region=TRUE)
  })
})

