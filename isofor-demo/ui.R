library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Isolation Forest Demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold", "Anomaly Score Threshold", min = 0, max = 1,
        step = 0.01, value = 1, width="200px"),
      shiny::selectInput("depth", "Tree Depth", choices = c(3,4,5,6,7,8),
        width="200px", selected=5),
      shiny::selectInput("ntree", "Number of Trees", choices = c(10,20,50,100,200,500),
        width="200px", selected = 50)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Score"),
      plotOutput("Depth")
    )
  )
))

