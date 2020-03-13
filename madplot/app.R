# Libraries ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr) # for data wrangling 

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "MadPlots"),
  dashboardSidebar(
    textInput("xlab",  "X-axis"),
    textInput("x1",  "X category 1"),
    textInput("x2",  "X category 2"),
    textInput("ylab",  "Y-axis")
  ),
  dashboardBody(
    plotOutput("madPlot")
  ),
  title = "MadPlots"
)

# Define server logic ----
server <- function(input, output, session) {
  output$madPlot <- renderPlot({
    dat <- data.frame(
      x = rep(c(input$x1, input$x2), each = 100),
      y = c(rnorm(100), rnorm(100, runif(1)))
    )
      
    thePlot <- ggplot(dat, aes(x, y)) +
      geom_violin(aes(fill = x), show.legend = FALSE) +
      geom_boxplot(width = 0.25) +
      xlab(input$xlab) +
      ylab(input$ylab)
    
    thePlot
  })
}

shinyApp(ui, server)

