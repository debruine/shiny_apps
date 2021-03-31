# Libraries ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr) # for data wrangling 

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "MadPlots"),
  dashboardSidebar(
    textInput("xlab",  "X-axis", "X"),
    textInput("x1",  "X category 1", "A"),
    textInput("x2",  "X category 2", "B"),
    textInput("ylab",  "Y-axis", "Y"),
    actionButton("resim", "Re-Simulate Data")
  ),
  dashboardBody(
    plotOutput("madPlot")
  ),
  title = "MadPlots"
)

# Define server logic ----
server <- function(input, output, session) {
  rv <- reactiveValues()
  
  observeEvent(input$resim, {
    rv$dat <- data.frame(
      x = rep(c("A", "B"), each = 100),
      y = c(rnorm(100), rnorm(100, runif(1)))
    )
  }, ignoreNULL = FALSE)
  
  output$madPlot <- renderPlot({
    ggplot(rv$dat , aes(x, y)) +
      geom_violin(aes(fill = x), show.legend = FALSE) +
      geom_boxplot(width = 0.25) +
      xlab(input$xlab) +
      ylab(input$ylab) +
      scale_x_discrete(labels = c(input$x1, input$x2)) +
      theme_minimal(base_size = 18)
  })
}

shinyApp(ui, server)

