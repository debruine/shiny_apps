## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

## Functions ----

source("npred_funcs.R")

## Interface Tab Items ----

source("npred_tab.R")

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "N Predictors"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("N Predictors", tabName = "npred_tab")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      npred_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")

  output$npred_plot <- renderPlot({
    resim <- input$npred_resim

    # keep track of progress, this function is long
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making plot", value = 0)

    npred_plot(input$npred_n, input$npred_vars, input$npred_reps, progress)
  }, height = function() {
    session$clientData$output_npred_plot_width/1.62
  })

} # end server()

shinyApp(ui, server)
