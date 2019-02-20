## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(viridis)

## Functions ----

source("withincor_funcs.R")

## Interface Tab Items ----

source("withincor_tab.R")

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Within-Subject Correlations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Within", tabName = "withincor_tab")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      withincor_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")

  withincor_plots <- reactive({
    resim <- input$withincor_resim

    withincor_render_plots(input$withincor_r,
                           input$withincor_m1,
                           input$withincor_m2,
                           input$withincor_sd1,
                           input$withincor_sd2,
                           input$withincor_n,
                           session)
  })

  output$ttext <- renderText({
    t <- withincor_plots()[4][[1]]

    p <- ifelse(t$p.value < .001, "< .001", paste("=", round(t$p.value, 3)))

    paste0("t = ", round(t$statistic, 2),
           "; df = ", t$parameter,
           "; p ", p)
  })

  output$withincor_plot1 <- renderPlot({
    withincor_plots()[1]
  }, height = function() {
    session$clientData$output_withincor_plot1_width/2
  })

  output$withincor_plot2 <- renderPlot({
    withincor_plots()[2]
  }, height = function() {
    session$clientData$output_withincor_plot2_width/2
  })

  output$withincor_plot3 <- renderPlot({
    withincor_plots()[3]
  }, height = function() {
    session$clientData$output_withincor_plot3_width
  })

} # end server()

shinyApp(ui, server)
