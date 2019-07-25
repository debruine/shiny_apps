# Libraries ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

# Functions ----
source("R/func.R")

# Define UI ----
source("ui/header.R")
source("ui/sidebar.R")
source("ui/main_tab.R")

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  title = "Template",
  skin = "purple", # "blue", "black", "purple", "green", "red", "yellow"
  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "custom.css")
    ),
    tabItems(
      main_tab
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

}

# Run the application ----
shinyApp(ui = ui, server = server)

