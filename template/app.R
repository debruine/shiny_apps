## libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
})

## functions ----

# source("R/func.R") # put long functions in external files

# display debugging messages in R if local, 
# or in the console log if remote
debug_msg <- function(...) {
    is_local <- Sys.getenv('SHINY_PORT') == ""
    txt <- paste(...)
    if (is_local) {
        message(txt)
    } else {
        shinyjs::logjs(txt)
    }
}

## tabs ----

# you can put complex tabs in separate files and source them
#source("ui/main_tab.R")
#source("ui/info_tab.R")

main_tab <- tabItem(
    tabName = "main_tab",
    h2("Main")
)

info_tab <- tabItem(
    tabName = "info_tab",
    h2("Info")
)


# if the header and/or sidebar get too complex, 
# put them in external files and uncomment below 
# source("ui/header.R") # defines the `header`
# source("ui/sidebar.R") # defines the `sidebar`


## UI ----
ui <- dashboardPage(
    skin = "red",
    # header, # if sourced above
    # sidebar, # if sourced above
    dashboardHeader(title = "Template"),
    dashboardSidebar(
        # https://fontawesome.com/icons?d=gallery&m=free
        sidebarMenu(
            id = "tabs",
            menuItem("Main", tabName = "main_tab",
                     icon = icon("home")),
            menuItem("Info", tabName = "info_tab",
                     icon = icon("info"))
        ),
        actionButton("say_hi", "Say Hi")
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") # links to www/custom.css
        ),
        tabItems(
            main_tab,
            info_tab
        )
    )
)


## server ----
server <- function(input, output, session) {
    observeEvent(input$say_hi, {
        debug_msg("say_hi", input$say_hi)
        shinyjs::alert("Hi")
    })
} 

shinyApp(ui, server)