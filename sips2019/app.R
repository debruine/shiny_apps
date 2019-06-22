library(shiny)
library(shinydashboard)
library(dplyr)
library(XML)

ui <- dashboardPage(
    dashboardHeader(title = "SIPS 2019 Schedule"),
    dashboardSidebar(
        sidebarMenu(
          tags$a(href="https://osf.io/ndzpt/", "OSF Page"),
          br(),
          tags$a(href="https://docs.google.com/spreadsheets/d/1G_SoWUquak6oD-b2L3L-XB7cF97UAGb8PPyVg3r0Lts/edit#gid=0", "Live Updating Schedule")
        )
    ),
    dashboardBody(
      tabsetPanel(type = "tabs",
                  tabPanel("Sunday",
          tabsetPanel(type = "tabs",
                      tabPanel("13:45", htmlOutput("sunday1")),
                      tabPanel("14:30", htmlOutput("sunday2")),
                      tabPanel("15:45", htmlOutput("sunday3")),
                      tabPanel("16:30", htmlOutput("sunday4"))
                      )),
          tabPanel("Monday",
          tabsetPanel(type = "tabs",
                      tabPanel("09:30", htmlOutput("monday1")),
                      tabPanel("10:15", htmlOutput("monday2")),
                      tabPanel("11:30", htmlOutput("monday3")),
                      tabPanel("13:30", htmlOutput("monday4")),
                      tabPanel("14:45", htmlOutput("monday5")),
                      tabPanel("15:30", htmlOutput("monday6"))
                      )),
          tabPanel("Tuesday",
          tabsetPanel(type = "tabs",
                      tabPanel("09:30", htmlOutput("tuesday1")),
                      tabPanel("10:15", htmlOutput("tuesday2")),
                      tabPanel("11:30", htmlOutput("tuesday3")),
                      tabPanel("13:30", htmlOutput("tuesday4")),
                      tabPanel("14:45", htmlOutput("tuesday5")),
                      tabPanel("15:30", htmlOutput("tuesday6"))
          ))
          ))
)

readGoogleSheet <- function(url){
  # Suppress warnings because Google docs seems to have incomplete final line
  suppressWarnings({
    doc <- paste(readLines(url), collapse=" ")
  })
  if(nchar(doc) == 0) stop("No content found")
  tables <- getNodeSet(htmlParse(doc), "//table")
  ret <- readHTMLTable(tables[[1]], header=TRUE, 
                       stringsAsFactors=FALSE, 
                       as.data.frame=TRUE)
}

read_row <- function(sips, row) {
  hm <- sips[row, 2]
  
  sessions <- sips[row, 3:26] %>%
    unlist() %>%
    unname()
  
  valid_sessions <- sessions[sessions != "" & sessions != "EDGE OF THE WORLD. TURN BACK."] 
  
  valid_sessions %>%
    paste0(collapse = "</li>\n\n<li><input type='checkbox'> ") %>%
    paste0("<h3>", hm, "</h3>\n\n", 
           "<ul><li><input type='checkbox'> ", ., "</li></ul>")
  
  
  x <- purrr::map2(1:length(valid_sessions), valid_sessions, function(i, v) {
    checkboxInput(paste0(row, "_", i), label = v, width = "100%")
  })
  
  c(list(h3(hm)), x)
}



server <- function(input, output) {
  sips <- reactive({
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Loading Schedule", value = 0)
    on.exit(progress$close())
    progress$set(value = 0.5, detail = "running")
    
    
    readGoogleSheet(url="https://docs.google.com/spreadsheets/d/1G_SoWUquak6oD-b2L3L-XB7cF97UAGb8PPyVg3r0Lts/edit#gid=0")
  })
  
  output$sunday1 <- renderUI({ read_row(sips(), 17) })
  output$sunday2 <- renderUI({ read_row(sips(), 18) })
  output$sunday3 <- renderUI({ read_row(sips(), 22) })
  output$sunday4 <- renderUI({ read_row(sips(), 23) })
  
  output$monday1 <- renderUI({ read_row(sips(), 42) })
  output$monday2 <- renderUI({ read_row(sips(), 43) })
  output$monday3 <- renderUI({ read_row(sips(), 45) })
  output$monday4 <- renderUI({ read_row(sips(), 47) })
  output$monday5 <- renderUI({ read_row(sips(), 49) })
  output$monday6 <- renderUI({ read_row(sips(), 50) })
  
  output$tuesday1 <- renderUI({ read_row(sips(), 70) })
  output$tuesday2 <- renderUI({ read_row(sips(), 71) })
  output$tuesday3 <- renderUI({ read_row(sips(), 73) })
  output$tuesday4 <- renderUI({ read_row(sips(), 75) })
  output$tuesday5 <- renderUI({ read_row(sips(), 77) })
  output$tuesday6 <- renderUI({ read_row(sips(), 78) })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
