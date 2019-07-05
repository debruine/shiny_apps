library(shiny)
library(shinydashboard)
library(dplyr)
library(XML)

fields <- c()

saveData <- function(data, name) {
  data <- t(data)
  # Create a unique file name
  fileName <- name
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path("responses", fileName), 
    row.names = FALSE, quote = TRUE
  )
}

ui <- dashboardPage(
    dashboardHeader(title = "SIPS 2019 Schedule"),
    dashboardSidebar(
        sidebarMenu(
          textInput("myid", "Your ID (make it unique)"),
          textOutput("checkid"),
          actionButton("loadid", "Load"),
          actionButton("saveid", "Save"),
          tags$a(href="https://osf.io/ndzpt/", "OSF Page"),
          br(),
          tags$a(href="https://docs.google.com/spreadsheets/d/1G_SoWUquak6oD-b2L3L-XB7cF97UAGb8PPyVg3r0Lts/edit#gid=0", "Live Updating Schedule")
        )
    ),
    dashboardBody(
      p("Click on the menu if the sidebar is collapsed to save or load your saved sessions"),
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

read_row <- function(sips, row, loc) {
  hm <- sips[row, 2]
  
  locs1 <- sips[loc, 3:11] %>%
    unlist() %>%
    unname()
  
  locs2 <- sips[loc-1, 5:26] %>%
    unlist() %>%
    unname()

  locs <- c(locs1, locs2)
  
  sessions <- sips[row, 3:26] %>%
    unlist() %>%
    unname()
  
  valid_sessions <- sessions[sessions != "" & sessions != "EDGE OF THE WORLD. TURN BACK."] 
  valid_locs <- locs[sessions != "" & sessions != "EDGE OF THE WORLD. TURN BACK."]

  x <- purrr::map2(1:length(valid_sessions), valid_sessions, function(i, v) {
    id <- paste0("cb_", row, "_", i)
    fields <<- c(fields, id)
    checkboxInput(id, label = paste0(v, " (", valid_locs[i], ")"), width = "100%")
  })
  
  c(list(h3(hm)), x)
}



server <- function(input, output, session) {
  
  output$checkid <- renderText({
    if (input$myid == "") {
      shinyjs::disable("loadid")
      shinyjs::disable("saveid")
      return("No ID")
    } else if (file.exists(paste0("responses/", input$myid))) {
      shinyjs::enable("loadid")
      shinyjs::enable("saveid")
      return(paste(input$myid, "exists"))
    } else {
      shinyjs::disable("loadid")
      shinyjs::enable("saveid")
      return(paste(input$myid, "is available"))
    }
  })
  
  sips <- reactive({
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Loading Schedule", value = 0)
    on.exit(progress$close())
    progress$set(value = 0.5, detail = "running")
    
    
    readGoogleSheet(url="https://docs.google.com/spreadsheets/d/1G_SoWUquak6oD-b2L3L-XB7cF97UAGb8PPyVg3r0Lts/edit#gid=0")
  })
  
  observeEvent(input$saveid, {
    if (input$myid == "") return(FALSE)
    
    data <- sapply(fields, function(x) input[[x]])
    saveData(data, input$myid)
    showNotification("Saved")
    shinyjs::disable("loadid")
    shinyjs::enable("saveid")
  })
  
  observeEvent(input$loadid, {
    id <- isolate(input$myid)
    
    if (id == "") return(FALSE)
    if (!file.exists(paste0("responses/", id))) return(FALSE)
    
    data <- read.csv(paste0("responses/", id), stringsAsFactors = FALSE) %>% as.list()
    lapply(names(data), function(x) {
      updateCheckboxInput(session, x, value = (data[[x]]=="TRUE"))
    })
    
    output$checkid <- renderText({ paste("Loaded from", id) })
    
    print(data)
  })
  
  output$sunday1 <- renderUI({ read_row(sips(), 17, 12) })
  output$sunday2 <- renderUI({ read_row(sips(), 18, 12) })
  output$sunday3 <- renderUI({ read_row(sips(), 22, 12) })
  output$sunday4 <- renderUI({ read_row(sips(), 23, 12) })
  
  output$monday1 <- renderUI({ read_row(sips(), 42, 37) })
  output$monday2 <- renderUI({ read_row(sips(), 43, 37) })
  output$monday3 <- renderUI({ read_row(sips(), 45, 37) })
  output$monday4 <- renderUI({ read_row(sips(), 47, 37) })
  output$monday5 <- renderUI({ read_row(sips(), 49, 37) })
  output$monday6 <- renderUI({ read_row(sips(), 50, 37) })
  
  output$tuesday1 <- renderUI({ read_row(sips(), 70, 65) })
  output$tuesday2 <- renderUI({ read_row(sips(), 71, 65) })
  output$tuesday3 <- renderUI({ read_row(sips(), 73, 65) })
  output$tuesday4 <- renderUI({ read_row(sips(), 75, 65) })
  output$tuesday5 <- renderUI({ read_row(sips(), 77, 65) })
  output$tuesday6 <- renderUI({ read_row(sips(), 78, 65) })
  
  lapply(c("sunday1", "sunday2", "sunday3", "sunday4",
           "monday1", "monday2", "monday3", "monday4", "monday5", "monday6", 
           "tuesday1", "tuesday2", "tuesday3", "tuesday4", "tuesday5", "tuesday6"),
         function(x) outputOptions(output, x, suspendWhenHidden = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)
