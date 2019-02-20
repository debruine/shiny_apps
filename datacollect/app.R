library(shiny)
library(ggplot2)

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("name", "used_shiny", "r_num_years")

saveData <- function(data) {
  # transpose data to wide format
  data <- t(data)
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.csv", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    data <- data.frame(
      name = character(),
      used_shiny = logical(),
      r_num_years = integer(),
      submit_time = as.Date(character())
    )
  } else {
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}

deleteData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  lapply(files, file.remove)
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Data Collection & Feedback"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        textInput("name", "Name", ""),
        checkboxInput("used_shiny", "I've built a Shiny app before", FALSE),
        sliderInput("r_num_years", "Number of years using R",
                    0, 10, 0, ticks = FALSE),
        actionButton("submit", "Submit"),
        downloadButton("downloadData", "Download"),
        actionButton("delete", "Delete All Data")
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        plotOutput(outputId = "yearsPlot"),
        tags$hr(),
        dataTableOutput("responses")
      )
    )
  ),
  
  server = function(input, output, session) {
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      data <- sapply(fields, function(x) input[[x]])
      data$submit_time <- date()
      saveData(data)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete, {
      deleteData()
    })
    
    # Show the previous responses in a reactive table ----
    output$responses <- renderDataTable({
      # update with current response when Submit or Delete are clicked
      input$submit 
      input$delete
      
      # reset values
      updateTextInput(session, "name", value = "")
      updateCheckboxInput(session, "used_shiny", value = FALSE)
      updateSliderInput(session, "r_num_years", value = 0)
      
      loadData()
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = "data.csv",
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
      }
    )
    
    output$yearsPlot <- renderPlot({
      input$submit
      input$delete
      
      data <- loadData()
      
      ggplot(data) +
        geom_histogram(
          aes(r_num_years), 
          binwidth = 1, 
          color = "black", 
          fill = "white"
        ) +
        scale_x_continuous(
          name = "Number of years using R", 
          breaks = 0:10,
          limits = c(-0.5, 10.5)
        ) + 
        theme_minimal() +
        theme(
          text = element_text(family = "Helvetica", size = 20),
          plot.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          axis.title.y = element_blank()
        )
    })
  }
)