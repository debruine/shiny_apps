library(shiny)
library(ggplot2)

outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

# Define the fields we want to save from the form
fields <- c("name", "r_expertise", "tidyverse", "my_res", "my_learn", 
            "my_ugt", "my_pgt", "dept_ugt", "dept_pgt", 
            "plan_ugt", "plan_pgt", "hope_get")

# Shiny app with fields that the user can submit data for
shinyApp(
  ui <- fluidPage(title = "Teaching R Survey",
    # CSS ----
    # stop the default input containers being 300px, which is ugly
    tags$head(
      tags$style(HTML("
        .shiny-input-container:not(.shiny-input-container-inline) {
          width: 100%;
          max-width: 100%;
        }
      "))
    ),
    
    # App title ----
    h3("Teaching Reproducible Data Analysis in R: Survey"),

    p("Please fill out the following brief survey to help us tailor the workshop to your needs and experience."),

    fluidRow(
      column(width=6,
         textInput("name", "Name", "")
      ),
      column(width=6,
        selectInput("r_expertise", "My experience with R", c("", "None", "Beginner", "Intermediate", "Expert"))
      )
    ),
    
    fluidRow(
      column(width=6,
        checkboxInput("tidyverse", "I am familiar with the tidyverse", FALSE),
        checkboxInput("my_res", "I use R in my own research", FALSE),
        checkboxInput("my_learn", "I want to learn to use R in my own research", FALSE),
        checkboxInput("my_ugt", "I use R in my undergraduate teaching", FALSE),
        checkboxInput("my_pgt", "I use R in my postgraduate teaching/supervision", FALSE)
      ),
      column(width=6,
        checkboxInput("dept_ugt", "My department currently uses R for undergraduate teaching", FALSE),
        checkboxInput("dept_pgt", "My department currently uses R for postgraduate teaching", FALSE),
        checkboxInput("plan_ugt", "My department plans to use R for undergraduate teaching", FALSE),
        checkboxInput("plan_pgt", "My department plans to use R for postgraduate teaching", FALSE)
      )
    ),
    
    textAreaInput("hope_get", "What do you hope to get out of this workshop?", "", width = "100%", rows = 3), 
    
    actionButton("submit", "Submit")
    #downloadButton("downloadData", "Test Download")
    #actionButton("delete", "Delete All Data")
  ),
  
  server = function(input, output, session) {
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      data <- sapply(fields, function(x) input[[x]])
      data$submit_time <- date()
      saveData(data)
      
      updateTextInput(session, "name", value = "")
      updateSelectInput(session, "r_expertise", selected = "")
      
      updateCheckboxInput(session, "tidyverse", value = FALSE)
      updateCheckboxInput(session, "my_res", value = FALSE)
      updateCheckboxInput(session, "my_learn", value = FALSE)
      updateCheckboxInput(session, "my_ugt", value = FALSE)
      updateCheckboxInput(session, "my_pgt", value = FALSE)
      
      updateCheckboxInput(session, "dept_ugt", value = FALSE)
      updateCheckboxInput(session, "dept_pgt", value = FALSE)
      updateCheckboxInput(session, "plan_ugt", value = FALSE)
      updateCheckboxInput(session, "plan_pgt", value = FALSE)
      
      updateTextAreaInput(session, "hope_get", value = "")
      
      showNotification("Thank you for completing the survey!", duration = 0, type = "message")
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = "data.csv",
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
      }
    )
  }
)