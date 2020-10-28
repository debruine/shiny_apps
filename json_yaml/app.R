## app.R ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(yaml)
  library(jsonlite)
  library(dplyr)
})

## functions ----

debug_msg <- function(txt) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if (is_local) {
    message(txt)
  } else {
    shinyjs::logjs(txt)
  }
}


drop_empty <- function(x) {
  for (i in rev(seq_along(x))) {
    y <- x[[i]]
    if (is.list(y)) {
      if (length(y) == 0) {
        x[[i]] <- NULL
      } else {
        x[[i]] <- drop_empty(y)
      }
    } else if (is.null(y)) {
      x[[i]] <- NULL
    }
  }
  return(x)
}

m2j <- function(m) {
  jsonlite::toJSON(m, auto_unbox = TRUE) %>%
    jsonlite::prettify(4) %>%
    paste(collapse = "\n")
}

## UI ----
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "JSON/YAML"),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file", "Load from File", width = "100%"),
      
      actionButton("example", "Load Example"),
      actionButton("prettify", "Prettify JSON"),
      actionButton("drop_empty", "Drop Empty Values"),
      radioButtons("type", NULL,
                   c("text" = "txt",
                     "JSON" = "json",
                     "YAML" = "yaml"),
                   selected = "txt"),
      HTML("<hr><ul style='margin-left:1px;'><li><a href='https://github.com/debruine/shiny/blob/master/json_yaml/app.R'>App Code</a></li><li><a href='https://www.json2yaml.com/'>Proper JSON/YAML app</a></li></ul>")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$style(HTML("
    .warning { background-color: #f2f0a2; }
    #text { font-family: monospace; }
    ")),
    
    box(width = 12,
      textAreaInput("text", NULL, "", "100%", "400px"),
      verbatimTextOutput("err")
    )
  )
)


## server ----
server <- function(input, output, session) {
  methods <- reactiveVal(list())
  
  # . . example ----
  observeEvent(input$example, {
    m <- list(
      title = "JSON/YAML",
      year = 2020,
      doi = NULL,
      author = list(
        orcid =  "0000-0002-7523-5539",
        surname =  "DeBruine",
        given = "Lisa M.",
        roles = c("Software", "Conceptualization"),
        email = "lisa.debruine@glasgow.ac.uk"
      )
    )
    
    methods(m)
        
    updateTextAreaInput(session, "text", value = "")
    updateRadioButtons(session, "type", selected = "json")
    updateTextAreaInput(session, "text", value = m2j(m))
  }, ignoreNULL = TRUE)
  
  # . . file ----
  observeEvent(input$file, {
    debug_msg("file")
    
    path <- input$file$datapath
    ext <- tools::file_ext(path) %>% tolower()
    
    txt <- tryCatch({
      readLines(path) %>% paste(collapse = "\n")
    }, error = function(e) {
      shinyjs::alert(e$message)
      return("")
    })
    
    updateTextAreaInput(session, "text", value = "")
    if (ext == "json") {
      updateRadioButtons(session, "type", selected = "json")
    } else if (ext == "yml") {
      updateRadioButtons(session, "type", selected = "yml")
    } else {
      updateRadioButtons(session, "type", selected = "text")
    }
    updateTextAreaInput(session, "text", value = txt)
  }, ignoreNULL = TRUE)
  
  # . . type ----
  observeEvent(input$type, {
    debug_msg("type")
    
    if (trimws(input$text) == "") return()
    
    if (input$type == "yaml") {
      # message("try to convert from JSON to YAML")
      tryCatch({
        m <- jsonlite::fromJSON(input$text, TRUE)
        y <- yaml::as.yaml(m)
        updateTextAreaInput(session, "text", value = y)
      })
    } else if (input$type == "json") {
      # message("try to convert from YAML to JSON")
      tryCatch({
        m <- yaml::yaml.load(input$text)
        j <- m2j(m)
        updateTextAreaInput(session, "text", value = j)
      })
    }
  })
  
  # . . text ----
  observeEvent(input$text, {
    debug_msg("text")
    
    if (input$text == "") { return() }
    
    tryCatch({
      if (input$type == "json") {
        err <- jsonlite::validate(input$text) %>%
          attr("err")
        if (!is.null(err)) stop(err, "")
        
        m <- jsonlite::fromJSON(input$text, TRUE)
        v <- "valid JSON"
      } else if (input$type == "yaml") {
        m <- yaml::yaml.load(input$text)
        v <- "valid YAML"
      } else {
        m <- strsplit(input$text, "\n")[[1]]
        v <- ""
      }
      shinyjs::removeClass('err', 'warning')
      output$err <- renderText(v)
      
      # update methods
      methods(m)
    }, error = function(e) {
      output$err <- renderText(e$message)
      shinyjs::addClass('err', 'warning')
    })
  })
  
  # . . prettify ----
  observeEvent(input$prettify, {
    debug_msg("prettify")
    if (input$type == "json") {
      tryCatch({
        txt <- input$text %>%
          jsonlite::fromJSON(TRUE) %>%
          m2j()
        
        updateTextAreaInput(session, "text", value = txt)
      }, error = function(e) {})
    }
  }, ignoreNULL = TRUE)
  
  # . . drop_empty ----
  observeEvent(input$drop_empty, {
    m <- methods() %>% drop_empty()
    methods(m)
    
    if (input$type == "yaml") {
      txt <- yaml::as.yaml(m)
    } else if (input$type == "json") {
      txt <- m2j(m)
    } else {
      txt <- unlist(m) %>%
        paste(collapse = "\n")
    }
    
    updateTextAreaInput(session, "text", value = txt)
  }, ignoreNULL = TRUE)
  
} # end server()

shinyApp(ui, server)