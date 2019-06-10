library(shiny)
library(shinydashboard)
library(magrittr)

ui <- dashboardPage(
    dashboardHeader(title = "Arguments"),
    dashboardSidebar(
        sidebarMenu(
            textInput(inputId = "func", label = "Function", value = "t.test"),
            actionButton(inputId = "goButton", label = "Get Arguments"),
            hr(),
            tags$a(href="https://github.com/debruine/shiny/tree/master/args/app.R", "Code for this app"),
            h3("Available Packages"),
            tableOutput("packages")
        )
    ),
    dashboardBody(
        p("This app is a demo to see how to get the default arguments for a user-input function."),
        fluidRow(
            box(width = 6, uiOutput("arg_input")),
            box(width = 6, htmlOutput("args"))
        )
    )
)

getHTMLhelp <- function(...){
    thefile <- help(...)
    capture.output(
        tools:::Rd2HTML(utils:::.getHelpFile(thefile))
    )
}

server <- function(input, output) {
    arglist <- reactive({
        input$goButton
        func_text <- isolate(input$func) 
        func <- parse(text = func_text) %>% eval()
        
        fml <- list()
        fml[["generic"]] <- formals(func)
        
        m3 <- .S3methods(func)
        for (m in m3) {
            cl <- gsub(paste0(func_text, "."), "", m, fixed = TRUE)
            func3 <- getS3method(func_text, class =  cl)
            fml[[cl]] <- formals(func3)
        }

        fml
    })
    
    output$packages <- renderTable({
        p <- installed.packages()
        
        data.frame("package" = p[, 1],
                   "version" = p[, 3])
    })
    
    output$arg_input <- renderUI({
        arglist <- arglist()
        lapply(names(arglist), function(a) {
            args <- arglist[[a]]
            arg_inputs <- lapply(names(args), function(arg) {
                editable <- TRUE
                if (is.symbol(args[[arg]])) {
                    val <- ""
                } else {
                    val <- tryCatch({
                            a <- eval(args[[arg]])
                            if (!is.vector(a) & !is.null(a)) {
                                a <- capture.output(args[[arg]])
                            }
                            a
                        },
                        error = function(e) {
                            capture.output(args[[arg]])
                        }
                    )
                }
                
                if (!editable) {
                    HTML(paste("<div class='form-group'><label>", 
                               arg, "</label> =", val, "</div>"))
                } else if (length(val) > 1) {
                    selectInput(arg, arg, choices = val, selected = val[1])
                } else {
                    textInput(arg, arg, value = val)
                }
            })
            
            tabPanel(a, arg_inputs)
        }) -> tp
        
        if (length(tp) > 1) {
            do.call(tabsetPanel, tp)
        } else {
            tp
        }
    })

    output$args <- renderUI({
        input$goButton
        func <- isolate(input$func)
        split_func <- strsplit(func, "::")[[1]]
        if (length(split_func) == 1) {
            h <- getHTMLhelp(split_func)
        } else {
            h <- getHTMLhelp(split_func[2], package = split_func[1])
        }
        
        start <- grep("<h3>Arguments</h3>", h)
        table_ends <- grep("</table>", h)
        end <- table_ends[table_ends>start] %>% min()
        
        paste(h[start:end], collapse = "\n") %>% HTML()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
