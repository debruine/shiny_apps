library(shiny)
library(shinydashboard)
library(rlang)
library(magrittr)

ui <- dashboardPage(
    dashboardHeader(title = "Arguments"),
    dashboardSidebar(
        sidebarMenu(
            textInput(inputId = "func", label = "Package::Function", value = "t.test"),
            actionButton(inputId = "goButton", label = "Get Arguments"),
            hr(),
            tableOutput("func_table"),
            h3("Available Packages"),
            tableOutput("packages")
        )
    ),
    dashboardBody(
        HTML("<p>This app is a demo to test getting the default arguments for a user-input function as inputs. <a href='https://github.com/debruine/shiny/tree/master/args/app.R'>Code for this app</a></p>"),
        fluidRow(
            box(width = 6, uiOutput("arg_input")),
            box(status = "primary", width = 6, htmlOutput("args"))
        )
    )
)

getHTMLhelp <- function(...){
    thefile <- help(...)
    capture.output(
        tools:::Rd2HTML(utils:::.getHelpFile(thefile))
    )
}

isValidName <- function(string) {
    grepl("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$", string)
}

server <- function(input, output) {
    output$func_table <- renderTable({
        func_text <- input$func
        
        if (grepl("^[._[:alnum:]]+::[._[:alnum:]]*$", func_text)) {
            split_func <- strsplit(func_text, "::")[[1]]
            expts <- getNamespaceExports(split_func[1])
            if (length(split_func) == 1) { split_func[2] <- "" }
            df <- data.frame(
                funcs = expts[grep(split_func[2], expts)] %>% sort()
            )
            names(df) <- split_func[1]
            df
        } else {
            data.frame()
        }
    })
    
    arglist <- reactive({
        input$goButton
        func_text <- isolate(input$func) %>% 
            gsub("[^a-zA-Z0-9_\\:.]", "", .)
        func <- parse(text = func_text) %>% eval()
        
        fml <- list()
        fml[["main"]] <- formals(func)
        
        if (isS3stdGeneric(func)) {
            m3 <- tryCatch(.S3methods(func), # ?!?!?!?!?!?
              error = function(e) return(list()))
            
            for (m in m3) {
                cl <- gsub(paste0(func_text, "."), "", m, fixed = TRUE)
                func3 <- getS3method(func_text, class =  cl)
                fml[[cl]] <- formals(func3)
            }
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
        tp <- lapply(names(arglist), function(a) {
            args <- arglist[[a]]
            arg_inputs <- lapply(names(args), function(arg) {
                editable <- TRUE
                if (is.symbol(args[[arg]])) {
                    val <- ""
                } else {
                    val <- tryCatch({
                            v <- eval(args[[arg]])
                            if (!is.vector(v) & !is.null(v)) {
                                v <- capture.output(args[[arg]])
                            }
                            v
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
        })
        
        if (length(tp) > 1) {
            do.call(tabsetPanel, tp)
        } else {
            tp
        }
    })

    output$args <- renderUI({
        input$goButton
        func_text <- isolate(input$func) %>% 
            gsub("[^a-zA-Z0-9_\\:.]", "", .)
        
        split_func <- strsplit(func_text, "::")[[1]]
        if (length(split_func) == 1) {
            h <- getHTMLhelp(split_func)
        } else {
            h <- getHTMLhelp(split_func[2], package = split_func[1])
        }
        
        get_rows <- c()
        
        startA <- grep("<h3>Arguments</h3>", h)
        if (length(startA) == 1) {
            table_ends <- grep("</table>", h)
            endA <- table_ends[table_ends>startA] %>% min()
            get_rows <- startA:endA
        }
        
        startD <- grep("<h3>Description</h3>", h) + 1
        if (length(startD) == 1) {
            h3_ends <- grep("</h3>", h)
            endD <- h3_ends[h3_ends>startD] %>% min() - 1
            get_rows <- c(startD:endD, get_rows)
        }
        
        paste(h[get_rows], collapse = "\n") %>% 
            paste0("<h2>", func_text, "</h2>\n", .) %>%
            HTML()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
