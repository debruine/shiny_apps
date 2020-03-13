## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)

## Functions ----

## Interface Tab Items ----

midway_tab <- tabItem(
  tabName = "midway_tab",
  p("Set effect size to 0 to calculate the false positive rate. The minimum cutoff sets the smallest effect size for which you'd continue the study."),
  valueBoxOutput("n")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Midway Power"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("midway", tabName = "midway_tab")
    ),
    numericInput("n_first", "Observations before power analysis", 
                min = 10, max = 100, value = 10, step = 1),
    numericInput("es", "Effect size", 
                 min = 0, max = 1, value = 0, step = .01),
    numericInput("reps", "Simulation reps", 
                 min = 100, max = 10000, value = 100, step = 100),
    numericInput("min_cutoff", "Minimum cutoff", 
                 min = 0, max = 3.0, value = .1, step = .01),
    numericInput("alpha", "Alpha", 
                 min = 0.001, max = 1.0, value = .05, step = .001),
    numericInput("power", "Power", 
                 min = 0.1, max = 1.0, value = .8, step = .05)
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      midway_tab
    )
  )
)

## server ----
server <- function(input, output, session) {

  output$n <- renderValueBox({
    fp <- purrr::map_dbl(1:input$reps, function(i) {
      dat <- rnorm(input$n_first, input$es)
      obs_d <- mean(dat)/sd(dat)
      if (obs_d >= input$min_cutoff) {
        power <- pwr::pwr.t.test(
          d = obs_d, 
          sig.level = 0.05, 
          power = input$power, 
          type = "one.sample"
        )
       
        additional_n <- power$n - input$n_first 
        if (additional_n < 0) additional_n = 0
        
        p <- purrr::map_dbl(1:input$reps, function(i) {
          dat <- c(dat, rnorm(additional_n))
          t.test(dat)$p.value
        })
        
        mean(p < input$alpha)
      } else {
        # give up is observed effect size less than cutoff
        NA
      }
    })
    
    fpr <- round(mean(fp, na.rm = TRUE), 2)
    label <- ifelse(input$es == 0, "False Positive Rate", "Power")
    valueBox(fpr, label, color = "red")
  })
  
} # end server()

shinyApp(ui, server)
