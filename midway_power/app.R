## app.R ##
library(shiny)
library(shinydashboard)

## Functions ----

## Interface Tab Items ----

midway_tab <- tabItem(
  tabName = "midway_tab",
  p("This app simulates the situation where you run studies and run a preliminary analysis after the first N observations. If the effect size at that point is smaller than the minimum cutoff, you scrap the study. If it is larger, you run a power calculation using this estimated effect size to determine how many more observations to collect to get the specified power for the specified alpha. The values below show the power of this strategy for sspecified parameters. Set effect size to 0 to calculate the false positive rate (which is always inflated)."),
  valueBoxOutput("scrapped"),
  valueBoxOutput("nn"),
  valueBoxOutput("fpr"),
  plotOutput("plot")
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
                 min = 100, max = 1000, value = 100, step = 100),
    numericInput("min_cutoff", "Minimum cutoff", 
                 min = 0, max = 3.0, value = .1, step = .01),
    numericInput("alpha", "Alpha", 
                 min = 0.001, max = 1.0, value = .05, step = .001),
    numericInput("power", "Power", 
                 min = 0.1, max = 1.0, value = .8, step = .05),
    radioButtons("alternative", "Tails", c("one.sided", "two.sided"))
  ),
  dashboardBody(
    tabItems(
      midway_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  dat <- reactive({
    purrr::map_dfr(1:input$reps, ~{
      # simulate data from population with specific effect size
      dat <- rnorm(input$n_first, input$es)
      
      # get observed effect size
      obs_d <- mean(dat)/sd(dat)
      if (input$alternative == "two.sided") {
        obs_d2 <- abs(obs_d)
      } else {
        obs_d2 <- obs_d
      }
      
      # only if observed ES > cutoff
      if (obs_d2 >= input$min_cutoff) {
        # get required N to power observed ES
        power <- power.t.test(
          delta = obs_d2, 
          sig.level = 0.05, 
          power = input$power, 
          type = "one.sample",
          alternative = input$alternative
        )
       
        # calculate how many more subjects needed to power for observed ES
        additional_n <- power$n - input$n_first 
        if (additional_n < 0) additional_n = 0
        
        # add additional subjects to first subjects and get p-values of result
        p <- replicate(input$reps, {
          dat <- c(dat, rnorm(additional_n, input$es))
          alt <- input$alternative
          if (alt == "one.sided") {
            alt <- ifelse(obs_d < 0, "less", "greater")
          }
          t.test(dat, alternative = alt)$p.value
        })
        
        # return 
        list(
          scrapped = "no",
          obs_d = obs_d,
          power = mean(p < input$alpha), 
          n = power$n
        )
      } else {
        # give up is observed effect size less than cutoff
        list(
          scrapped = "yes",
          obs_d = obs_d,
          power = NA,
          n = NA
        )
      }
    })
  })
    

  output$nn <- renderValueBox({
    nn <- round(mean(dat()$n, na.rm = TRUE))
    valueBox(nn, "Mean Total N", color = "purple") 
  })
  
  output$fpr <- renderValueBox({
    fpr <- round(mean(dat()$power, na.rm = TRUE), 2)
    label <- ifelse(input$es == 0, "False Positive Rate", "Power")
    valueBox(fpr, label, color = "green") 
  })
  
  output$scrapped <- renderValueBox({
    scrapped <- round(100*mean(dat()$scrapped == "yes"))
    valueBox(scrapped, "Percent studies scrapped", color = "blue") 
  })
  
  output$plot <- renderPlot({
    ggplot(dat(), aes(x = obs_d, fill = scrapped)) +
      geom_histogram(binwidth = 0.05, color = "grey") +
      geom_vline(xintercept = input$min_cutoff) +
      scale_fill_manual(values = c("red", "#0073B7")) +
      xlab(paste0("Observed effect size after first ", 
                  input$n_first, " observations"))
  })
  
} # end server()

shinyApp(ui, server)
