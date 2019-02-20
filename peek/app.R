library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

peek <- function(start_n, max_n, by = 1, alpha = 0.05, dist = "normal") {
  # set up null effect for max n
  if (dist == "normal") {
    a <- rnorm(max_n) 
  }
  
  peeks <- seq(start_n, max_n, by)
  
  for (n in peeks) {
    t <- t.test(a[1:n], mu = 0)
    # return n for first sig p-value
    if (t$p.value < alpha) return(n) 
  }
  
  0 # if none sig by end
}

# ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Peek"),
  dashboardSidebar(
    useShinyjs(),
    sliderInput("n", "Observations at first and last peek", 
                    min = 10, max = 100, step = 10, value = c(10, 50)),
    sliderInput("by", "New observations before next peek", 
                    min = 1, max = 10, step = 1, value = 5),
    sliderInput("alpha", "Significant p-value (alpha)", 
                min = .001, max = .10, step = .001, value = .05),
    sliderInput("reps", "Number of runs in this simulation", 
                    min = 100, max = 1000, step = 100, value = 100)
  ),
  dashboardBody(
    p("Just how bad is it to peek at your data every few observations and stop 
      collecting data once you have a significant result? 
      Simulate the false positive rate below. 
      You can change the parameters in the sidebar (in the menu if minimised)."),
    plotOutput("plot", height = 400),
    actionButton("rerun", "Re-run Simulation"),
    p(id="intro", "This simulated a normal distribution with no effect and ran a 
      1-sample t-test on the first 10 observations, then collected 5 new 
      observations and peeked again until 50 observations. This procedure was run 
      100 times and reported the percent of runs that produced a significant result 
      (p < .05) and plotted the N at which the first significant result was found."),
    p("There are several papers and blogs discussing this issue:"),
    tags$ul(
      tags$li(a(href="https://t.co/51fDX07hQr", 
      "Performing High-Powered Studies Efficiently With Sequential Analyses; Daniël Lakens")),
      tags$li(a(href="https://neuroneurotic.net/2016/08/25/realistic-data-peeking-isnt-as-bad-as-you-thought-its-worse/",
      "Realistic data peeking isn’t as bad as you* thought – it’s worse; Sam Schwarzkopf"))
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    input$rerun
    input$n
    input$by
    input$reps
    input$alpha
    
    r <- replicate(input$reps, peek(input$n[1], input$n[2], input$by, input$alpha))
    
    pcnt_sig <- round(100*sum(r > 0)/input$reps, 1)
    
    html("intro", paste0("This simulated a normal distribution with with no effect and ran a 
      1-sample t-test on the first ",input$n[1]," observations, then collected ",input$by," new 
      observations and peeked again until ",input$n[2]," observations. This procedure was run 
      ",input$reps," times and reported the percent of runs that produced a significant result 
      (p < ",input$alpha,")  and plotted the N at which the first significant result was found."))

    ggplot() +
      geom_histogram(aes(r, y=..density..), binwidth = 1,
                     color = "black", fill="red") +
      coord_cartesian(xlim = c(input$n[1]-0.5, input$n[2]+0.5), ylim = c(0, 0.1)) +
      theme_minimal() +
      labs(x = "Number of observations at first significant p-value",
           title = paste0(pcnt_sig, "% of ", input$reps, " runs obtained a significant result"))

  })
}

shinyApp(ui, server)