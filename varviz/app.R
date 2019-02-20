library(shiny)
library(ggplot2)

# Define UI for app that draws a plot of simulated data from a normal distribution ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Va-Va-Variance!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      numericInput("n", "Number of Subjects:", 100),
      sliderInput("m", "Mean:", min = -10, max = 10, value = 0, step = 1),
      sliderInput("sd", "SD:", min = 0, max = 10, value = 5, step = 1),
      #numericInput("m", "Mean:", 0),
      #numericInput("sd", "SD:", 1)
      checkboxGroupInput("view", "View:",
                         c("points" = "points",
                           "violin" = "violin",
                           "lines" = "lines"),
                         selected = c("points", "violin", "lines")
                         ),
      actionButton("reset", "Reset")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Scatter plot ----
      
      plotOutput(outputId = "scatterPlot", height = "auto"),
      p("Purple line = sample mean; grey lines = ± 0, 1, 2 & 3 SDs")
    )
  )
)

# Define server logic required to draw a plot ----
server <- function(input, output, session) {
  output$scatterPlot <- renderPlot({
    reset <- input$reset
    
    # input error handling
    my_n <- ifelse(is.numeric(input$n), max(1, round(input$n)), 100)
    m <- ifelse(is.numeric(input$m), input$m, 0)
    sd <- ifelse(is.numeric(input$sd), input$sd, 5)
    
    data <- data.frame(
      n = 1:my_n,
      dv = rnorm(my_n, m, sd)
    )
    
    cols <- c("mean" = "purple", "±1SD" = "blue", "±2SD" = "darkgreen", "±3SD" = "goldenrod")
    
    g <- ggplot(data)
    
    if ("lines" %in% input$view) {
      g <- g + 
        geom_hline(color = "grey20", yintercept = m) +
        geom_hline(color = "grey40", yintercept = m - sd) +
        geom_hline(color = "grey40", yintercept = m + sd) +
        geom_hline(color = "grey60", yintercept = m - 2*sd) +
        geom_hline(color = "grey60", yintercept = m + 2*sd) +
        geom_hline(color = "grey80", yintercept = m - 3*sd) +
        geom_hline(color = "grey80", yintercept = m + 3*sd)
    }
    
    if ("points" %in% input$view) {
      g <- g + geom_point(aes(n, dv), alpha = 1/log(my_n+2))
    }
    
    if ("violin" %in% input$view) {
      g <- g + geom_violin(aes(my_n/2, dv), width = my_n/2, fill = "purple", alpha = 0.25) +
        geom_hline(color = "purple", yintercept = mean(data$dv), alpha = 0.5)
    }
    
    
    g + scale_colour_manual(name="Lines",values=cols) + 
      theme(legend.position="bottom") +
      coord_cartesian(ylim = c(-50, 50)) +
      labs(
        x = "Participant Number"
      )
    
  }, height = function() {
    session$clientData$output_scatterPlot_width
  })
}

shinyApp(ui = ui, server = server)
