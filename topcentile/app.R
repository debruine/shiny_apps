library(shiny)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Percentile Estimation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Specify the mean of group 1 ----
      numericInput("m1", "Mean of Group 1:", 100),
      
      # Input: Specify the sd of group 1 ----
      numericInput("sd1", "SD of Group 1:", 11),
      
      # Input: Specify the mean of group 2 ----
      numericInput("m2", "Mean of Group 2:", 100),
      
      # Input: Specify the sd of group 2 ----
      numericInput("sd2", "SD of Group 2:", 10),
      
      # Input: Specify the percentile to compare ----
      numericInput("percentile", "Percentile:", 99),
      #sliderInput("percentile", "Percentile:", min = 0, max = 100, step = 0.1, value = 99)
      
      actionButton("reset", "Reset")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Density plot ----
      plotOutput(outputId = "distPlot"),
      
      # Output: Text ----
      h4(textOutput("distText"), align = "center"),
      h4(textOutput("aboveText"), align = "center"),
      h4(textOutput("ratioText"), align = "center")
      
    )
  )
)

# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    reset <- input$reset
    
    # sample from input distributions ----
    n <- 100000
    g1 <- rnorm(n, input$m1, input$sd1)
    g2 <- rnorm(n, input$m2, input$sd2)
    
    my_data <- data.frame(
      grp = rep(c("1", "2"), each = n),
      var = c(g1, g2)
    )
    
    # calculate percentile cutoff value ----
    my_quantile <- (input$percentile)/100
    q_cutoff <- quantile(my_data$var, probs = c(my_quantile))
    
    # calulate percent of population above cutoff and ratio ----
    g1_above <- round(100*sum(g1 > q_cutoff)/n, 2)
    g2_above <- round(100*sum(g2 > q_cutoff)/n, 2)
    g1_g2_ratio <- round(sum(g1 > q_cutoff)/sum(g2 > q_cutoff), 2)
    
    output$distText <- renderText({
      paste0(g1_above, "% of Group 1 and ", g2_above, "% of Group 2")
    })
    
    output$aboveText <- renderText({
      char_percentile <- as.character(input$percentile)
      right_digit <- substr(char_percentile, 
                            nchar(char_percentile), 
                            nchar(char_percentile))
      
      nth <- "th"
      nth <- ifelse(right_digit == "1", "st", nth)
      nth <- ifelse(right_digit == "2", "nd", nth)
      nth <- ifelse(right_digit == "3", "rd", nth)
      
      paste0("are above the combined ", input$percentile, nth,
             " percentile (", round(q_cutoff, 2), ")")
    })
    
    output$ratioText <- renderText({
      paste0("The ratio is ", g1_g2_ratio, ":1")
    })
    
    # generate density plot ----
    ggplot(my_data, aes(var, fill = grp)) + 
      geom_density(alpha = 0.5) +
      geom_vline(xintercept = q_cutoff) +
      guides(fill=guide_legend(title="Group"))
    
  })

}

shinyApp(ui = ui, server = server)