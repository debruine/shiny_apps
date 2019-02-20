library(shiny)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("normal simulation: independent-samples t-test"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput("n", "Number of observations (n):", min = 0, max = 1000, value = 100, step = 10),
      sliderInput("mean1", "Mean 1:", min = -2, max = 2, value = 0, step = 0.05),
      sliderInput("mean2", "Mean 2:", min = -2, max = 2, value = 0, step = 0.05),
      #sliderInput("sd", "SD:", min = 1, max = 1, value = 1, step = 1),
      h4("SD = 1"),
      
      actionButton("flip", "Generate 1 Data Set"),
      actionButton("flip100", "Generate 1000 Data Sets"),
      actionButton("reset", "Reset")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Text ----
      h4("t.test(mean1, mean2)"),
      
      # Output: Density plot ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "pPlot")
      
    )
  )
)

# Global variables ----
allsim <- c()
allp <- c()

flip100 <- 0
reset_n <- 0
n <- 100
mean1 <- 0
mean2 <- 0
sd <- 1
ci <- c(0,0)
data <- c()

# Define server logic required to draw a plot ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    flip <- input$flip
    
    if (input$reset > reset_n
        | input$n != n
        | input$mean1 != mean1
        | input$mean2 != mean2
        # | input$sd != sd
    ) {
      reset_n <<- input$reset
      n <<- input$n
      mean1 <<- input$mean1
      mean2 <<- input$mean2
      # sd <<- input$sd
      allsim <<- c()
      allp <<- c()
    }
    
    repeats <- 1
    
    if (input$flip100 > flip100) {
      flip100 <<- input$flip100
      repeats <- 1000
    }
    
    normsim <- function() {
      data <<- data.frame(
        m1 = rnorm(n, mean1, sd), 
        m2 = rnorm(n, mean2, sd)
      )
      ttest <- t.test(data$m1, data$m2)
      allsim <<- c(allsim, mean(data$m1)-mean(data$m2))
      allp <<- c(allp, ttest$p.value)
      ci <<- c(round(ttest$conf.int[1], 3),
               round(ttest$conf.int[2], 3))
    }
    
    replicate(repeats, normsim())
    
    p_value <- round(allp[length(allp)], 3)
    ci <- paste0("95% CI = ", ci[1], " to ", ci[2])
    oprobText <- paste0("Effect Size = ", round(mean(data$m1)-mean(data$m2), 3))
    power <- round(mean(allp<.05)*100)
    
    output$pPlot <- renderPlot({
      ggplot() +
        geom_histogram(aes(allp), binwidth = 0.01, color = "black", fill="white", boundary = 0, closed = "left") +
        xlim(0, 1) +
        labs(title = paste(length(allp), "p-values:", power, "% power (at alpha = .05)"),
             x = "p-values")
    })
    
    ggplot(data) +
      geom_density(aes(m1), color = "black", fill="red", alpha = 0.5 ) +
      geom_vline(xintercept = mean(data$m1), color = "red") +
      
      geom_density(aes(m2), color = "black", fill="blue", alpha = 0.5 ) +
      geom_vline(xintercept = mean(data$m2), color = "blue") +
      
      xlim(min(mean1, mean2) - 4*sd, max(mean1, mean2) + 4*sd) +
      labs(title = paste("Most recent dataset: ", oprobText, ",", ci, ", p =", p_value))
    
  })
}

shinyApp(ui = ui, server = server)
