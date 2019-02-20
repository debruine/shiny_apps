library(shiny)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("coin flip simulation: exact binomial test"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      sliderInput("n", "Number of flips (n):", min = 0, max = 100, value = 10, step = 1),
      sliderInput("p", "Probability of heads (p):", min = 0, max = 1, value = 0.5, step = 0.01),


      actionButton("flip", "Flip"),
      actionButton("flip100", "Flip 100x"),
      actionButton("reset", "Reset")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Text ----
      h4("binom.test(obs, n, p = 0.5)"),
      p(textOutput("coinText")),
      p(textOutput("oprobText")),
      p(textOutput("pText")),
      p(textOutput("ciText")),

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
n <- 10
p <- 0.5
ci <- c(0, 0)

# Define server logic required to draw a plot ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    flip <- input$flip

    if (input$reset > reset_n |
        input$n != n |
        input$p != p
       ) {
      reset_n <<- input$reset
      n <<- input$n
      p <<- input$p
      allsim <<- c()
      allp <<- c()
    }

    repeats <- 1

    if (input$flip100 > flip100) {
      flip100 <<- input$flip100
      repeats <- 100
    }

    flip_coin <- function() {
      coin <- rbinom(1, n, p)
      btest <- binom.test(coin, n, p = 0.5)
      allsim <<- c(allsim, coin)
      allp <<- c(allp, btest$p.value)
      ci <<- c(round(btest$conf.int[1], 3),
               round(btest$conf.int[2], 3))
    }

    replicate(repeats, flip_coin())

    output$pText <- renderText({
      paste0("P-value = ", round(allp[length(allp)], 3))
    })

    output$ciText <- renderText({
      paste0("95% CI = ", ci[1], " to ", ci[2])
    })

    output$oprobText <- renderText({
      paste0("Observed probability of heads = ", allsim[length(allsim)]/n)
    })

    output$coinText <- renderText({
      paste0("Observed number of heads = ", allsim[length(allsim)], "/", n)
    })

    output$pPlot <- renderPlot({
      ggplot() +
        geom_histogram(aes(allp), binwidth = 0.01, color = "black", fill="white", boundary = 0, closed = "left") +
        xlim(0, 1) +
        labs(title = paste(length(allp), "p-values"),
             x = "p-value")
    })

    ggplot() +
      geom_histogram(aes(allsim), binwidth = 1, color = "black", fill="white") +
      xlim(-0.5, n+0.5) +
      labs(title = paste("Observed Number of Heads from", length(allsim), "sets of", n, "flips"),
           x = "Heads")

  })
}

shinyApp(ui = ui, server = server)
