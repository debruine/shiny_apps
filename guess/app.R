# Libraries ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define UI ----

# . Sidebar ----
sidebar <- dashboardSidebar(
  useShinyjs(), #add useShinyjs to be able to disable buttons upon making a choice.
  sidebarMenu(
    menuItem("Guess", tabName = "main_tab"),
    menuItem("Data", tabName = "data_tab")
  ),
  tags$a("Code on GitHub", href = "https://github.com/debruine/shiny/blob/master/guess/app.R"),
  h4("Display Options"),
  checkboxInput("show_violin", "Violin Plot",value = T),
  checkboxInput("show_boxplot", "BoxPlot",value = F),
  checkboxInput("show_points", "Points",value = T),
  sliderInput("n_obs", "Observations per group", min = 1, max = 100, value = 1, step = 1),
  checkboxInput("one_two", "One at a time",value = T),
  checkboxInput("trinary", "Trinary Input",value = F),
  sliderInput("prob_null", "Null probability", min = 0, max = 100, value = 50, step = 5),
  p("This app is not storing your data beyond this session.")
)


main_tab <- tabItem(
  tabName = "main_tab",
  p("This app will show you a graph of simulated data with a random number of observations in each of two groups and a random effect size. The effect size will be between -3 and 3, your job is to guess the size and direction of the effect."),
  fluidRow(
    column(
      width = 6,
      box(
        width = NULL,
        h4("How much larger is group B (blue circle) than group A (red square)?"),
        # . . d_guess slide ----
        sliderInput("d_guess", "My effect size (d) guess", 
                    min = -3, max = 3, value = 0, step = 0.05),
        radioButtons("dir_guess", "My effect direction guess", 
                     choices = c("A larger" = "A", 
                                 "A and B same" = "0", 
                                 "B larger" = "B"), 
                     inline = TRUE),
        # . . sample_again button ----
        actionButton("sample_again", "Sample Again"),
        # . . submit_guess button ----
        actionButton("submit_guess", "Submit Guess"),
        # . . new_sim button ----
        actionButton("new_sim", "Simulate a new dataset")
      )
    ),
    column(
      width = 6,
      # . . values output ----
      valueBoxOutput("samplesBox"),
      valueBoxOutput("guessBox"),
      valueBoxOutput("esBox")
    )
  ),
  fluidRow( # start row 1
    column( # start column 1
      width = 6,
      box(
        width = NULL,
        # . . current_n output ----
        textOutput("current_n"),
        # . . current_plot plot ----
        plotOutput("current_plot")
      )
    ), # end column 1
    column( # start column 2
      width = 6,
      box(
        title = "Your guessing performance",
        width = NULL,
        # . . performance_plot plot ----
        plotOutput("performance_plot")
      )
    )
  )
)

data_tab <- tabItem(
  tabName = "data_tab",
  # . . data_table ----
  dataTableOutput("data_table")
)

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "Guess"),
  sidebar,
  dashboardBody(
    tabItems(
      main_tab,
      data_tab
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  disable("new_sim")
  disable("n_obs")
  
  # Set reactiveValues ----
  app_vals <- reactiveValues(
    data = data.frame(direction = character(),
                      guess = double(),
                      real = double(),
                      samples = integer(),
                      n_obs = integer(),
                      violin = logical(),
                      boxplot = logical(),
                      points = logical(),
                      trinary = logical(),
                      one_two = logical()
    ),
    n = 20,
    offset = 0,
    es = 0,
    es_show = "?",
    sample_n = 0
  )
  
  observe({
    if (input$trinary) {
      show("dir_guess")
      hide("d_guess")
    } else {
      hide("dir_guess")
      show("d_guess")
    }
  })
  
  # . New simulation ----
  observeEvent(input$new_sim, {
    enable("submit_guess")
    enable("sample_again")
    disable("new_sim")
    enable("d_guess")
    enable("dir_guess")
    disable("n_obs")
    
    # reset slider
    updateSliderInput(session, "d_guess", value = 0)
    
    # set N
    app_vals$n <- input$n_obs
    
    # set sample effect size
    app_vals$es <- (rnorm(1, 0, 1) %>% 
                      pmax(-3) %>% 
                      pmin(3) * 2
    ) %>% 
      round(1) / 2
    
    # null effects 50% of the time
    pn <- input$prob_null/100
    app_vals$es <- sample(c(0, app_vals$es), 1, prob = c(pn, 1-pn))
    
    # set offset (so one group isn't always at 0)
    app_vals$offset <- sample(seq(-1,1,by = 0.1), 1)
    
    app_vals$sample_n <- 0
    click("sample_again")
    
  }, ignoreNULL = FALSE)


  # . Sample again ----
  simdata <- eventReactive(input$sample_again, {
    app_vals$es_show <- "?"
    
    app_vals$sample_n <- app_vals$sample_n + 1
    
    # simulate data
    A <-  rnorm(app_vals$n, app_vals$offset, 1)
    B <- rnorm(app_vals$n, app_vals$offset + app_vals$es, 1)
    dat <- data.frame(
      group = rep(c("A", "B"), each = app_vals$n),
      val = c(A, B)
    )
    
    dat$group <- factor(dat$group, levels = c("A", "B"))

    return(dat)
  }, ignoreNULL = FALSE)
  
  # . valueBoxes ----
  output$samplesBox <- renderValueBox({
    valueBox(app_vals$sample_n, "Samples",color = "red")
  })
  output$guessBox <- renderValueBox({
    valueBox(ifelse(input$trinary, input$dir_guess, input$d_guess), 
             "Your Guess",color = "purple")
  })
  output$esBox <- renderValueBox({
    valueBox(app_vals$es_show, "Effect Size",color = "blue")
  })

  # . Show current_n ----
  output$current_n <- renderText(
    paste0("The graph below shows ",
           app_vals$n,
           " observations in each group.")
  )

  # . Save guess ----
  save_guess <- eventReactive(input$submit_guess, {
    disable("submit_guess")
    enable("new_sim")
    disable("sample_again")
    disable("d_guess")
    disable("dir_guess")
    enable("n_obs")
    
    app_vals$es_show <- paste0(case_when(
      app_vals$es < 0 ~ "A",
      app_vals$es == 0 ~ "0",
      app_vals$es > 0 ~ "B"
    ), ": ", app_vals$es)
    
    if (input$trinary) {
      direction <- input$dir_guess
      guess <- NA
    } else {
      direction <- case_when(
        input$d_guess < 0 ~ "A",
        input$d_guess == 0 ~ "0",
        input$d_guess > 0 ~ "B"
      )
      guess <- input$d_guess
    }
    
    app_vals$data <- app_vals$data %>%
      add_row(direction = direction,
              guess = guess,
              real = app_vals$es,
              samples = app_vals$sample_n,
              n_obs = input$n_obs,
              violin = input$show_violin,
              boxplot = input$show_boxplot,
              points = input$show_points,
              trinary = input$trinary,
              one_two = input$one_two)

    return(app_vals$data)
  })

  # . Generate current_plot ----
  output$current_plot <- renderPlot({
    dat <- simdata()
    if (input$one_two) {
      # show A on odd and B on even trials)
      if (app_vals$sample_n %% 2 == 1) {
        dat <- filter(dat, group == "A")
        #color_vals <- "red"
        #shape_vals <- 15
      } else {
        dat <- filter(dat, group == "B")
        #color_vals <- "steelblue3"
        #shape_vals <- 19
      }
    }
    #} else {
      color_vals <- c("red", "steelblue3")
      shape_vals <- c(15, 19)
    #}
      
    dat$group <- factor(dat$group, levels = c("A", "B"))
    
    p <- dat %>%
      ggplot(aes(group, val, color = group, shape = group)) +
      ylim(-6, 6) +
      ylab("") +
      scale_x_discrete(drop = F) +
      scale_colour_manual(values = color_vals, drop = F) +
      scale_shape_manual(values = shape_vals, drop = F)
      theme_minimal()

    if (input$show_points) {
      pt_width <- (app_vals$n-1) * 0.004
      pt_size <- 5.6 - log(app_vals$n)
      p <- p + geom_jitter(show.legend = F, width = pt_width, size = pt_size)
    }
    if (input$show_violin & app_vals$n > 1) {
      p <- p + geom_violin(draw_quantiles = 0.5,
                           alpha = 0.3, show.legend = F)
    }
    if (input$show_boxplot & app_vals$n > 1) {
      p <- p + geom_boxplot(width = 0.25, alpha = 0.3, show.legend = F)
    }

    p
  })

  # . Generate performance_plot ----
  output$performance_plot <- renderPlot({
    save_guess() %>%
      ggplot(aes(real, guess)) +
      geom_abline(slope = 1, intercept = 0, color = "grey30") +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("The actual effect size (d)") +
      ylab("Your guessed effect size (d)") +
      coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
  })
  
  output$data_table <- renderDataTable( save_guess() )
}

# Run the application ----
shinyApp(ui = ui, server = server)

