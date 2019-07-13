# Libraries ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

saveData <- function(data, id, outputDir = "responses") {
  # Create a unique file name
  fileName <- sprintf("%s.csv", id)
  
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = TRUE, quote = TRUE
  )
}

loadData <- function(outputDir = "responses") {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    data <- data.frame()
  } else {
    data <- lapply(files, function(f) {
      read.csv(f, stringsAsFactors = FALSE) %>%
        mutate(session_id = gsub("responses/", "", f))
    })
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}

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
  checkboxInput("trinary", "Trinary Input",value = T),
  sliderInput("prob_null", "Null probability", min = 0, max = 100, value = 50, step = 5),
  p("This app is not storing your data beyond this session.")
)

# . main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  inlineCSS(list(.A = "color: white !important; background: #DD4B39 !important;",
                 .B = "color: white !important; background: #0073B7 !important;",
                 .null = "color: white !important; background: #605CA8 !important;")),
  # . . instructions ----
  box(title = "Instructions", width = 12,
    h5("Your task is to guess whether there is a real difference between two groups, one represented by red squares, and one represented by blue circles. To inform your guess, you will sample individual data points from each group."),
    p("The real difference between the two groups will be randomly decided by the app (and shown after you made your decision). The difference is either an effect size of 0, 0.2, 0.5, or 0.8. If there is an effect, it can be positive or negative (i.e., squares can have a higher or lower means than circles)."),
    h5("You should sample data until you are 80% certain about your decision about whether there is a real difference or not. If you do this task 30 times, you should guess correctly 24 of the 30 times."),
    p("Click the 'Next Trial' button to start, and click the 'Sample Again' button until you are 80% certain of your choice. Afterwards, the app will reveal whether you were correct or not. You can click the 'Next Trial' button to start again. The app will keep track of your performence."),
    collapsible = TRUE, collapsed = TRUE
  ),
  
  
  fluidRow(
    column(
      width = 4,
      # . . trinary input ----
      box(id = "trinary_input",
        width = NULL,
        h4("Which group is larger?"),
        # . . trinary guess buttons ----
        actionButton("guess_A", "A", width = "32%"),
        actionButton("guess_0", "A = B", width = "32%"),
        actionButton("guess_B", "B", width = "32%"),
        
        # . . dir_guess radiobuttons ----
        radioButtons("dir_guess", "My effect direction guess", 
                     choices = c("A larger" = "A", 
                                 "A and B same" = "0", 
                                 "B larger" = "B"), 
                     inline = TRUE)
      ),
      # . . continuous input ----
      box(id = "continuous_input",
          width = NULL,
          h4("How much larger is group B (blue circle) than group A (red square)?"),
          # . . d_guess slide ----
          sliderInput("d_guess", NULL, 
                      min = -3, max = 3, value = 0, step = 0.05)
      ),
      # . . sample_again button ----
      actionButton("sample_again", "Sample Again", width = "49%"),
      # . . submit_guess button ----
      actionButton("submit_guess", "Submit Guess", width = "49%"),
      # . . next_trial button ----
      actionButton("next_trial", "Next Trial", width = "49%")
    ),
    column(
      width = 8,
      box(
        width = NULL,
        # . . current_n output ----
        textOutput("current_n"),
        # . . current_plot plot ----
        plotOutput("current_plot")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      # . . values output ----
      valueBoxOutput("samplesBox", width = 4),
      valueBoxOutput("guessBox", width = 4),
      valueBoxOutput("esBox", width = 4),
      h3(textOutput("feedback"))
    ),
    column(
      width = 6,
      # . performance plot ----
      box(
        title = "Your guessing performance",
        width = NULL,
        # . . performance_plot plot ----
        plotOutput("performance_plot")
      )
    )
  )
)

# . data_tab ----
data_tab <- tabItem(
  tabName = "data_tab",
  # . . data_table ----
  dataTableOutput("data_table"),
  downloadButton("download", "Download Session Data"),
  downloadButton("download_all", "Download All Data")
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
  # On start ----
  addClass(selector = "body", class = "sidebar-collapse")
  hide("submit_guess")
  hide("dir_guess")
  
  # toggle for trinary/continuous input ----
  observe({
    if (input$trinary) {
      show("trinary_input")
      hide("continuous_input")
    } else {
      hide("trinary_input")
      show("continuous_input")
    }
  })
  
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
    id = paste0(
      format(Sys.time(), format = "%Y-%m-%d-%H%M%S"),
      "_", sample(letters, 5) %>% paste(collapse = "")),
    n = 20,
    offset = 0,
    es = 0,
    es_show = "?",
    guess_show = "?",
    sample_n = 0,
    feedback = ""
  )

  # next_trial ----
  observeEvent(input$next_trial, {
    show("submit_guess")
    enable("sample_again")
    hide("next_trial")
    enable("d_guess")
    enable("guess_A")
    enable("guess_0")
    enable("guess_B")
    disable("n_obs")
    
    # disable submit until an option is chosen
    if (input$trinary) disable("submit_guess")
    
    # set button colours normal
    removeClass(id = "guess_A", class = "A")
    removeClass(id = "guess_0", class = "null")
    removeClass(id = "guess_B", class = "B")
    
    app_vals$feedback <- ""
    
    # reset slider
    updateSliderInput(session, "d_guess", value = 0)
    
    # set N
    app_vals$n <- input$n_obs
    
    # set sample effect size
    #app_vals$es <- (rnorm(1, 0, 1) %>% 
    #                  pmax(-3) %>% 
    #                  pmin(3) * 2
    #) %>% round(1) / 2
    
    app_vals$es <- sample(c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8), 1)
    
    # null effects 50% of the time
    pn <- input$prob_null/100
    app_vals$es <- sample(c(0, app_vals$es), 1, prob = c(pn, 1-pn))
    
    # set offset (so one group isn't always at 0)
    app_vals$offset <- sample(seq(-1,1,by = 0.1), 1)
    
    app_vals$sample_n <- 0
    click("sample_again")
    
  }, ignoreNULL = FALSE)


  # sample_again ----
  simdata <- eventReactive(input$sample_again, {
    app_vals$es_show <- "?"
    app_vals$guess_show <- "?"
    
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
  
  # valueBoxes ----
  output$samplesBox <- renderValueBox({
    valueBox(app_vals$sample_n, "Samples",color = "red")
  })
  output$guessBox <- renderValueBox({
    valueBox(app_vals$guess_show, "Your Guess",color = "purple")
  })
  output$esBox <- renderValueBox({
    valueBox(app_vals$es_show, "True Effect",color = "blue")
  })
  output$feedback <- renderText(app_vals$feedback)

  # Show current_n ----
  output$current_n <- renderText(
    paste0("The graph below shows ", 
           app_vals$n,
           " observation", 
           ifelse(app_vals$n == 1, "", "s"), 
           " in each group.")
  )
  
  # . guess button actions ----
  observeEvent(input$guess_A, {
    updateRadioButtons(session, "dir_guess", selected = "A")
    addClass(id = "guess_A", class = "A")
    removeClass(id = "guess_0", class = "null")
    removeClass(id = "guess_B", class = "B")
    enable("submit_guess")
  })
  observeEvent(input$guess_0, {
    updateRadioButtons(session, "dir_guess", selected = "0")
    removeClass(id = "guess_A", class = "A")
    addClass(id = "guess_0", class = "null")
    removeClass(id = "guess_B", class = "B")
    enable("submit_guess")
  })
  observeEvent(input$guess_B, {
    updateRadioButtons(session, "dir_guess", selected = "B")
    removeClass(id = "guess_A", class = "A")
    removeClass(id = "guess_0", class = "null")
    addClass(id = "guess_B", class = "B")
    enable("submit_guess")
  })

  # . Save guess ----
  observeEvent(input$submit_guess, {
    hide("submit_guess")
    show("next_trial")
    disable("sample_again")
    disable("d_guess")
    disable("guess_A")
    disable("guess_0")
    disable("guess_B")
    enable("n_obs")

    if (input$trinary) {
      direction <- input$dir_guess
      app_vals$guess_show <- direction
      guess <- NA
      app_vals$es_show <- case_when(
        app_vals$es < 0 ~ "A",
        app_vals$es == 0 ~ "0",
        app_vals$es > 0 ~ "B"
      )
    } else {
      direction <- case_when(
        input$d_guess < 0 ~ "A",
        input$d_guess == 0 ~ "0",
        input$d_guess > 0 ~ "B"
      )
      guess <- input$d_guess
      app_vals$guess_show <- guess
      app_vals$es_show <- app_vals$es
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
    
    # . . feedback ----
    real_dir <- case_when(
      app_vals$es < 0 ~ "bigger",
      app_vals$es == "0" ~ "the same size as",
      app_vals$es > 0 ~ "smaller"
    )
    
    correct <- case_when(
      app_vals$es < 0    & direction == "A" ~ "Correct!",
      app_vals$es == "0" & direction == "0"~ "Correct!",
      app_vals$es > 0    & direction == "B" ~ "Correct!",
      TRUE ~ "Incorrect."
    )
    
    app_vals$feedback <- paste0(
      correct, " In this trial, A was ", real_dir , 
      " than B with a true effect size of ",
      app_vals$es, "."
    )
    
    saveData(app_vals$data, app_vals$id)
  })

  # . Generate current_plot ----
  output$current_plot <- renderPlot({
    dat <- simdata()
    if (input$one_two) {
      # show A on odd and B on even trials)
      if (app_vals$sample_n %% 2 == 1) {
        dat <- filter(dat, group == "A")
      } else {
        dat <- filter(dat, group == "B")
      }
    }
    
    p <- dat %>%
      ggplot(aes(group, val, color = group, shape = group)) +
      ylim(-6, 6) +
      ylab("") +
      scale_x_discrete(drop = F) +
      scale_colour_manual(values = c("red", "steelblue3"), drop = F) +
      scale_shape_manual(values = c(15, 19), drop = F)
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
    if (input$trinary) {
      app_vals$data %>%
        mutate(
          correct = case_when(
            real == 0 & guess == "0" ~ 1,
            real < 0 & guess == "A" ~ 1,
            real > 0 & guess == "B" ~ 1,
            TRUE ~ 0
          ), 
          bin = round(real*10)/10
        ) %>%
        group_by(bin) %>%
        summarise(correct = mean(correct)) %>%
        ggplot(aes(bin, correct)) +
        geom_col()
        
    } else {
      app_vals$data %>%
        ggplot(aes(real, guess)) +
        geom_abline(slope = 1, intercept = 0, color = "grey30") +
        geom_point() +
        geom_smooth(method = "lm") +
        xlab("The true effect size (d)") +
        ylab("Your guessed effect size (d)") +
        coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
    }
  })
  
  # . download session data
  output$download <- downloadHandler(
    'data.csv', 
    content = function(file) { write.csv(app_vals$data, file) }, 
    contentType = "text/csv"
  )
  
  # . download all data
  output$download_all <- downloadHandler(
    'all_data.csv', 
    content = function(file) { write.csv(loadData(), file) }, 
    contentType = "text/csv"
  )
  
  output$data_table <- renderDataTable( loadData() )
}

# Run the application ----
shinyApp(ui = ui, server = server)

