# Libraries ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

# Functions ----
source("func.R")

# Define UI ----

source("sidebar.R")
source("main_tab.R")
source("data_tab.R")

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "Guess"),
  sidebar,
  dashboardBody(
    useShinyjs(), #add useShinyjs to be able to disable buttons upon making a choice.
    tags$head(
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "custom.css")
    ),
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
  disable("sample_again")
  disable("d_guess")
  disable("guess_A")
  disable("guess_0")
  disable("guess_B")
  
  # toggle for trinary/continuous input ----
  observe({
    toggle("trinary_input", condition = input$trinary)
    toggle("continuous_input", condition = !input$trinary)
    toggle("submit_guess", condition = !input$trinary)
    toggleState("submit_guess", condition = !input$trinary)
  })
  
  # Set app_vals reactiveValues ----
  app_vals <- reactiveValues(
    data = data.frame(trial_n = integer(),
                      guess_dir = character(),
                      guess_es = double(),
                      real = double(),
                      correct = logical(),
                      samples = integer(),
                      n_obs = integer(),
                      violin = logical(),
                      boxplot = logical(),
                      points = logical(),
                      trinary = logical(),
                      one_two = logical(),
                      accumulate = logical()
    ),
    id = c(format(Sys.time(), format = "%Y-%m-%d-%H%M%S"),
      "_", sample(letters, 8, replace = TRUE)) %>% 
      paste(collapse = ""),
    stats = data.frame(),
    offset = 0,
    es = 0,
    es_show = "?",
    guess_show = "?",
    sample_n = 0,
    feedback = "",
    direction = ""
  )
  
  output$guess_correct <- renderText({
    sprintf(
      "You have answered %i of %i trials correctly.",
      sum(app_vals$data$correct),
      length(app_vals$data$correct)
    )
  })

  # next_trial ----
  observeEvent(input$next_trial, {
    enable("sample_again")
    hide("next_trial")
    enable("d_guess")
    enable("guess_A")
    enable("guess_0")
    enable("guess_B")
    
    # disable submit until an option is chosen
    if (input$trinary) {
      disable("submit_guess")
    } else {
      show("submit_guess")
    }
    
    # set button colours normal
    removeClass(id = "guess_A", class = "A")
    removeClass(id = "guess_0", class = "null")
    removeClass(id = "guess_B", class = "B")
    
    app_vals$feedback <- ""
    app_vals$direction <- ""
    
    # reset slider
    updateSliderInput(session, "d_guess", value = 0)
    
    # set sample effect size
    app_vals$es <- sample(c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8), 1)
    
    # null effects prob_null% of the time
    pn <- input$prob_null/100
    app_vals$es <- sample(c(0, app_vals$es), 1, prob = c(pn, 1-pn))
    
    # set offset (so one group isn't always at 0)
    app_vals$offset <- sample(seq(-1,1,by = 0.1), 1)
    
    # generate dataset(s) up front (maybe speed things up?)
    # sample_n <- input$n_obs*input$max_samples
    # A <- rnorm(sample_n, app_vals$offset, 1)
    # B <- rnorm(sample_n, app_vals$offset + app_vals$es, 1)
    # app_vals$dat <- expand.grid(trial_n = 1:input$n_obs, 
    #                    sample_n = 1:input$max_samples,
    #                    group = c("A", "B")) %>%
    #   mutate(val = c(A, B) %>% round(3) ) %>%
    #   select(sample_n, trial_n, group, val)
    # 
    # app_vals$dat$group <- factor(dat$group, levels = c("A", "B"))
    
    app_vals$sample_n <- 0
    click("sample_again")
    
  }, ignoreNULL = TRUE)


  # sample_again / simdata()----
  simdata <- eventReactive(input$sample_again, {
    app_vals$es_show <- "?"
    app_vals$guess_show <- "?"
    
    app_vals$sample_n <- app_vals$sample_n + 1
    
    # prevent further sampling after max_samples
    if (app_vals$sample_n >= input$max_samples) {
      disable("sample_again")
    }
    
    # simulate data
    A <-  rnorm(input$n_obs, app_vals$offset, 1)
    B <- rnorm(input$n_obs, app_vals$offset + app_vals$es, 1)
    dat <- data.frame(
      group = rep(c("A", "B"), each = input$n_obs),
      val = c(A, B) %>% round(3)
    )
    
    dat$group <- factor(dat$group, levels = c("A", "B"))
    
    if (input$one_two) {
      # show A on odd and B on even trials)
      if (app_vals$sample_n %% 2 == 1) {
        dat <- filter(dat, group == "A")
      } else {
        dat <- filter(dat, group == "B")
      }
    }
    
    # record sample stats
    stats <- dat %>%
      mutate(trial_n = input$next_trial,
             sample_n = input$sample_again)
      
    app_vals$stats <- suppressWarnings(
      bind_rows(app_vals$stats, stats)
    )

    return(dat)
  }, ignoreNULL = TRUE)
  
  # valueBoxes ----
  output$samplesBox <- renderValueBox({
    valueBox(app_vals$sample_n, "Samples",color = "black")
  })
  output$guessBox <- renderValueBox({
    color <- case_when(
      app_vals$direction == "A" ~ "red",
      app_vals$direction == "0" ~ "purple",
      app_vals$direction == "B" ~ "blue",
      TRUE ~ "black"
    )
    valueBox(app_vals$guess_show, "Your Guess",color = color)
  })
  output$esBox <- renderValueBox({
    color <- case_when(
      app_vals$es_show == "?" ~ "black",
      app_vals$es_show == "A" ~ "red",
      app_vals$es_show < 0 ~ "red",
      app_vals$es_show == "0" ~ "purple",
      app_vals$es_show == "B" ~ "blue",
      app_vals$es_show > 0 ~ "blue",
      TRUE ~ "black"
    )
    valueBox(app_vals$es_show, "True Effect",color = color)
  })
  output$feedback <- renderUI(HTML(app_vals$feedback))
  
  # guess button actions ----
  observeEvent(input$guess_A, {
    app_vals$direction <- "A"
    addClass(id = "guess_A", class = "A")
    removeClass(id = "guess_0", class = "null")
    removeClass(id = "guess_B", class = "B")
    click("submit_guess")
  })
  observeEvent(input$guess_0, {
    app_vals$direction <- "0"
    removeClass(id = "guess_A", class = "A")
    addClass(id = "guess_0", class = "null")
    removeClass(id = "guess_B", class = "B")
    click("submit_guess")
  })
  observeEvent(input$guess_B, {
    app_vals$direction <- "B"
    removeClass(id = "guess_A", class = "A")
    removeClass(id = "guess_0", class = "null")
    addClass(id = "guess_B", class = "B")
    click("submit_guess")
  })

  # submit_guess ----
  observeEvent(input$submit_guess, {
    hide("submit_guess")
    show("next_trial")
    disable("sample_again")
    disable("d_guess")
    disable("guess_A")
    disable("guess_0")
    disable("guess_B")

    if (input$trinary) {
      app_vals$guess_show <- app_vals$direction
      guess <- NA
      app_vals$es_show <- case_when(
        app_vals$es < 0 ~ "A",
        app_vals$es == 0 ~ "0",
        app_vals$es > 0 ~ "B"
      )
    } else {
      app_vals$direction <- case_when(
        input$d_guess < 0 ~ "A",
        input$d_guess == 0 ~ "0",
        input$d_guess > 0 ~ "B"
      )
      guess <- input$d_guess
      app_vals$guess_show <- guess
      app_vals$es_show <- app_vals$es
    }
    
    correct <- case_when(
      app_vals$es < 0    & app_vals$direction == "A" ~ TRUE,
      app_vals$es == "0" & app_vals$direction == "0"~ TRUE,
      app_vals$es > 0    & app_vals$direction == "B" ~ TRUE,
      TRUE ~ FALSE
    )
    
    app_vals$data <- bind_rows(app_vals$data,
      data.frame(trial_n = input$next_trial,
              guess_dir = app_vals$direction,
              guess_es = guess,
              real = app_vals$es,
              correct = correct,
              samples = app_vals$sample_n,
              n_obs = input$n_obs,
              violin = input$show_violin,
              boxplot = input$show_boxplot,
              points = input$show_points,
              trinary = input$trinary,
              one_two = input$one_two,
              accumulate = input$accumulate))
    
    # feedback ----
    real_dir <- case_when(
      app_vals$es < 0 ~ "bigger",
      app_vals$es == "0" ~ "the same size as",
      app_vals$es > 0 ~ "smaller"
    )
    
    correct_text <- ifelse(correct, "Correct!", "Incorrect.")
    
    # get data from this trial
    trial_dat <-  filter(app_vals$stats, 
                         trial_n == input$next_trial)
    
    # calculate t.test if enough data
    t_text <- ""
    if (n_distinct(trial_dat$group) == 2 & nrow(trial_dat) > 3) {
      t <- t.test(val ~ group, data = trial_dat) %>%
        broom::tidy()
      
      t_text <- sprintf(
        " (t = %2.2f, p = %.3f)",
        t$statistic,
        t$p.value
      )
    }

    means <- trial_dat %>%
      add_row(group = "B", val = NA) %>%
      group_by(group) %>%
      summarise(m = mean(val, na.rm = TRUE),
                sd = sd(val, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(sd_pooled = sqrt(mean(sd))) %>%
      select(-sd) %>%
      spread(group, m)
    
    pwr_text <- ""
    if (app_vals$es != 0 & nrow(trial_dat) > 3) {
      pwr_text <- sprintf(
        "With %i data points, you had %2.1f%% power to detect this effect with an alpha of 0.05.",
        nrow(trial_dat),
        power.t.test(n = nrow(trial_dat), delta = app_vals$es)$power * 100
      )
    }
    
    app_vals$feedback <- sprintf(
      "<h3>%s In this trial, A was %s than B with a true effect size of %.2f. %s</h3>
      
      <h3>Across the %i data points you observed in this trial, A had a mean of %1.2f, B had a mean of %1.2f, and the observed effect size was d = %1.2f%s.</h3>",
      correct_text, 
      real_dir, 
      app_vals$es, 
      pwr_text,
      nrow(trial_dat),
      means$A,
      means$B,
      (means$B - means$A)/means$sd_pooled ,
      t_text
    )
    
    saveData(app_vals$data, app_vals$stats, app_vals$id)
  })

  # . current_plot ----
  output$current_plot <- renderPlot({
    dat <- simdata()
    
    # TODO: show accumulate on submit_guess
    if (input$accumulate & input$n_obs == 1) {
      dat <- app_vals$stats %>%
        filter(trial_n == input$next_trial)
    }
    
    current_plot(dat, 
                 points = input$show_points, 
                 violin = input$show_violin, 
                 boxplot = input$show_boxplot)
  })

  # . performance_plot ----
  output$performance_plot <- renderPlot({
    if (input$trinary) {
      app_vals$data %>% summary_tri_plot()
    } else {
      app_vals$data %>% summary_guess_plot()
    }
  })
  
  # . overall_plot ----
  output$overall_plot <- renderPlot({
    input$submit_guess
    
    loadData() %>% summary_tri_plot()
  })
  
  # . download session data ----
  output$download <- downloadHandler(
    'data.csv', 
    content = function(file) { readr::write_csv(app_vals$data, file) }, 
    contentType = "text/csv"
  )
  
  # . download all data ----
  output$download_data <- downloadHandler(
    'all_data.csv', 
    content = function(file) { readr::write_csv(loadData(), file) }, 
    contentType = "text/csv"
  )
  
  # . download all stats ----
  output$download_stats <- downloadHandler(
    'all_stats.csv', 
    content = function(file) { readr::write_csv(loadData(pattern = "*_stats.csv"), file) }, 
    contentType = "text/csv"
  )
  
  # . data_table ----
  output$data_table <- renderDataTable( app_vals$data )
}

# Run the application ----
shinyApp(ui = ui, server = server)

