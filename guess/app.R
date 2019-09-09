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
  tog_interface(FALSE)
  # always hidden now (only needs shown for continuous slider)
  hide("submit_guess") 
  hide("d_guess")
  presets(session = session)
  
  # toggle for trinary/continuous input ----
  observe({
    toggle("trinary_input", condition = input$trinary)
    toggle("continuous_input", condition = !input$trinary)
    toggle("perf_plot_box", condition = !input$trinary)
    #toggle("submit_guess", condition = !input$trinary)
    #toggleState("submit_guess", condition = !input$trinary)
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
                      barplot = logical(),
                      trinary = logical(),
                      one_two = logical(),
                      accumulate = logical(),
                      stringsAsFactors = FALSE
    ),
    id = c(format(Sys.time(), format = "%Y-%m-%d-%H%M%S"),
      "_", sample(letters, 8, replace = TRUE)) %>% 
      paste(collapse = ""),
    stats = data.frame(stringsAsFactors = FALSE),
    offset = 0,
    sd = 1,
    es = 0,
    es_show = "?",
    guess_show = "?",
    sample_n = 0,
    feedback = "",
    direction = "",
    FP = 0,
    FN = 0,
    TP = 0,
    TN = 0,
    guessing = FALSE # flag for display
  )
  
  output$guess_correct <- renderUI(HTML({
    cor <- sum(app_vals$data$correct)
    tot <- length(app_vals$data$correct)
    sprintf(
      "<h3>You have answered %2.0f%% (%i of %i) trials correctly.</h3>
      <h4>(%2.0f%% true positives, %2.0f%% true negatives, %2.0f%% false positives, %2.0f%% false negatives)</h4>",
      ifelse(tot == 0, 0, round(100*cor/tot)),
      cor,
      tot,
      ifelse(tot == 0, 0, round(100*app_vals$TP/tot)),
      ifelse(tot == 0, 0, round(100*app_vals$TN/tot)),
      ifelse(tot == 0, 0, round(100*app_vals$FP/tot)),
      ifelse(tot == 0, 0, round(100*app_vals$FN/tot))
    )
  }))
  
  # settings ----
  
  observeEvent(input$setting_debug, {
    presets(show_points = T,
            show_meanse = T,
            accumulate = T,
            show_debug = T, 
            session = session)
  })
  observeEvent(input$setting_1, {
    presets(trinary = T,
            accumulate = F,
            session = session) 
  })
  observeEvent(input$setting_2, {
    presets(trinary = T,
            accumulate = T, 
            session = session) 
  })
  observeEvent(input$setting_3, {
    presets(trinary = F, 
            accumulate = F, 
            session = session) 
  })
  observeEvent(input$setting_4, {
    presets(accumulate = T, 
            trinary = F, 
            session = session)  
  })
  observeEvent(input$setting_c, {
    presets(n_obs = 50,
            max_samples = 1,
            one_two = F,
            accumulate = T, 
            trinary = F,
            prob_null = 10,
            session = session) 
  })

  # next_trial ----
  observeEvent(input$next_trial, {
    app_vals$guessing <- FALSE
    tog_interface(TRUE)
    
    # set button colours normal
    setButtonClass()
    
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
    app_vals$offset <- sample(seq(-1, 1, by = 0.25), 1)
    app_vals$sd <- sample(seq(0.5, 1, by = 0.1), 1)
    
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
    message("sample ", input$sample_again)
    app_vals$es_show <- "?"
    app_vals$guess_show <- "?"
    
    app_vals$sample_n <- app_vals$sample_n + 1
    
    # prevent further sampling after max_samples
    if (app_vals$sample_n >= input$max_samples) {
      disable("sample_again")
      hide("sample_again")
    }
    
    # simulate data
    A <-  rnorm(input$n_obs, app_vals$offset, app_vals$sd) %>% pmin(4) %>% pmax(-4)
    B <- rnorm(input$n_obs, app_vals$offset + (app_vals$es * app_vals$sd), app_vals$sd) %>% pmin(4) %>% pmax(-4)
    #A <- truncnorm::rtruncnorm(input$n_obs, -4, 4, app_vals$offset, app_vals$sd)
    #B <- truncnorm::rtruncnorm(input$n_obs, -4, 4, app_vals$offset + (app_vals$es * app_vals$sd), app_vals$sd)
    dat <- data.frame(
      group = rep(c("A", "B"), each = input$n_obs),
      val = c(A, B) %>% round(3)
    )
    
    # I don't think we need this anymore
    #dat$group <- factor(dat$group, levels = c("A", "B"))
    
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
      app_vals$direction == "A>B" ~ "red",
      app_vals$direction == "A=B" ~ "purple",
      app_vals$direction == "B>A" ~ "blue",
      TRUE ~ "black"
    )
    valueBox(app_vals$guess_show, "Your Guess",color = color)
  })
  output$esBox <- renderValueBox({
    color <- case_when(
      app_vals$es_show == "?" ~ "black",
      app_vals$es_show == "A>B" ~ "red",
      app_vals$es_show < 0 ~ "red",
      app_vals$es_show == "A=B" ~ "purple",
      app_vals$es_show == 0 ~ "purple",
      app_vals$es_show == "B>A" ~ "blue",
      app_vals$es_show > 0 ~ "blue",
      TRUE ~ "black"
    )
    valueBox(app_vals$es_show, "True Effect",color = color)
  })
  output$feedback <- renderUI(HTML(app_vals$feedback))
  
  # guess button actions ----
  observeEvent(input$guess_A, {
    app_vals$direction <- "A>B"
    setButtonClass(id = "guess_A", class = "A")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_0, {
    app_vals$direction <- "A=B"
    setButtonClass(id = "guess_0", class = "null")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_B, {
    app_vals$direction <- "B>A"
    setButtonClass(id = "guess_B", class = "B")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  
  observeEvent(input$guess_A8, {
    updateSliderInput(session, "d_guess", value = -0.8)
    setButtonClass(id = "guess_A8", class = "A")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_A5, {
    updateSliderInput(session, "d_guess", value = -0.5)
    setButtonClass(id = "guess_A5", class = "A")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_A2, {
    updateSliderInput(session, "d_guess", value = -0.2)
    setButtonClass(id = "guess_A2", class = "A")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_00, {
    updateSliderInput(session, "d_guess", value = 0)
    setButtonClass(id = "guess_00", class = "null")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_B2, {
    updateSliderInput(session, "d_guess", value = +0.2)
    setButtonClass(id = "guess_B2", class = "B")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_B5, {
    updateSliderInput(session, "d_guess", value = +0.5)
    setButtonClass(id = "guess_B5", class = "B")
    click("submit_guess")
  }, ignoreNULL = TRUE)
  observeEvent(input$guess_B8, {
    updateSliderInput(session, "d_guess", value = +0.8)
    setButtonClass(id = "guess_B8", class = "B")
    click("submit_guess")
  }, ignoreNULL = TRUE)

  # submit_guess ----
  observeEvent(input$submit_guess, {
    app_vals$guessing <- TRUE # set flag for display
    tog_interface(FALSE)

    if (input$trinary) {
      app_vals$guess_show <- app_vals$direction
      guess <- NA
      app_vals$es_show <- case_when(
        app_vals$es < 0  ~ "A>B",
        app_vals$es == 0 ~ "A=B",
        app_vals$es > 0  ~ "B>A"
      )
    } else {
      app_vals$direction <- case_when(
        input$d_guess < 0  ~ "A>B",
        input$d_guess == 0 ~ "A=B",
        input$d_guess > 0  ~ "B>A"
      )
      guess <- input$d_guess
      app_vals$guess_show <- guess
      app_vals$es_show <- app_vals$es
    }
    
    correct <- case_when(
      app_vals$es < 0    & app_vals$direction == "A>B" ~ TRUE,
      app_vals$es == 0   & app_vals$direction == "A=B"~ TRUE,
      app_vals$es > 0    & app_vals$direction == "B>A" ~ TRUE,
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
              barplot = input$show_barplot,
              meanse = input$show_meanse,
              trinary = input$trinary,
              one_two = input$one_two,
              accumulate = input$accumulate,
              stringsAsFactors = FALSE))
    
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

    stats <- trial_dat %>%
      add_row(group = "B", val = NA) %>%
      group_by(group) %>%
      summarise(m = mean(val, na.rm = TRUE),
                sd = sd(val, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(sd_pooled = sqrt(mean(sd)))
    
    pwr_text <- ""
    if (app_vals$es != 0 & nrow(trial_dat) > 3) {
      pwr_text <- sprintf(
        "With %i data points, you had %2.1f%% power to detect this effect with an alpha of 0.05.",
        nrow(trial_dat),
        power.t.test(n = nrow(trial_dat), delta = app_vals$es)$power * 100
      )
    }
    
    my_res <- case_when(
      app_vals$es < 0  & app_vals$direction == "A>B" ~ "TP",
      app_vals$es > 0  & app_vals$direction == "B>A" ~ "TP",
      app_vals$es == 0 & app_vals$direction == "A=B" ~ "TN",
      app_vals$es == 0 & app_vals$direction != "A=B" ~ "FP",
      TRUE ~ "FN"
    )
    
    # increment count for T/F P/N ----
    app_vals[[my_res]] <- app_vals[[my_res]] + 1
    
    correct_text <- list(
      TP = "Correct, this is a true positive. There was a real effect in the population and you correctly identified it.",
      TN = "Correct, this is a true negative. There was no real effect in the population and you correctly identified this.",
      FP = "Incorrect, this is a false positive (Type I Error). There was no real effect in the population but you incorrectly identified one.",
      FN =  "Incorrect, this is a false negative (Type II Error). There was a real effect in the population but you failed to identify it."
    )
    
    # concat feedback ----
    app_vals$feedback <- sprintf(
      "<h3>%s</h3>
      <h3>Across the %i data points you observed in this trial, A had a mean of %1.2f (SD = %1.2f), B had a mean of %1.2f (SD = %1.2f), and the observed effect size was d = %1.2f%s.</h3>
      <h3>These data were sampled from a population where A had a mean of %1.2f (SD = %1.2f), B had a mean of %1.2f (SD = %1.2f), and the true effect size was d = %1.2f.</h3>",
      correct_text[[my_res]], 
      nrow(trial_dat),
      stats$m[1],
      stats$sd[1],
      stats$m[2],
      stats$sd[2],
      (stats$m[2] - stats$m[1])/stats$sd_pooled[1],
      t_text,
      app_vals$offset, 
      app_vals$sd, 
      app_vals$offset + (app_vals$sd*app_vals$es),
      app_vals$sd, 
      app_vals$es
    )
    
    # save the data ----
    saveData(app_vals$data, app_vals$stats, app_vals$id)
  })

  # . current_plot ----
  output$current_plot <- renderPlot({
    dat <- simdata()
    
    if ((input$accumulate | app_vals$guessing) & 
        input$n_obs == 1) {
      dat <- app_vals$stats %>%
        filter(trial_n == input$next_trial)
    }
    
    current_plot(dat, 
                 points = input$show_points, 
                 violin = input$show_violin, 
                 boxplot = input$show_boxplot,
                 barplot = input$show_barplot,
                 meanse = (input$show_meanse | app_vals$guessing),
                 stats = (input$show_debug),
                 m1 = app_vals$offset,
                 m2 = app_vals$offset + (app_vals$es * app_vals$sd),
                 sd = app_vals$sd,
                 pt_width = ifelse(input$accumulate | input$n_obs > 1| app_vals$guessing, .35, 0))
  })

  # . performance_plot ----
  output$performance_plot <- renderPlot({
    if (input$trinary) {
      #app_vals$data %>% summary_tri_plot()
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

