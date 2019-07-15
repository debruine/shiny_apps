# Libraries ----
library(shiny)
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
    shinyjs::useShinyjs(), #add useShinyjs to be able to disable buttons upon making a choice.
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
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  shinyjs::hide("submit_guess")
  shinyjs::disable("sample_again")
  shinyjs::disable("d_guess")
  shinyjs::disable("guess_A")
  shinyjs::disable("guess_0")
  shinyjs::disable("guess_B")
  
  # toggle for trinary/continuous input ----
  observe({
    if (input$trinary) {
      shinyjs::show("trinary_input")
      shinyjs::hide("continuous_input")
      shinyjs::hide("submit_guess")
    } else {
      shinyjs::hide("trinary_input")
      shinyjs::show("continuous_input")
      shinyjs::show("submit_guess")
    }
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
                      one_two = logical()
    ),
    id = c(format(Sys.time(), format = "%Y-%m-%d-%H%M%S"),
      "_", sample(letters, 8, replace = TRUE)) %>% 
      paste(collapse = ""),
    stats = data.frame(),
    n = 1,
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
    message("next_trial: ", input$next_trial)
    shinyjs::enable("sample_again")
    shinyjs::hide("next_trial")
    shinyjs::enable("d_guess")
    shinyjs::enable("guess_A")
    shinyjs::enable("guess_0")
    shinyjs::enable("guess_B")
    
    # shinyjs::disable submit until an option is chosen
    if (input$trinary) {
      shinyjs::disable("submit_guess")
    } else {
      shinyjs::show("submit_guess")
    }
    
    # set button colours normal
    shinyjs::removeClass(id = "guess_A", class = "A")
    shinyjs::removeClass(id = "guess_0", class = "null")
    shinyjs::removeClass(id = "guess_B", class = "B")
    
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
    shinyjs::click("sample_again")
    
  }, ignoreNULL = TRUE)


  # sample_again / simdata()----
  simdata <- eventReactive(input$sample_again, {
    app_vals$es_show <- "?"
    app_vals$guess_show <- "?"
    
    app_vals$sample_n <- app_vals$sample_n + 1
    
    # simulate data
    A <-  rnorm(app_vals$n, app_vals$offset, 1)
    B <- rnorm(app_vals$n, app_vals$offset + app_vals$es, 1)
    dat <- data.frame(
      group = rep(c("A", "B"), each = app_vals$n),
      val = c(A, B) %>% round(3)
    )
    
    dat$group <- factor(dat$group, levels = c("A", "B"))
    
    if (input$one_two) {
      # shinyjs::show A on odd and B on even trials)
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
      app_vals$guess_show == "A" ~ "red",
      app_vals$guess_show == "0" ~ "purple",
      app_vals$guess_show == "B" ~ "blue",
      TRUE ~ "black"
    )
    valueBox(app_vals$guess_show, "Your Guess",color = color)
  })
  output$esBox <- renderValueBox({
    color <- case_when(
      app_vals$es_show == "A" ~ "red",
      app_vals$es_show == "0" ~ "purple",
      app_vals$es_show == "B" ~ "blue",
      TRUE ~ "black"
    )
    valueBox(app_vals$es_show, "True Effect",color = color)
  })
  output$feedback <- renderUI(HTML(app_vals$feedback))
  
  # guess button actions ----
  observeEvent(input$guess_A, {
    app_vals$direction <- "A"
    shinyjs::addClass(id = "guess_A", class = "A")
    shinyjs::removeClass(id = "guess_0", class = "null")
    shinyjs::removeClass(id = "guess_B", class = "B")
    shinyjs::click("submit_guess")
  })
  observeEvent(input$guess_0, {
    app_vals$direction <- "0"
    shinyjs::removeClass(id = "guess_A", class = "A")
    shinyjs::addClass(id = "guess_0", class = "null")
    shinyjs::removeClass(id = "guess_B", class = "B")
    shinyjs::click("submit_guess")
  })
  observeEvent(input$guess_B, {
    app_vals$direction <- "B"
    shinyjs::removeClass(id = "guess_A", class = "A")
    shinyjs::removeClass(id = "guess_0", class = "null")
    shinyjs::addClass(id = "guess_B", class = "B")
    shinyjs::click("submit_guess")
  })

  # submit_guess ----
  observeEvent(input$submit_guess, {
    message("submit_guess")
    
    shinyjs::hide("submit_guess")
    shinyjs::show("next_trial")
    shinyjs::disable("sample_again")
    shinyjs::disable("d_guess")
    shinyjs::disable("guess_A")
    shinyjs::disable("guess_0")
    shinyjs::disable("guess_B")

    if (input$trinary) {
      message("direction=", app_vals$direction)
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
    
    app_vals$data <- app_vals$data %>%
      add_row(trial_n = input$next_trial,
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
              one_two = input$one_two)
    
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
    
    if (input$accumulate & input$n_obs == 1) {
      dat <- app_vals$stats %>%
        filter(trial_n == input$next_trial)
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
      pt_width <- (nrow(dat)-1) * 0.004
      pt_size <- 5.6 - log(nrow(dat))
      p <- p + geom_jitter(show.legend = F, width = pt_width, size = pt_size)
    }
    if (input$show_violin & nrow(dat) > 1) {
      p <- p + geom_violin(draw_quantiles = 0.5,
                           alpha = 0.3, show.legend = F)
    }
    if (input$show_boxplot & nrow(dat) > 1) {
      p <- p + geom_boxplot(width = 0.25, alpha = 0.3, show.legend = F)
    }

    p
  })

  # . performance_plot ----
  output$performance_plot <- renderPlot({
    if (input$trinary) {
      # TODO: too rigid, needs flexibility for other levels combos
      app_vals$data %>%
        mutate(bin = factor(real, levels = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8))) %>%
        group_by(bin) %>%
        summarise(correct = mean(correct)*100) %>%
        ggplot(aes(bin, correct, fill = bin)) +
        geom_col(show.legend = FALSE) +
        xlab("The true effect size (d)") +
        ylab("Percent correct") +
        scale_x_discrete(drop = FALSE) +
        scale_fill_manual(values = c("#DD4B39", "#DD4B39", "#DD4B39",
                                     "#605CA8", 
                                     "#0073B7", "#0073B7", "#0073B7"),
                          drop = FALSE)
        
    } else {
      app_vals$data %>%
        ggplot(aes(real, guess_es)) +
        geom_abline(slope = 1, intercept = 0, color = "grey30") +
        geom_point() +
        geom_smooth(method = "lm") +
        xlab("The true effect size (d)") +
        ylab("Your guessed effect size (d)") +
        coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
    }
  })
  
  # . overall_plot ----
  output$overall_plot <- renderPlot({
    input$submit_guess
    
    loadData() %>%
      mutate(bin = factor(real, levels = c(-0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8))) %>%
      group_by(bin) %>%
      summarise(correct = mean(correct)*100) %>%
      ggplot(aes(bin, correct, fill = bin)) +
      geom_col(show.legend = FALSE) +
      xlab("The true effect size (d)") +
      ylab("Percent correct") +
      scale_x_discrete(drop = FALSE) +
      scale_fill_manual(values = c("#DD4B39", "#DD4B39", "#DD4B39",
                                   "#605CA8", 
                                   "#0073B7", "#0073B7", "#0073B7"),
                        drop = FALSE)
      
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

