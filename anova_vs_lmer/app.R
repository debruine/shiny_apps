## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(afex)
options("scipen"=10, "digits"=4)

## Functions ----

source("R/misc_funcs.R")
source("R/data_funcs.R")
source("R/plot_funcs.R")
source("R/lmer_funcs.R")

## Interface Tab Items ----

source("main_tab.R")

about_tab <- tabItem(
  tabName = "about_tab",
  h3("About this App"),
  p("Stuff about this")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "ANOVA vs LMER"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "main_tab"),
     # menuItem("About", tabName = "about_tab"),
      actionButton("reset", "Reset"),
      # sample size input ----
      box(
        title = "Sample Size",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        sliderInput("sub_n", "Subjects:", 
                    min = 10, max = 500, value = 50, step = 10),
        sliderInput("stim_n", "Stimuli per Group:", 
                    min = 5, max = 50, value = 10, step = 5)
      ),
      # fixed effects input ----
      box(
        title = "Fixed Effects",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        hidden(
            sliderInput("grand_i", "Overall Mean", min = 1, max = 7, value = 3, step = 0.1)
        ),
        sliderInput("stim_type_eff", "Main Effect of Stimulus Type in Raw Score: mean(B) - mean(A)", 
                    min = -2, max = 2, value = 0.5, step = 0.05)
        
      ),
      # random effects input ----
      box(
        title = "Random Effects",
        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        sliderInput("sub_sd", "Subject intercept SD:", min = 0, max = 2, value = 0.85, step = 0.05),
        sliderInput("stim_sd", "Stimulus intercept SD:", min = 0, max = 2, value = 0.75, step = 0.05),
        sliderInput("error_sd", "Residual (error) SD:", min = 0, max = 2, value = 1.10, step = 0.05),
        sliderInput("stim_type_sd", "Subject slope (stim type):", 
                    min = 0, max = 2, value = 0.50, step = 0.05)
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      main_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  ggplot2::theme_set(ggplot2::theme_bw())
  
  # reset all inputs ---- 
  observeEvent(input$reset, {
    updateSliderInput(session, "B", value = +0.5)
    updateSliderInput(session, "A", value = -0.5)
    
    updateSliderInput(session, "grand_i", value = 3)
    updateSliderInput(session, "stim_type_eff", value = 0.5)
    
    updateSelectInput(session, "coding", selected = "effect")
    
    updateSliderInput(session, "sub_n", value = 50)
    updateSliderInput(session, "stim_n", value = 10)
    
    updateSliderInput(session, "sub_sd", value = 0.85)
    updateSliderInput(session, "stim_sd", value = 0.75)
    updateSliderInput(session, "stim_type_sd", value = 0.50)
    updateSliderInput(session, "error_sd", value = 1.10)
  })
  
  # simulate data ----
  trials <- reactive({
    print("trials()")
    resim <- input$resim
    
    # simulate each trial
    sim_trials(sub_n = input$sub_n,
              sub_sd = input$sub_sd,
              stim_n = input$stim_n,
              stim_sd = input$stim_sd,
              stim_type_sd = input$stim_type_sd,
              error_sd = input$error_sd)
  })
  
  dat <- reactive({
    print("dat()")
    
    output$power_table <- renderTable({ tibble() })

    # calculate DV using current effect sizes and coding
    dat_code(trials(),
             grand_i = input$grand_i,
             stim_type_eff = input$stim_type_eff,
             B_code = 0.5, 
             A_code = -0.5)
  })
  
  # run LMER ----
  lmer_mod <- reactive({
    print("lmer_mod()")
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing LMER", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    progress$set(value = 0.5, detail = "running")
    
    sim_lmer(dat())
  })
  
  # get LMER summary ----
  lmer_text <- reactive({
    print("lmer_text()")
    summary(lmer_mod())
  })
  
  # Plots ----
  
  ## dat_plot ----
  output$dat_plot <- renderPlot({
    plot_dat(dat(), input$grand_i, input$dat_plot_view)
  }, height = function() {
    session$clientData$output_dat_plot_width*2/3
  })
  
  ## dat_sub_plot ----
  output$dat_sub_plot <- renderPlot({
    plot_dat(dat(), input$grand_i, input$dat_plot_view, "sub")
  }, height = function() {
    session$clientData$output_dat_sub_plot_width*2/3
  })
  
  ## dat_stim_plot ----
  output$dat_stim_plot <- renderPlot({
    plot_dat(dat(), input$grand_i, input$dat_plot_view, "stim")
  }, height = function() {
    session$clientData$output_dat_stim_plot_width*2/3
  })
  
  ## descr_table ----
  output$descr_table <- renderTable({
    descr(dat())
  }, digits = 2, width = "100%")
  
  ## sub_coef ----
  output$sub_coef <- renderTable({
    print("sim_sub_anova()")
    sim_sub_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`)
  }, digits = 3, width = "100%")
  
  ## stim_coef ----
  output$stim_coef <- renderTable({
    print("sim_stim_anova()")
    sim_stim_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`)
  }, digits = 3, width = "100%")
  
  ## lmer_coef ----
  output$lmer_coef <- renderTable({
    lmer_text()$coefficients %>% 
      as_tibble(rownames = "Effect") %>%
      filter(Effect != "(Intercept)") %>%
      rename(`p-value` = `Pr(>|t|)`)
  }, digits = 3, width = "100%")
  
  ## power_calc ----
  observeEvent(input$calc_power, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Running Simulation", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    n_reps <- input$n_reps
    
    dp <- purrr::map_df(1:n_reps, function(r) { 
      progress$set(value = r/n_reps, detail = paste(r, "of ", n_reps))
      if (r == n_reps) progress$close()
      sim_power(r, 
                sub_n = input$sub_n,
                sub_sd = input$sub_sd,
                stim_n = input$stim_n,
                stim_sd = input$stim_sd,
                stim_type_sd = input$stim_type_sd,
                error_sd = input$error_sd,
                grand_i = input$grand_i,
                stim_type_eff = input$stim_type_eff,
                A_code = -0.5,
                B_code = +0.5)
    })
    
    output$power_table <- renderTable({
      print("power_table()")
      
      dp %>% 
        filter(effect == "stim_type.code") %>%
        mutate(
          sub_n = input$sub_n,
          stim_n = input$stim_n,
          `main effect` = input$stim_type_eff,
          analysis = recode(analysis, 
                            "lmer" = "LMER",
                            "anova_sub" = "By-Subjects ANOVA",  
                            "anova_stim" = "By-Stimuli ANOVA")) %>%
        group_by(analysis, type, sub_n, stim_n, `main effect`) %>%
        summarise(sig = mean(p < input$alpha)) %>%
        spread(type, sig)
    }, digits = 2, width = "100%")
    
    output$power_plot_lmer <- renderPlot({
      plot_power_lmer(dp)
    }, height = function() {
      session$clientData$output_power_plot_lmer_width*2/3
    })
    
    output$power_plot_anova <- renderPlot({
      plot_power_anova(dp)
    }, height = function() {
      session$clientData$output_power_plot_anova_width*2/3
    })
  })
} # end server()

shinyApp(ui, server)