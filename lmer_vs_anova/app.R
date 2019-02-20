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

source("plot_tab.R")
source("lmer_tab.R")
source("fixef_tab.R")

about_tab <- tabItem(
  tabName = "about_tab",
  h3("About This Apps"),
  p("Stuff about this")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "LMER vs ANOVA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plots", tabName = "plot_tab"),
      menuItem("Fixed Effects", tabName = "fixef_tab"),
      menuItem("Random Effects", tabName = "lmer_tab"),
      menuItem("About", tabName = "about_tab"),
      div(style="display:inline-block; width: 50%;",
        actionButton("resim", "Re-Simulate")
      ),
      div(style="display:inline-block; width: 33%;",
        actionButton("reset", "Reset")
      ),
      # sample size input ----
      box(
        title = "Sample Size",
        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        sliderInput("sub_n", "Subjects per Group:", min = 10, max = 100, value = 25, step = 10),
        sliderInput("stim_n", "Stimuli:", min = 5, max = 100, value = 20, step = 5)
      ),
      # coding input ----
      box(
        title = "Coding",
        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        selectInput("coding", NULL, 
                    list("Effect/Deviation" = "effect", 
                         "Sum" = "sum", 
                         "Treatment" = "treatment",
                         "Custom" = "custom"), "effect"),
        sliderInput("easy", "Easy:", min = -1, max = 1, value = -0.5, step = 0.5),
        sliderInput("hard", "Hard:", min = -1, max = 1, value = +0.5, step = 0.5),
        sliderInput("congr", "Congruent:", min = -1, max = 1, value = -0.5, step = 0.5),
        sliderInput("incon", "Incongruent:", min = -1, max = 1, value = +0.5, step = 0.5)
      ),
      # fixed effects input ----
      box(
        title = "Fixed Effects",
        solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        
        # selectInput("fixef_type", "Specify By:", 
        #             list("Effects" = "effects", 
        #                  "Group Means" = "means"), "effects"),
        # div(id="specify_by_effects", 
          # sliderInput("grand_i", "Grand Intercept",
          #             min = 400, max = 800, value = 600, step = 10),
          # sliderInput("sub_cond_eff", "Condition", 
          #             min = -50, max = 50, value = 0, step = 5),
          # sliderInput("stim_version_eff", "Version", 
          #             min = -50, max = 50, value = 0, step = 5),
          # sliderInput("cond_version_ixn", "Condition:Version", 
          #             min = -50, max = 50, value = 0, step = 5)
        # ),
        # div(id="specify_by_means",
          sliderInput("easy_congr", "Mean for Easy Congruent",
                      min = 400, max = 800, value = 600, step = 10),
          sliderInput("easy_incon", "Mean for Easy Incongruent",
                      min = 400, max = 800, value = 600, step = 10),
          sliderInput("hard_congr", "Mean for Hard Congruent",
                      min = 400, max = 800, value = 600, step = 10),
          sliderInput("hard_incon", "Mean for Hard Incongruent",
                      min = 400, max = 800, value = 600, step = 10)
          
        # )
      ),
      
      # random effects input ----
      h4("Random Effects"),
      actionButton("ranef_zero", "Set All to 0"),
      box(
        title = "Intercept SDs",
        solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        sliderInput("sub_sd", "Subject:", min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_sd", "Stimulus:", min = 0, max = 100, value = 50, step = 10),
        sliderInput("error_sd", "Residual:", min = 0, max = 100, value = 50, step = 10)
      ),
      box(
        title = "Slope SDs",
        solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        sliderInput("sub_version_sd", "Subject (version):", 
                    min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_version_sd", "Stimulus (version):",
                    min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_cond_sd", "Stimulus (condition):",
                    min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_cond_version_sd", "Stimulus (condition:version):",
                    min = 0, max = 100, value = 50, step = 10)
      ),
      box(
        title = "Correlations",
        solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        sliderInput("sub_i_version_cor", "Subject Intercept and Slope (version):",
                    min = -1, max = 1, value = -0.2, step = 0.05),
        sliderInput("stim_i_cor", "Stimulus Intercept and Slopes:",
                    min = -1, max = 1, value = -0.2, step = 0.05),
        sliderInput("stim_s_cor", "Stimulus Slopes:",
                    min = -1, max = 1, value = 0.2, step = 0.05)
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      plot_tab,
      fixef_tab,
      lmer_tab,
      about_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  # reset all inputs ---- 
  observeEvent(input$reset, {
    updateSliderInput(session, "hard_congr", value = 600)
    updateSliderInput(session, "hard_incon", value = 600)
    updateSliderInput(session, "easy_congr", value = 600)
    updateSliderInput(session, "easy_incon", value = 600)
    
    updateSelectInput(session, "coding", selected = "effect")
    
    updateSliderInput(session, "sub_sd", value = 50)
    updateSliderInput(session, "sub_version_sd", value = 50)
    updateSliderInput(session, "stim_sd", value = 50)
    updateSliderInput(session, "stim_version_sd", value = 50)
    updateSliderInput(session, "stim_cond_sd", value = 50)
    updateSliderInput(session, "stim_cond_version_sd", value = 50)
    updateSliderInput(session, "error_sd", value = 50)
    
    updateSliderInput(session, "sub_i_version_cor", value = -0.2)
  })
  
  # toggle by effect and by means
  # observeEvent(input$fixef_type, {
  #   print("toggle fixef_type")
  #   
  #   if (input$fixef_type == "effects") {
  #     shinyjs::show("specify_by_effects")
  #     shinyjs::hide("specify_by_means")
  #   } else if (input$fixef_type == "means") {
  #     shinyjs::hide("specify_by_effects")
  #     shinyjs::show("specify_by_means")
  #   }
  # })
  
  # zero all SDs ---- 
  observeEvent(input$ranef_zero, {
    updateSliderInput(session, "sub_sd", value = 0)
    updateSliderInput(session, "sub_version_sd", value = 0)
    updateSliderInput(session, "stim_sd", value = 0)
    updateSliderInput(session, "stim_version_sd", value = 0)
    updateSliderInput(session, "stim_cond_sd", value = 0)
    updateSliderInput(session, "stim_cond_version_sd", value = 0)
    updateSliderInput(session, "error_sd", value = 10) # stats get wierd with 0 variance
  })
  
  # update coding ----
  observeEvent(input$coding, {
    print(input$coding)
    
    if (input$coding == "sum") {
      updateSliderInput(session, "easy", value = -1)
      updateSliderInput(session, "hard", value = +1)
      updateSliderInput(session, "congr", value = -1)
      updateSliderInput(session, "incon", value = +1)
    } else if (input$coding == "treatment") {
      updateSliderInput(session, "easy", value = 0)
      updateSliderInput(session, "hard", value = +1)
      updateSliderInput(session, "congr", value = 0)
      updateSliderInput(session, "incon", value = +1)
    } else { # effect or custom
      updateSliderInput(session, "easy", value = -0.5)
      updateSliderInput(session, "hard", value = +0.5)
      updateSliderInput(session, "congr", value = -0.5)
      updateSliderInput(session, "incon", value = +0.5)
    }
    
    slider_active = input$coding == "custom"
    shinyjs::toggleState("easy", slider_active)
    shinyjs::toggleState("hard", slider_active)
    shinyjs::toggleState("congr", slider_active)
    shinyjs::toggleState("incon", slider_active)
    
  })
  
  # update simple means ----
  # observeEvent(c(input$grand_i,
  #                input$sub_cond_eff,
  #                input$stim_version_eff,
  #                input$cond_version_ixn), {
  #   print("effects_to_simple_means")
  #   new_eff <- effects_to_simple_means(
  #     input$grand_i,
  #     input$sub_cond_eff,
  #     input$stim_version_eff,
  #     input$cond_version_ixn,
  #     input$hard, 
  #     input$easy,
  #     input$congr,
  #     input$incon
  #   )
  #   
  #   updateSliderInput(session, "hard_congr", value = new_eff$hard_congr)
  #   updateSliderInput(session, "hard_incon", value = new_eff$hard_incon)
  #   updateSliderInput(session, "easy_congr", value = new_eff$easy_congr)
  #   updateSliderInput(session, "easy_incon", value = new_eff$easy_incon)
  # }, ignoreInit = TRUE)
  
  # update effects ----
  observeEvent(c(input$hard_congr,
                 input$hard_incon,
                 input$easy_congr,
                 input$easy_incon), {
    print("simple_means_to_effects")
    new_eff <- simple_means_to_effects(
      input$hard_congr,
      input$hard_incon,
      input$easy_congr,
      input$easy_incon
    )
    
    output$effect_sizes <- renderUI({
      tagList(
        tags$span(paste("Intercept =", new_eff$grand_i)),
        tags$br(),
        tags$span(paste("Condition =", new_eff$sub_cond_eff)),
        tags$span("(hard - easy)"),
        tags$br(),
        tags$span(paste("Version =", new_eff$stim_version_eff)),
        tags$span("(incon - congr)"),
        tags$br(),
        tags$span(paste("Cond:Vers =", new_eff$cond_version_ixn))
      )
    })

    # updateSliderInput(session, "grand_i", value = new_eff$grand_i)
    # updateSliderInput(session, "sub_cond_eff", value = new_eff$sub_cond_eff)
    # updateSliderInput(session, "stim_version_eff", value = new_eff$stim_version_eff)
    # updateSliderInput(session, "cond_version_ixn", value = new_eff$cond_version_ixn)
  })
  # 
  # simulate data ----
  trials <- reactive({
    print("trials()")
    resim <- input$resim
    
    # simulate each trial
    sim_trials(sub_n = input$sub_n,
              sub_sd = input$sub_sd,
              sub_version_sd = input$sub_version_sd, 
              sub_i_version_cor = input$sub_i_version_cor,
              stim_n = input$stim_n,
              stim_sd = input$stim_sd,
              stim_version_sd = input$stim_version_sd,
              stim_cond_sd = input$stim_cond_sd,
              stim_cond_version_sd = input$stim_cond_version_sd,
              stim_i_cor = input$stim_i_cor,
              stim_s_cor = input$stim_s_cor,
              error_sd = input$error_sd)
  })
  
  dat <- reactive({
    print("dat()")
    
    output$lmer_coef <- renderTable({ tibble() })
    
    new_eff <- simple_means_to_effects(
      input$hard_congr,
      input$hard_incon,
      input$easy_congr,
      input$easy_incon
    )
    
    # calculate DV using current effect sizes and coding
    dat_code(trials(),
             grand_i = new_eff$grand_i,
             sub_cond_eff = new_eff$sub_cond_eff,
             stim_version_eff = new_eff$stim_version_eff,
             cond_version_ixn = new_eff$cond_version_ixn,
             hard = input$hard * 1.0, 
             easy = input$easy * 1.0,
             congr = input$congr * 1.0,
             incon = input$incon * 1.0)
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
    new_eff <- simple_means_to_effects(
      input$hard_congr,
      input$hard_incon,
      input$easy_congr,
      input$easy_incon
    )
    plot_dat(dat(), new_eff$grand_i, input$dat_plot_view)
  }, height = function() {
    session$clientData$output_dat_plot_width*2/3
  })
  
  ## dat_sub_plot ----
  output$dat_sub_plot <- renderPlot({
    new_eff <- simple_means_to_effects(
      input$hard_congr,
      input$hard_incon,
      input$easy_congr,
      input$easy_incon
    )
    plot_dat(dat(), new_eff$grand_i, input$dat_plot_view, "sub")
  }, height = function() {
    session$clientData$output_dat_sub_plot_width*2/3
  })
  
  ## dat_stim_plot ----
  output$dat_stim_plot <- renderPlot({
    new_eff <- simple_means_to_effects(
      input$hard_congr,
      input$hard_incon,
      input$easy_congr,
      input$easy_incon
    )
    plot_dat(dat(), new_eff$grand_i, input$dat_plot_view, "stim")
  }, height = function() {
    session$clientData$output_dat_stim_plot_width*2/3
  })
  
  ## ranef_sub_plot ----
  output$ranef_sub_plot <- renderPlot({
    plot_ranef_sub(lmer_mod(), dat())
  }, height = function() {
    session$clientData$output_ranef_sub_plot_width
  })
  
  ## ranef_stim_plot ----
  output$ranef_stim_plot <- renderPlot({
    plot_ranef_stim(lmer_mod(), dat())
  }, height = function() {
    session$clientData$output_ranef_stim_plot_width
  })
  
  # Tables ----
  
  ## lmer_varcor ----
  # output$lmer_varcor <- renderTable({
  #   lmer_text()$varcor %>% 
  #     as_tibble(rownames = "Effect") %>%
  #     select(-vcov) %>%
  #     mutate(type = ifelse(is.na(var2), "sd", "cor")) %>%
  #     spread(type, sdcor) %>%
  #     mutate(var1 = ifelse(is.na(var1), "", var1),
  #            var2 = ifelse(is.na(var2), "", var2)) %>%
  #     arrange(grp, var1, var2)
  # })
  
  ## descr_table ----
  output$descr_table <- renderTable({
    descr(dat())
  }, digits = 1, width = "100%")
  
  ## dat_table ----
  output$dat_table <- renderTable({
    dat() %>%
      select(sub_id, stim_id, 
             sub_cond, sub_cond.code,
             stim_version, stim_version.code,
             err, dv)
  }, digits = 1, width = "100%")
  
  ## sub_table ----
  output$sub_dat_table <- renderTable({
    dat() %>%
      group_by(sub_id, sub_i, sub_cond, sub_version_slope) %>%
      summarise()
  }, digits = 1, width = "100%")
  
  ## stim_dat_table ----
  output$stim_dat_table <- renderTable({
    dat() %>%
      group_by(stim_id, stim_i, stim_version_slope, stim_cond_slope, stim_cond_version_slope) %>%
      summarise()
  }, digits = 1, width = "100%")
  
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
  observeEvent(input$calc_fixed, {
    output$lmer_coef <- renderTable({
      isolate(lmer_text())$coefficients %>% 
        as_tibble(rownames = "Effect") %>%
        rename(`p-value` = `Pr(>|t|)`)
    }, digits = 3, width = "100%")
  })
  

  
} # end server()

shinyApp(ui, server)