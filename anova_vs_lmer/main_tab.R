### main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  h3("Instructions"),
  p("Set the parameters in the sidebar menu for a face rating task where people (subjects) rate faces (stimuli) of each of two types (A and B). The dependent variable (DV) is rating (1-7)."),
  p("The simulation defaults to an effect for stimulus type of 0.5 (a half-point difference between stimuli of type A and B) and random effects with the same SDs as the open data from the Face Research London Set. You can change these defaults under the Fixed Effects or Random Effects tab."),
  tags$a(href="https://figshare.com/articles/Face_Research_Lab_London_Set/5047666", "Face Research London Set on Figshare"),
  h3("Compare False Positive Rate and Power"),
  p("This function will run the set number of simulations with the parameters you've set and report the proportion of runs that gave a significant effect of stimulus type (given the alpha you set). It will also report the false positive rate for the same simulations with the main effect of stimulus type set to 0. If you set the main effect of stimulus type to 0, then power will be equal to the false positive rate."),
  fluidRow(
    column(
      width = 2,
      actionButton("calc_power", "Calculate")
    ),
    column(
      width = 5,
      sliderInput("n_reps", "Number of Simulations to Run:", 
                  min = 10, max = 500, value = 100, step = 10)
    ),
    column(
      width = 5,
      sliderInput("alpha", "(Justify Your) Alpha",
                  min = .005, max = .100, value = 0.05, step = 0.005)
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "False Positive/Power Calculations",
        width = NULL,
        tableOutput("power_table")
      )
    ),
    column(
      width = 4,
      p("It is not an error that the false positive rate for the by-subjects ANOVA is very high. With this type of within-subjects, between-stimulus design, you can get very high false positive rates if stimuli have some variation in their mean DV (i.e., where faces tend to vary in attractiveness). For this type of design (no between-subject factors), the by-stimulus ANOVA will tend to have a nominal false positive rate, but will have the same type of inflated false positive rate for designs with between-subject factors where subjects have some random variation in their mean repsonses."),
      p("If you set the Stimulus Intercept SD to 0, you will see that the by-subjects ANOVA has a false positive rate closer to the nominal alpha (defaults to 0.05). However, this models a very unrealistic situation where the variation in attractivenes of your stimuli is 0.")
    )
  ),
  h4("Simulated Effect Size Distribution"),
  fluidRow(
    column(
      width = 6,
      plotOutput(outputId = "power_plot_anova", height = "auto")
    ), 
    column(
      width = 6,
      plotOutput(outputId = "power_plot_lmer", height = "auto")
    )
  ),
  
  h3("Compare ANOVAs and LMER for an individual simulation"),
  actionButton("resim", "Re-Simulate"),
  fluidRow(
    box(
      title = "Descriptives",
      width = 8,
      tableOutput("descr_table")
    ),
    box(
      title = "Plot Type",
      width = 4,
      checkboxGroupInput(
        "dat_plot_view", 
        "View:",
        c("violin" = "violin",
          "boxplot" = "boxplot"),
        selected = c("violin", "boxplot")
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "By-Stimuli ANOVA",
        width = NULL,
        tableOutput("stim_coef")
      )
    ),
    column(
      width = 4,
      box(
        title = "Aggregated by Stimuli",
        width = NULL,
        plotOutput(outputId = "dat_stim_plot", height = "auto")
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "By-Subjects ANOVA",
        width = NULL,
        tableOutput("sub_coef")
      )
    ),
    column(
      width = 4,
      box(
        title = "Aggregated by Subject",
        width = NULL,
        plotOutput(outputId = "dat_sub_plot", height = "auto")
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "LMER",
        width = NULL,
        p("The p-value for the main effect in LMER will be identical to the by-stimuli ANOVA if the random slope for the main effect is set to zero (i.e., where between-subject variation in the effect of stimulus type is 0)."),
        tableOutput("lmer_coef")
      )
    ),
    column(
      width = 4,
      box(
          title = "Not Aggregated",
          width = NULL,
          plotOutput(outputId = "dat_plot", height = "auto")
      )
    )
  )
)
