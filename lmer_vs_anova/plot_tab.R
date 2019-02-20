### plot_tab ----
plot_tab <- tabItem(
  tabName = "plot_tab",
  p("Set the parameters in the sidebar menu for a Stroop task where people (subjects) say the colour of colour words (stimuli) shown in each of two versions (congruent and incongruent). Subjects are in one of two conditions (hard and easy). The dependent variable (DV) is reaction time."),
  fluidRow( # start row 1
    column(
      width = 2,
      box(
        title = "Plot Type",
        width = NULL,
        checkboxGroupInput(
          "dat_plot_view", 
          "View:",
           c("violin" = "violin",
             "boxplot" = "boxplot"),
           selected = c("violin", "boxplot")
        )
      )
    ),
    column(
      width = 2,
      box(
        title = "Fixed Effects",
        width = NULL,
        htmlOutput("effect_sizes")
      )
    ),
    column(
      width = 8,
      box(
        title = "Descriptives",
        width = NULL,
        tableOutput("descr_table")
      )
    )
  ), # end row 1
  fluidRow( # start row 2
    column(
      width = 4,
      box(
        title = "Not Aggregated",
        width = NULL,
        plotOutput(outputId = "dat_plot", height = "auto")
      )
    ),
    column(
      width = 4,
      box(
        title = "Aggregated by Subject",
        width = NULL,
        plotOutput(outputId = "dat_sub_plot", height = "auto")
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
  ), # end row 2
  fluidRow( # start row 3
    column(
      width = 12,
      box(
        title = "Data",
        width = NULL,
        tabsetPanel(type = "tabs",
                    tabPanel("Trials", tableOutput("dat_table")),
                    tabPanel("Subjects", tableOutput("sub_dat_table")),
                    tabPanel("Stimuli", tableOutput("stim_dat_table"))
        )
      )
    )
  ) # end row 3
)
