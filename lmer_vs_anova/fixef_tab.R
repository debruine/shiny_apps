### fixef_tab ----
fixef_tab <- tabItem(
  tabName = "fixef_tab",
  fluidRow( # start row 1
    column(
      width = 12,
      box(
        title = "By-Subjects ANOVA",
        width = 12,
        tableOutput("sub_coef")
      ),
      box(
        title = "By-Stimuli ANOVA",
        width = 12,
        tableOutput("stim_coef")
      ),
      box(
        title = "LMER",
        width = 12,
        div(style="float: right; width: 10em;",
            actionButton("calc_fixed", "Calculate LMER")
        ),
        p("Mixed effects models take a while to run, so this won't update automatically every time you change parameters like the ANOVAs above."),
        tableOutput("lmer_coef")
      )
    )
  ) # end row 1
)
