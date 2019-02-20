### lmer_tab ----
lmer_tab <- tabItem(
  tabName = "lmer_tab",
  fluidRow(
    column(
      width = 4,
      plotOutput(outputId = "ranef_sub_plot", height = "auto")
    ),
    column(
      width = 8,
      plotOutput(outputId = "ranef_stim_plot", height = "auto")
    )
  ),
  box(
    title = "VarCor",
    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
    width = 12,
    tableOutput("lmer_varcor")
  )
)
