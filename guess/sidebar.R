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
  checkboxInput("accumulate", "Accumulate",value = F),
  sliderInput("prob_null", "Null probability", min = 0, max = 100, value = 50, step = 5),
  p("This app is not storing your data beyond this session.")
)
