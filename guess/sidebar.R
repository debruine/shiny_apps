# . Sidebar ----
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Guess", tabName = "main_tab"),
    menuItem("Data", tabName = "data_tab")
  ),
  
  checkboxInput("show_debug", "Debug", value = T),
  tags$ul(
    tags$li(textOutput("debug_es")),
    tags$li(textOutput("debug_offset")),
    tags$li(textOutput("debug_sd"))
  ),
  
  tags$a("Code on GitHub", href = "https://github.com/debruine/shiny/blob/master/guess/app.R"),
  h4("Display Options"),
  checkboxInput("show_violin", "Violin Plot",value = T),
  checkboxInput("show_boxplot", "BoxPlot",value = F),
  checkboxInput("show_points", "Points",value = T),
  checkboxInput("show_barplot", "Bar Plot",value = F),
  sliderInput("n_obs", "Observations per group", min = 1, max = 100, value = 1, step = 1),
  sliderInput("max_samples", "Maximum Samples", min = 1, max = 200, value = 200, step = 1),
  checkboxInput("one_two", "One at a time",value = T),
  checkboxInput("trinary", "Trinary Input",value = T),
  checkboxInput("accumulate", "Accumulate",value = T),
  sliderInput("prob_null", "Null probability", min = 0, max = 100, value = 50, step = 5),
  p("This app is storing your data.")
)
