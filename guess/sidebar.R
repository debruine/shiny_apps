# . Defaults ----
def <- list(
  show_violin = F,
  show_boxplot = F,
  show_points = T,
  show_barplot = F,
  show_meanse = F,
  n_obs = 1,
  max_samples = 10000,
  one_two = T,
  trinary = T,
  accumulate = F,
  prob_null = 50,
  show_debug = F
)

# . Sidebar ----
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Guess", tabName = "main_tab"),
    menuItem("Data", tabName = "data_tab")
  ),
  
  actionButton("setting_1", "1: Trinary", width = "80%"),
  actionButton("setting_2", "2: Accumulate", width = "80%"),
  actionButton("setting_3", "3: Septenary", width = "80%"),
  actionButton("setting_c", "Septenary 1 Sample", width = "80%"),
  actionButton("setting_debug", "Debug", width = "80%"),
  checkboxInput("show_debug", "Debug", value = def$show_debug),
  
  tags$a("Code on GitHub", href = "https://github.com/debruine/shiny/blob/master/guess/app.R"),
  h4("Display Options"),
  checkboxInput("show_meanse", "Mean+SE",value = def$show_meanse),
  checkboxInput("show_violin", "Violin Plot",value = def$show_violin),
  checkboxInput("show_boxplot", "BoxPlot",value = def$show_boxplot),
  checkboxInput("show_points", "Points",value = def$show_points),
  checkboxInput("show_barplot", "Bar Plot",value = def$show_barplot),
  sliderInput("n_obs", "Observations per group", 
              min = 1, max = 100, value = def$n_obs, step = 1),
  numericInput("max_samples", "Maximum Samples", 
              min = 1, max = 10000, value = def$max_samples, step = 1),
  checkboxInput("one_two", "One at a time",value = def$one_two),
  checkboxInput("trinary", "Trinary Input",value = def$trinary),
  checkboxInput("accumulate", "Accumulate",value = def$accumulate),
  sliderInput("prob_null", "Null probability", 
              min = 0, max = 100, value = def$prob_null, step = 5),
  p("This app is storing your data.")
)
