sidebar <- dashboardSidebar(
  # https://fontawesome.com/icons?d=gallery&m=free
  sidebarMenu(
    menuItem("Home", tabName = "main_tab", icon = icon("home")),
    menuItem("Normal", tabName = "norm_tab", icon = icon("ghost")),
    menuItem("Binomial", tabName = "binom_tab", icon = icon("hand-peace")),
    menuItem("Poisson", tabName = "pois_tab", icon = icon("fish")),
    menuItem("Code on Github", href = "https://github.com/debruine/shiny/", icon = icon("github"))
  )
)

