# . data_tab ----
data_tab <- tabItem(
  tabName = "data_tab",
  # . . data_table ----
  dataTableOutput("data_table"),
  downloadButton("download", "Download Session Data"),
  downloadButton("download_data", "Download All Data"),
  downloadButton("download_stats", "Download All Stats")
)