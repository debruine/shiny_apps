### withincor_tab ----
withincor_tab <- tabItem(
  tabName = "withincor_tab",
  tags$span("This app lets you try out the code from Daniël Lakens' blog post: "),
  tags$a(href = "http://daniellakens.blogspot.com/2016/11/why-within-subject-designs-require-less.html",
         "Why Within-Subject Designs Require Fewer Participants than Between-Subject Designs."),
  p("Try changing the correlation and see how the SD of the difference scores
    changes and what effect this has on the test statistic."),
  fluidRow( # start row 1
    column( # start column 1
      width = 3,
      box(
        title = "Parameters",
        width = NULL,
        sliderInput("withincor_r", "Correlation:", min = -1, max = 1, value = 0.25, step = 0.01),
        sliderInput("withincor_n", "N:", min = 0, max = 1e4, value = 1000, step = 50),
        sliderInput("withincor_m1", "Mean 1:", min = 50, max = 150, value = 100, step = 1),
        sliderInput("withincor_sd1", "SD 1:", min = 0, max = 20, value = 10, step = 1),
        sliderInput("withincor_m2", "Mean 2:", min = 50, max = 150, value = 105, step = 1),
        sliderInput("withincor_sd2", "SD 2:", min = 0, max = 20, value = 10, step = 1),
        actionButton("withincor_resim", "Re-Simulate")
      )
    ), # end column 1
    column( # start column 2
      width = 5,
      box(
        title = "Data",
        width = NULL,
        plotOutput(outputId = "withincor_plot1", height = "auto")
      ),
      box(
        title = "Difference Scores",
        width = NULL,
        plotOutput(outputId = "withincor_plot2", height = "auto")
      )
    ), # end column 2
    column( # start column 3
      width = 4,
      box(
        title = "T-Test",
        width = NULL,
        textOutput(outputId = "ttext")
      ),
      box(
        title = "Correlation",
        width = NULL,
        plotOutput(outputId = "withincor_plot3", height = "auto")
      )
    ) # end column 3
  ) # end row 1
)

