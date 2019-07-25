# . main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  # . . instructions ----
  box(title = "Instructions", width = 12,
      p("Your task is to guess whether there is a real difference between two groups, one represented by red squares, and one represented by blue circles. To inform your guess, you will sample individual data points from each group."),
      p("The real difference between the two groups will be randomly decided by the app (and shown after you made your decision). The difference is either an effect size of 0, 0.2, 0.5, or 0.8. If there is an effect, it can be positive or negative (i.e., squares can have a higher or lower means than circles)."),
      p("You should sample data until you are 80% certain about your decision about whether there is a real difference or not. If you do this task 30 times, you should guess correctly 24 of the 30 times."),
      p("Click the 'Next Trial' button to start, and click the 'Sample Again' button until you are 80% certain of your choice. Afterwards, the app will reveal whether you were correct or not. You can click the 'Next Trial' button to start again. The app will keep track of your performance."),
      collapsible = TRUE, collapsed = TRUE
  ),
  
  
  fluidRow(
    column(
      width = 4,
      # . . trinary input ----
      box(id = "trinary_input",
          width = NULL,
          h4("Which group is larger?"),
          # . . trinary guess buttons ----
          actionButton("guess_A", "A", width = "32%"),
          actionButton("guess_0", "A = B", width = "32%"),
          actionButton("guess_B", "B", width = "32%")
      ),
      # . . continuous input ----
      box(id = "continuous_input",
          width = NULL,
          h4("How much larger is group B (blue circle) than group A (red square)?"),
          # . . d_guess slide ----
          sliderInput("d_guess", NULL, 
                      min = -1, max = 1, value = 0, step = 0.1)
      ),
      # . . sample_again button ----
      actionButton("sample_again", "Sample Again", width = "49%"),
      # . . submit_guess button ----
      actionButton("submit_guess", "Submit Guess", width = "49%"),
      # . . next_trial button ----
      actionButton("next_trial", "Next Trial", width = "49%")
    ),
    column(
      width = 8,
      box(
        width = NULL,
        # . . current_plot plot ----
        plotOutput("current_plot")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      # . . values output ----
      valueBoxOutput("samplesBox", width = 4),
      valueBoxOutput("guessBox", width = 4),
      valueBoxOutput("esBox", width = 4),
      htmlOutput("feedback")
    ),
    column(
      width = 6,
      # . performance plot ----
      box(
        width = NULL,
        h4(textOutput("guess_correct")),
        # . . performance_plot plot ----
        plotOutput("performance_plot")
      )
    )
  )
)
