# . main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  # . . instructions ----
  # box(title = "Instructions", width = 12,
  #     p("Your task is to guess whether there is a real difference between two groups, one represented by red squares, and one represented by blue circles. To inform your guess, you will sample individual data points from each group."),
  #     p("The real difference between the two groups will be randomly decided by the app (and shown after you made your decision). The difference is either an effect size of 0, 0.2, 0.5, or 0.8. If there is an effect, it can be positive or negative (i.e., squares can have a higher or lower means than circles)."),
  #     p("You should sample data until you are 80% certain about your decision about whether there is a real difference or not. If you do this task 30 times, you should guess correctly 24 of the 30 times."),
  #     p("Click the 'New Trial' button to start, and click the 'Draw New Sample' button until you are 80% certain of your choice. Afterwards, the app will reveal whether you were correct or not. You can click the 'New Trial' button to start again. The app will keep track of your performance."),
  #     collapsible = TRUE, collapsed = TRUE
  # ),
  
  
  fluidRow(
    column(
      width = 5,
      # . . trinary input ----
      box(id = "trinary_input",
          width = NULL,
          h4("Guess the Effect"),
          # . . trinary guess buttons ----
          actionButton("guess_A", "A > B", width = "32%"),
          actionButton("guess_0", "A = B", width = "32%"),
          actionButton("guess_B", "B > A", width = "32%")
      ),
      # . . continuous input ----
      box(id = "continuous_input",
          width = NULL,
          h4("What is your best guess of the true population effect size, expressed in Cohen's d?"),
          # . . d_guess slide ----
          sliderInput("d_guess", NULL, min = -1, max = 1, value = 0, step = 0.1),
          # . . septenary guess buttons ----
          span("A>B", style="float: left; color: #DD4B39;"),
          span("B>A", style="float: right; color: #0073B7;"),
          hr(),
          div(id = "septernary_buttons",
            actionButton("guess_A8", "-0.8", width = "13%"),
            actionButton("guess_A5", "-0.5", width = "13%"),
            actionButton("guess_A2", "-0.2", width = "13%"),
            actionButton("guess_00", "0", width = "13%"),
            actionButton("guess_B2", "+0.2", width = "13%"),
            actionButton("guess_B5", "+0.5", width = "13%"),
            actionButton("guess_B8", "+0.8", width = "13%"),
            style="text-align: center;"
          ),
          # . . submit_guess button ----
          actionButton("submit_guess", "Submit Guess", width = "100%")
      ),
      # . . sample_again button ----
      actionButton("sample_again", "Draw New Sample", width = "100%"),
      # . . next_trial button ----
      actionButton("next_trial", "New Trial", width = "100%")
    ),
    column(
      width = 7,
      box(
        width = NULL,
        # . . current_plot plot ----
        plotOutput("current_plot")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      # . . values output ----
      htmlOutput("guess_correct"),
      valueBoxOutput("samplesBox", width = 4),
      valueBoxOutput("guessBox", width = 4),
      valueBoxOutput("esBox", width = 4),
      htmlOutput("feedback")
    )
  ),
  fluidRow(
    column(
      width = 12,
      # . performance plot ----
      box(id = "perf_plot_box",
        width = NULL,
        # . . performance_plot plot ----
        plotOutput("performance_plot")
      )
    )
  )
)
