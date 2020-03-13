# Libraries ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidytext) # for transforming text
library(dplyr) # for data wrangling 
library(ggwordcloud) # to render wordclouds

# default text
demo_text <- ""


# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "Congratulations!"),
  dashboardSidebar(
    textAreaInput("theText", 
                  "Enter text here", 
                  value = demo_text, 
                  height = "200px"),
    sliderInput("minWord", "Omit words that occur less than", 1, 20, 3),
    sliderInput("topWords", "Maximum number of words to include", 1, 100, 100),
    sliderInput("maxTextSize", "Maximum text size", 10, 100, 40),
    sliderInput("plotHeight", "Aspect Ratio 1:", 0.5, 2, 1, .05),
    selectInput("colourScheme", "Colour Scheme", 
                list(
                  "Default" = 1,
                  "Viridis"= 2,
                  "Magma" = 3,
                  "Inferno" = 4,
                  "Plasma" = 5,
                  "PsyTeachR" = 6
                ),
                selected = 1
    ),
    HTML("Download the <a href='https://github.com/debruine/shiny/tree/master/wordcloud'>app code here<a/>.")
  ),
  dashboardBody(
    plotOutput("wordCloudPlot")
  ),
  title = "Congratulations!"
)

# Define server logic ----
server <- function(input, output, session) {
  output$wordCloudPlot <- renderPlot({
    # process the text
    text_table <- tibble(text = input$theText)
    
    # this is better than the next two lines, but requires tidytext, 
    #   which isn't installed on our shiny server
    word_table <- unnest_tokens(text_table, "word", "text") %>%
      #words <- input$theText %>% tolower() %>% strsplit("[^a-zA-Z]+")
      #word_table <- tibble(word = words[[1]]) %>%
      count(word) %>%
      filter(n >= input$minWord) %>% 
      arrange(desc(n)) %>%
      head(input$topWords)
    
    # create the ggwordcloud
    thePlot <- ggplot(word_table, aes(label = word, color = word, size = n)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = input$maxTextSize) +
      theme_minimal()
    
    # set the colour scheme
    if (input$colourScheme == 2) {
      thePlot <- thePlot + scale_colour_viridis_d()
    } else if (input$colourScheme == 3) {
      thePlot <- thePlot + scale_colour_viridis_d(option = "A")
    } else if (input$colourScheme == 4) {
      thePlot <- thePlot + scale_colour_viridis_d(option = "B")
    } else if (input$colourScheme == 5) {
      thePlot <- thePlot + scale_colour_viridis_d(option = "C")
    } else if (input$colourScheme == 6) {
      ptrc <- c("#983E82","#E2A458","#F5DC70","#59935B","#467AAC","#61589C")
      palette <- rep(ptrc, length.out = nrow(word_table))
      thePlot <- thePlot + scale_colour_manual(values = palette)
    }
    
    thePlot
  }, 
  width = "auto", 
  height = function() {
    session$clientData$output_wordCloudPlot_width*input$plotHeight
  })
}

shinyApp(ui, server)

