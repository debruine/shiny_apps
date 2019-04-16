#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidytext) # for transforming text
library(dplyr) # for data wrangling 
library(ggwordcloud) # to render wordclouds

# default text
africa_by_toto <- "I hear the drums echoing tonight
But she hears only whispers of some quiet conversation
She's coming in, 12:30 flight
The moonlit wings reflect the stars that guide me towards salvation
I stopped an old man along the way
Hoping to find some old forgotten words or ancient melodies
He turned to me as if to say, Hurry boy, it's waiting there for you
It's gonna take a lot to drag me away from you
There's nothing that a hundred men or more could ever do
I bless the rains down in Africa
Gonna take some time to do the things we never had (ooh, ooh)
The wild dogs cry out in the night
As they grow restless, longing for some solitary company
I know that I must do what's right
As sure as Kilimanjaro rises like Olympus above the Serengeti
I seek to cure what's deep inside, frightened of this thing that I've become
It's gonna take a lot to drag me away from you
There's nothing that a hundred men or more could ever do
I bless the rains down in Africa
Gonna take some time to do the things we never had (ooh, ooh)
Hurry boy, she's waiting there for you
It's gonna take a lot to drag me away from you
There's nothing that a hundred men or more could ever do
I bless the rains down in Africa
I bless the rains down in Africa
(I bless the rain)
I bless the rains down in Africa (I bless the rain)
I bless the rains down in Africa
I bless the rains down in Africa (ah, gonna take the time)
Gonna take some time to do the things we never had (ooh, ooh)"


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Word Cloud App"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textAreaInput("theText", 
                       "Enter text here", 
                       value = africa_by_toto, 
                       height = "200px"),
         sliderInput("minWord", "Omit words that occur less than", 1, 20, 3),
         sliderInput("topWords", "Maximum number of words to include", 1, 100, 100),
         sliderInput("maxTextSize", "Maximum text size", 10, 100, 30),
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
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("wordCloudPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
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
   }, width = "auto", height = "auto")
}

# Run the application 
shinyApp(ui = ui, server = server)

