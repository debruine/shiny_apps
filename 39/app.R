# Libraries ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(packcircles)
library(ggiraph)
library(ggraph)
library(igraph)

lewitt <- c(
    "#CC2738", # red
    "#63B024", # green
    "#0B75D8", # blue
    "#BBBFC3", # grey
    "#CA2838", # red
    "#FDF25A", # yellow
    "#76C234", # green
    "#F16C43", # orange
    "#6767D4" # blue
)

# . dashboardPage ----
ui <- dashboardPage(
    dashboardHeader(title = "Happy Birthday"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        actionButton("spiky_reload", "Reload"),
        plotOutput("spiky"),
        sliderInput("n_circles", "Number of Circles", 
                    min = 1, max = 200, value = 100),
        plotOutput("circles"),
        actionButton("circles2_reload", "Reload"),
        plotOutput("circles2"),
        sliderInput("n_stripes", "Number of Stripes", 
                    min = 1, max = 100, value = 30),
        plotOutput("stripes")
    ),
    title = "Happy Birthday"
)

# Define server logic ----
server <- function(input, output, session) {
    output$spiky <- renderPlot({
        input$spiky_reload
        
        ngroup <- 18
        names=paste("G_",seq(1,ngroup),sep="")
        DAT <- data.frame()
        
        for(i in seq(1:ngroup)){
            data=data.frame( matrix(0, ngroup , 3))
            data[,1]=i
            data[,2]=sample(names, nrow(data))
            data[,3]=prop.table(sample( c(rep(0,100),c(1:ngroup)) ,nrow(data)))
            DAT=rbind(DAT,data)
        }
        colnames(DAT)=c("Year","Group","Value")
        d <- DAT[order( DAT$Year, DAT$Group) , ]
        
        ggplot(d, aes(x=Year, y=Value, fill=Group )) + 
            geom_area(alpha=1, color = "black", size = 1) + 
            theme_bw() + 
            scale_fill_manual(values = rep(lewitt, 3))+ 
            theme(line = element_blank(), 
                  text = element_blank(), 
                  title = element_blank(), 
                  legend.position="none", 
                  panel.border = element_blank(), 
                  panel.background = element_blank()
                  )
    })
    
    output$circles <- renderPlot({
        # Create data
        sizes <- sample(1:100, input$n_circles, replace = TRUE)
        
        # Generate the layout
        packing <- circleProgressiveLayout(sizes, sizetype='area')
        dat.gg <- circleLayoutVertices(packing, npoints=50)
    
        # Make the plot with a few differences compared to the static version:
        ggplot() + 
            geom_polygon_interactive(data = dat.gg, 
                                     aes(x, y, group = id, fill=as.factor(id), 
                                         data_id = id), 
                                     colour = "black", size = 1) +
            scale_fill_manual(values = rep_len(lewitt, input$n_circles)) +
            theme_void() + 
            theme(legend.position="none", 
                  plot.margin=unit(c(0,0,0,0),"cm") ) + 
            coord_equal()
    })
    
    output$circles2 <- renderPlot({
        input$circles2_reload
        
        edges=flare$edges
        vertices = flare$vertices
        mygraph <- graph_from_data_frame( edges, vertices=vertices )
        
        # Control the size of each circle: (use the size column of the vertices data frame)
        ggraph(mygraph, layout = 'circlepack', weight="size") + 
            geom_node_circle(aes(fill = sample(LETTERS[1:length(lewitt)], 
                                               nrow(vertices), 
                                               replace = T)), 
                             size = 1) +
            scale_fill_manual(values = lewitt) +
            theme_void() + 
            coord_equal() +
            theme(legend.position="FALSE")
    })
    
    output$stripes <- renderPlot({
        par(bg="black")
        par(mar=c(0,0,0,0))
        plot(1, 1, xlim=c(0,1), ylim=c(0,1), col="black")
        for(i in 0:input$n_stripes-1){
            abline(v = i/input$n_stripes, 
                   col = lewitt[i%%9+1], 
                   lwd = 2000/input$n_stripes)
        }
    })
}

shinyApp(ui, server)

