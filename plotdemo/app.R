## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                              1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# from https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

# setup theme ----

bgcolor <- "black"
textcolor <- "white"
my_theme <- theme(
  plot.background = element_rect(fill = bgcolor, colour = bgcolor),
  panel.background = element_rect(fill = NA),
  legend.background = element_rect(fill = NA),
  legend.position="none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  text = element_text(colour = textcolor, size=20),
  axis.text = element_text(colour = textcolor, size=15)
)

my_fill <- scale_fill_manual(values = c("#3D99CC", "#898710"))

makeData <- function(n, group_effect, stim_effect, ixn_effect) {
  data <<- tibble(
    group = rep(c("A", "B"), n),
    stim_group = rep(c("A", "B"), each = n)
  ) %>%
    mutate(
      dv = rnorm(n*2, 0, 1),
      group_ef = ifelse(group=="B", group_effect/2, -group_effect/2),
      stim_ef = ifelse(stim_group=="B", stim_effect/2, -stim_effect/2),
      ixn = ifelse(group==stim_group, ixn_effect/2, -ixn_effect/2),
      dv = dv + group_ef + stim_ef + ixn
    )
  
  summary_data <<- data %>%
    group_by(group, stim_group) %>%
    summarise(
      mean = mean(dv),
      min = mean(dv) - qnorm(0.975)*sd(dv)/sqrt(n()),
      max = mean(dv) + qnorm(0.975)*sd(dv)/sqrt(n())
    )
}

data <- NULL
summary_data <- NULL

makeData(100, 0, 0, 0.5)

# ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Plot Demo"),
  dashboardSidebar(
    sliderInput("n", "Number of participants", min = 0, max = 1000, step = 10, value = 100),
    sliderInput("group_effect", "Group Main Effect", min = -2, max = 2, step = 0.1, value = 0),
    sliderInput("stim_effect", "Task Main Effect", min = -2, max = 2, step = 0.1, value = 0),
    sliderInput("ixn", "Group x Task Interaction", min = -2, max = 2, step = 0.1, value = 0.5),
    actionButton("rerun", "Re-run Simulation")
  ),
  dashboardBody(
    p("This app simulates data from two groups on two tasks. You can set the size of the 
       group and task main effects and the group-by-task interaction in the sidebar 
       (in the menu if minimised)."),
    a("Tutorial with code", href="https://debruine.github.io/plot_comparison.html"),
    fluidRow(
      box(title="Bar Plot", plotOutput("barplot", height = 250), 
          p("Bars represent means and standard errors"), width = 4),
      box(title="Box Plot", plotOutput("boxplot", height = 250), 
          p("Boxes represent medians and interquartile range"), width = 4),
      box(title="Violin Plot", plotOutput("violin", height = 250), 
          p("Shapes represent distributions, dots are means with 95% CI"), width = 4),
      box(title="Violinbox Plot", plotOutput("violinbox", height = 250), 
          p("A violin plot with a boxplot superimposed"), width = 4),
      box(title="Split Violin Plot", plotOutput("splitviolin", height = 250), 
          p("Each half of the violin is a distribution"), 
          a("Split-violin code", href="https://debruine.github.io/plot_comparison.html"), width = 4), 
      box(title="Raincloud Plot", plotOutput("raincloud", height = 250), 
          p("Shapes represent distributions and rain is individual data points"), 
          a("Code from Micah Allen", href="https://micahallen.org/2018/03/15/introducing-raincloud-plots/"), width = 4)
    )
    #   )
    # )
  )
)

server <- function(input, output, session) {
  #addClass(selector = "body", class = "sidebar-collapse")
  
  output$barplot <- renderPlot({
    input$group_effect
    input$stim_effect
    input$ixn
    input$n
    input$rerun
    
    makeData(input$n, input$group_effect, input$stim_effect, input$ixn)
    

  
    output$boxplot <- renderPlot({
      data %>%
        ggplot(aes(group, dv, fill = stim_group)) +
        geom_hline(yintercept=0, color=textcolor, size=1) +
        geom_boxplot(color = textcolor, position = position_dodge(width=0.9), alpha = 0.75) +
        ylab("DV") +
        xlab("Participant group") +
        my_theme + my_fill
    })
    
    output$violin <- renderPlot({
      data %>%
        ggplot(aes(group, dv, fill = stim_group)) +
        geom_hline(yintercept=0, color=textcolor, size=1) +
        geom_violin(color=textcolor, trim=FALSE, alpha = 0.75) +
        geom_pointrange(
          data = summary_data,
          aes(group, mean, ymin=min, ymax=max),
          shape = 20,
          color = textcolor, 
          position = position_dodge(width = 0.9)
        ) +
        ylab("DV") +
        xlab("Participant group") +
        my_theme + my_fill
    })
    
    output$violinbox <- renderPlot({
      data %>%
        ggplot(aes(group, dv, fill = stim_group)) +
        geom_hline(yintercept=0, color=textcolor, size=1) +
        geom_violin(color=textcolor, trim=FALSE, alpha = 0.75) +
        geom_boxplot(color = textcolor, width = 0.25, position = position_dodge(width=0.9)) +
        ylab("DV") +
        xlab("Participant group") +
        my_theme + my_fill
    })
    
    output$splitviolin <- renderPlot({
      data %>%
        ggplot(aes(group, dv, fill = stim_group)) +
        geom_hline(yintercept=0, color=textcolor, size=1) +
        geom_split_violin(color=textcolor, trim=FALSE, alpha = 0.75) +
        geom_pointrange(
          data = summary_data,
          aes(group, mean, ymin=min, ymax=max),
          shape = 20,
          color = textcolor, 
          position = position_dodge(width = 0.2)
        ) +
        ylab("DV") +
        xlab("Participant group") +
        my_theme + my_fill
    })
    
    output$raincloud <- renderPlot({
      data %>%
        ggplot(aes(group, dv, fill = stim_group)) +
        geom_hline(yintercept=0, color=textcolor, size=1) +
        geom_flat_violin(position = position_nudge(x = .25, y = 0), 
                         color=textcolor, trim=FALSE, alpha = 0.75) +
        geom_point(aes(color = stim_group), 
                   position = position_jitter(width = .2, height = 0), 
                   size = .5, alpha = .75) +
        ylab("DV") +
        xlab("Participant group") +
        coord_flip() +
        scale_color_manual(values = c("#3D99CC", "#898710")) +
        my_theme + my_fill
    })
    
    data %>%
      group_by(group, stim_group) %>%
      summarise(
        mean = mean(dv),
        se = sd(dv)/sqrt(n())
      ) %>%
      ggplot(aes(group, mean, fill=stim_group)) +
      geom_hline(yintercept=0, color=textcolor, size=1) +
      geom_col(color = "white", position="dodge", alpha = 0.75) +
      geom_errorbar(aes(ymin = mean-se, ymax=mean+se), 
                    width=0.1, 
                    position=position_dodge(0.9), 
                    color=textcolor) +
      ylab("DV") +
      xlab("Participant group") +
      my_theme + my_fill
  })
}

shinyApp(ui, server)