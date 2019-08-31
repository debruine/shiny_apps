saveData <- function(data, stats, id, outputDir = "responses") {
  # Create a unique file name
  dataName <- sprintf("%s_data.csv", id)
  statsName <- sprintf("%s_stats.csv", id)
  
  # Write the files to the local system
  readr::write_csv(
    x = data,
    path = file.path(outputDir, dataName)
  )
  
  readr::write_csv(
    x = stats,
    path = file.path(outputDir, statsName)
  )
}


loadData <- function(outputDir = "responses", 
                     pattern = "*_data.csv") {
  # read all the files into a list
  files <- list.files(outputDir, 
                      pattern = pattern, 
                      full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    data <- data.frame()
  } else {
    data <- lapply(files, function(f) {
      readr::read_csv(f, col_types = readr::cols(guess_dir = "c")) %>%
        mutate(session_id = gsub("responses/", "", f))
    })
    
    # Concatenate all data together into one data.frame
    data <- do.call(bind_rows, data)
  }
  
  data
}

summary_tri_plot <- function(data) {
  # TODO: too rigid, needs flexibility for other levels combos
  mutate(data, bin = factor(real, levels = c(-.8, -.5, -.2, 0, .2, .5, .8))) %>%
    group_by(bin) %>%
    summarise(correct = mean(correct)*100) %>%
    ggplot(aes(bin, correct, fill = bin)) +
    geom_col(show.legend = FALSE) +
    xlab("The true effect size (d)") +
    ylab("Percent correct") +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values = c("#DD4B39", "#DD4B39", "#DD4B39",
                                 "#605CA8", 
                                 "#0073B7", "#0073B7", "#0073B7"),
                      drop = FALSE)  +
    theme_minimal()
}

summary_guess_plot <- function(data) {
  ggplot(data, aes(real, guess_es)) +
    geom_abline(slope = 1, intercept = 0, color = "grey30") +
    geom_point() +
    geom_smooth(method = "lm") +
    xlab("The true effect size (d)") +
    ylab("Your guessed effect size (d)") +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
    theme_minimal()
}

current_plot <- function(data, 
                         points  = FALSE, 
                         violin  = FALSE, 
                         boxplot = FALSE,
                         barplot = FALSE, 
                         stats = FALSE,
                         m1 = 0, m2 = 0, sd = 1) {
  p <- data %>%
    ggplot(aes(group, val, color = group, shape = group)) +
    coord_cartesian(ylim = c(-4, 4.5)) +
    ylab("") +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(-4, -2, 0, 2, 4)) +
    scale_colour_manual(values = c("red", "steelblue3"), drop = F) +
    scale_shape_manual(values = c(15, 19), drop = F) +
    theme_minimal()
  
  if (barplot) {
    p <- p + 
      stat_summary(fun.y=mean,
                   position=position_dodge(width=0.95),
                   geom="bar", fill = "white") +
      stat_summary(fun.data=mean_cl_normal,
                   position=position_dodge(0.95),
                   geom="errorbar", width = 0.25)
  }
  
  if (points) {
    pt_width <- .45 #min(.45, (nrow(data)-1) * 0.004) # not > 40
    pt_size <- max(1, 5.6 - log(nrow(data))) # not < 1
    p <- p + geom_point(size = pt_size, 
                        position = position_jitter(seed = 20, width = pt_width, height = 0))
  }
  
  if (violin & nrow(data) > 1) {
    p <- p + geom_violin(draw_quantiles = 0.5,
                         alpha = 0.3)
  }
  
  if (boxplot & nrow(data) > 1) {
    p <- p + geom_boxplot(width = 0.25, alpha = 0.3)
  }
  
  if (stats) {
    means <- data %>%
      add_row(group = "B", val = NA) %>%
      group_by(group) %>%
      summarise(m = mean(val, na.rm = TRUE),
                sd = sd(val, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(sd_pooled = sqrt(mean(sd))) %>%
      select(-sd) %>%
      spread(group, m)
    d <- (means$B - means$A)/means$sd_pooled

    p <- p + stat_summary(fun.data = function(x) {
      m <- mean(x) %>% round(2)
      s <- sd(x) %>% round(2)
      l <- paste0("sample M = ", m, ", SD = ", s)
      data.frame(y = 3.8, label = l)
    }, geom = "text", size = 5) +
      annotate("text", y = 4.6, x = 1.5, 
               label = paste0("sample d = ", round(d, 2)),
               size = 5) +
      annotate("text", y = 4.2, x = 1, 
               label = paste0("pop M = ", m1, ", SD = ", sd),
               color = "red", size = 5) +
      annotate("text", y = 4.2, x = 2, 
               label = paste0("pop M = ", m2, ", SD = ", sd),
               color = "steelblue3", size = 5)
  }
  
  p + theme(legend.position = "none")
}