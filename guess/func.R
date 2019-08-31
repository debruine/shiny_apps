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
                         boxplot = FALSE) {
  p <- data %>%
    ggplot(aes(group, val, color = group, shape = group)) +
    ylim(-3, 3) +
    ylab("") +
    scale_x_discrete(drop = F) +
    scale_colour_manual(values = c("red", "steelblue3"), drop = F) +
    scale_shape_manual(values = c(15, 19), drop = F) +
    theme_minimal()
  
  if (points) {
    pt_width <- min(50, (nrow(data)-1) * 0.004) # not > 50
    pt_size <- max(1, 5.6 - log(nrow(data))) # not < 1
    p <- p + geom_point(show.legend = F, size = pt_size, 
                        position = position_jitter(seed = 20, width = pt_width, height = 0))
  }
  if (violin & nrow(data) > 1) {
    p <- p + geom_violin(draw_quantiles = 0.5,
                         alpha = 0.3, show.legend = F)
  }
  if (boxplot & nrow(data) > 1) {
    p <- p + geom_boxplot(width = 0.25, alpha = 0.3, show.legend = F)
  }
  
  p
}