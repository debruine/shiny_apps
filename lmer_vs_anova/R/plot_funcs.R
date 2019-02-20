plot_dat <- function(dat, grand_i = 0, view = c("violin", "boxplot"), grp = "") {
  min_dv <- min(dat$dv)
  max_dv <- max(dat$dv)
  
  # aggregate over subjects or stimuli if grp is set
  if (grp == "sub") {
    dat <- dat %>%
      group_by(sub_id, sub_cond, stim_version) %>%
      summarise(dv = mean(dv))
  } else if (grp == "stim") {
    dat <- dat %>%
      group_by(stim_id, sub_cond, stim_version) %>%
      summarise(dv = mean(dv))
  }
  
  plot <- ggplot(dat, aes(sub_cond, dv, color = stim_version)) +
    geom_hline(yintercept = grand_i) +
    xlab("Subject Condition") +
    ylab("Reaction Time") +
    scale_color_discrete(name = "Stimulus Version") +
    coord_cartesian(ylim = c(min_dv, max_dv)) + 
    theme(legend.position="bottom")
  
  if ("violin" %in% view) {
    plot <- plot + geom_violin(alpha = 0.5)
  }
  
  if ("boxplot" %in% view) {
    plot <- plot + geom_boxplot(width = 0.2, position = position_dodge(width = 0.9))
  }
  
  return(plot)
}

# plot random effects by subject for model and data
plot_ranef_sub <- function(mod, dat) {
  sub <- dat %>%
    group_by(sub_id, sub_i, sub_version_slope) %>%
    summarise()

  ranef(mod)$sub_id %>%
    as_tibble(rownames = "sub_id") %>%
    rename(mod_i = `(Intercept)`,
           mod_version_slope = stim_version.code) %>%
    mutate(sub_id = as.integer(sub_id)) %>%
    left_join(sub, by = "sub_id") %>%
    select(mod_i, sub_i, mod_version_slope,  sub_version_slope) %>%
    ggplot(aes(mod_i, sub_i)) +
    geom_point() +
    geom_smooth(method = lm)
  #   GGally::ggpairs(lower = list(continuous = "smooth"),
  #                   progress = FALSE)
}

# plot random effects by stimuli for model and data
plot_ranef_stim <- function(mod, dat) {
  stim <- dat %>%
    group_by(stim_id, stim_i, stim_version_slope, stim_cond_slope, stim_cond_version_slope) %>%
    summarise()

  ranef(mod)$stim_id %>%
    as_tibble(rownames = "stim_id") %>%
    rename(mod_i = `(Intercept)`,
           mod_version_slope = stim_version.code,
           mod_cond_slope = sub_cond.code,
           mod_cond_version_slope = `stim_version.code:sub_cond.code`) %>%
    mutate(stim_id = as.integer(stim_id)) %>%
    left_join(stim, by = "stim_id") %>%
    select(mod_i, stim_i,
           mod_version_slope, stim_version_slope,
           mod_cond_slope, stim_cond_slope,
           mod_cond_version_slope, stim_cond_version_slope) %>%
    ggplot(aes(mod_i, stim_i)) +
    geom_point() +
    geom_smooth(method = lm)
  #   GGally::ggpairs(lower = list(continuous = "smooth"),
  #                   progress = FALSE)
}