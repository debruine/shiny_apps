plot_dat <- function(dat, grand_i = 0, view = c("violin", "boxplot"), grp = "") {
  min_dv <- min(dat$dv)
  max_dv <- max(dat$dv)
  
  # aggregate over subjects or stimuli if grp is set
  if (grp == "sub") {
    dat <- dat %>%
      group_by(sub_id, stim_type) %>%
      summarise(dv = mean(dv))
  } else if (grp == "stim") {
    dat <- dat %>%
      group_by(stim_id, stim_type) %>%
      summarise(dv = mean(dv))
  }
  
  plot <- ggplot(dat, aes(stim_type, dv, color = stim_type)) +
    geom_hline(yintercept = grand_i) +
    xlab("Stimulus Type") +
    ylab("Rating") +
    scale_color_discrete(name = "Stimulus Type") +
    coord_cartesian(ylim = c(min_dv, max_dv)) + 
    theme(legend.position="bottom")
  
  if ("violin" %in% view) {
    plot <- plot + geom_violin(alpha = 0.5, show.legend = FALSE)
  }
  
  if ("boxplot" %in% view) {
    plot <- plot + geom_boxplot(width = 0.2, show.legend = FALSE, 
                                position = position_dodge(width = 0.9))
  }
  
  return(plot)
}

plot_power_lmer <- function(dat) {
  dat %>%
    filter(type == "power", analysis == "lmer") %>%
    mutate(analysis = recode(analysis, 
                             "lmer" = "LMER")) %>%
    filter(type == "power") %>%
    ggplot(aes(es)) +
    geom_density() +
    xlab("Effect Size (raw estimate)") +
    ggtitle("LMER")
}

plot_power_anova <- function(dat) {
  dat %>%
    filter(type == "power", analysis != "lmer") %>%
    mutate(analysis = recode(analysis, 
                             "anova_sub" = "By-Subjects ANOVA",  
                             "anova_stim" = "By-Stimuli ANOVA")) %>%
    ggplot(aes(es, color = analysis)) +
    geom_density(show.legend = TRUE) +
    xlab("Effect Size (Cohen's d)") +
    theme(legend.position="bottom") +
    ggtitle("ANOVAs")
}