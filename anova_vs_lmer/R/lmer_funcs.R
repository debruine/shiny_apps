sim_lmer <- function(dat) {
  mod <- lmer(dv ~ stim_type.code +
                (1 + stim_type.code | sub_id) + 
                (1 | stim_id),
              data = dat)
  
  return(mod)
}

sim_sub_anova <- function(dat) {
  dat_sub <- dat %>%
    group_by(sub_id, stim_type, stim_type.code) %>%
    summarise(dv = mean(dv))
  
  mod <- afex::aov_4(dv ~ (stim_type.code | sub_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_sub)
  
  mod.sum <- anova(mod)

  # within cohen's d  
  x <- filter(dat_sub, stim_type == "A") %>% pull(dv)
  y <- filter(dat_sub, stim_type == "B") %>% pull(dv)
  mod.sum$d <- cohen_d(x, y, TRUE)

  return(mod.sum)
}

sim_stim_anova <- function(dat) {
  dat_stim <- dat %>%
    group_by(stim_id, stim_type, stim_type.code) %>%
    summarise(dv = mean(dv))
  
  mod <- afex::aov_4(dv ~ stim_type.code + (1 | stim_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_stim)
  
  mod.sum <- anova(mod)
  
  # between cohen's d
  x <- filter(dat_stim, stim_type == "A") %>% pull(dv)
  y <- filter(dat_stim, stim_type == "B") %>% pull(dv)
  mod.sum$d <- cohen_d(x, y, FALSE)
  
  return(mod.sum)
}

sim_power <- function(rep = 0, ...) {
  dat <- sim_dat(...)
  dots <- list(...)
  
  # run models to calculate power
  mod.lmer <- sim_lmer(dat)
  mod.sub <- sim_sub_anova(dat)
  mod.stim <- sim_stim_anova(dat)
  
  if (dots$stim_type_eff != 0) {
    # run models for null effect to calculate false positives
    dat$dv <- dat$dv_null
    
    mod.lmer.null <- sim_lmer(dat)
    mod.sub.null <- sim_sub_anova(dat)
    mod.stim.null <- sim_stim_anova(dat)
  }
  
  # get output into tables
  table.lmer <- summary(mod.lmer)$coefficients %>%
    as_tibble(rownames = "effect") %>%
    filter(effect != "(Intercept)") %>%
    select(effect, es = Estimate, p = 6) %>%
    mutate(analysis = "lmer", type = "power")
  
  table.sub <- mod.sub %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = d, p = 7) %>%
    mutate(analysis = "anova_sub", type = "power")
  
  table.stim <- mod.stim %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = d, p = 7) %>%
    mutate(analysis = "anova_stim", type = "power")
  
  if (dots$stim_type_eff == 0) {
    # avoid duplicate models if effect is null
    table.lmer.null <- mutate(table.lmer, type = "false positive")
    table.sub.null <- mutate(table.sub, type = "false positive")
    table.stim.null <- mutate(table.stim, type = "false positive")
  } else {
    table.lmer.null <- summary(mod.lmer.null)$coefficients %>%
      as_tibble(rownames = "effect") %>%
      filter(effect != "(Intercept)") %>%
      select(effect, es = Estimate, p = 6) %>%
      mutate(analysis = "lmer", type = "false positive")
    
    table.sub.null <- mod.sub.null %>%
      as_tibble(rownames = "effect") %>%
      select(effect, es = ges, p = 7) %>%
      mutate(analysis = "anova_sub", type = "false positive")
    
    table.stim.null <- mod.stim.null %>%
      as_tibble(rownames = "effect") %>%
      select(effect, es = ges, p = 7) %>%
      mutate(analysis = "anova_stim", type = "false positive")
  }
  
  bind_rows(table.lmer, table.sub, table.stim, 
            table.lmer.null, table.sub.null, table.stim.null) %>%
    mutate(rep = rep)
}
