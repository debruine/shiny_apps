
simple_means_to_effects <- function(hard_congr = 0,
                                    hard_incon = 0,
                                    easy_congr = 0,
                                    easy_incon = 0) {
  # mean of all conditions
  grand_i <- (easy_congr + easy_incon + hard_congr + hard_incon)/4
  # mean difference between easy and hard conditions
  sub_cond_eff     <- (hard_congr + hard_incon)/2 -
    (easy_congr + easy_incon)/2
  
  # mean difference between incongruent and congruent versions
  stim_version_eff <- (hard_incon + easy_incon)/2 - 
    (hard_congr + easy_congr)/2  
  # interaction between version and condition
  cond_version_ixn <- (hard_incon - hard_congr) -
    (easy_incon - easy_congr)
  
  
  list(
    "grand_i" = grand_i,
    "sub_cond_eff" = sub_cond_eff,
    "stim_version_eff" = stim_version_eff,
    "cond_version_ixn" = cond_version_ixn
  )
}

effects_to_simple_means <- function(grand_i = 400,
                                    sub_cond_eff = 0,
                                    stim_version_eff = 0,
                                    cond_version_ixn = 0,
                                    # coding
                                    hard = 0.5,
                                    easy = -0.5,
                                    congr = -0.5,
                                    incon = 0.5) {
  
  
  hard_congr <- grand_i + 
    hard * sub_cond_eff + 
    congr * stim_version_eff + 
    hard * congr * cond_version_ixn
  easy_congr <- grand_i + 
    easy * sub_cond_eff + 
    congr * stim_version_eff + 
    easy * congr * cond_version_ixn
  hard_incon <- grand_i + 
    hard * sub_cond_eff + 
    incon * stim_version_eff + 
    hard * incon * cond_version_ixn
  easy_incon <- grand_i + 
    easy * sub_cond_eff + 
    incon * stim_version_eff + 
    easy * incon * cond_version_ixn
  
  list(
    "hard_congr" = hard_congr,
    "easy_congr" = easy_congr,
    "hard_incon" = hard_incon,
    "easy_incon" = easy_incon
  )
}

sim_trials  <- function(sub_n = 50,
                        sub_sd = 50,
                        sub_version_sd = 50, 
                        sub_i_version_cor = -0.2,
                        stim_n = 50,
                        stim_sd = 50,
                        stim_version_sd = 50,
                        stim_cond_sd = 50,
                        stim_cond_version_sd = 50,
                        stim_i_cor = -0.2,
                        stim_s_cor = +0.2,
                        error_sd = 50) {
  sub <- rnorm_multi(
    n = sub_n*2, 
    vars = 2, 
    cors = sub_i_version_cor,
    mu = 0, # means of random intercepts and slopes are always 0
    sd = c(sub_sd, sub_version_sd),
    varnames = c("sub_i", "sub_version_slope")
  ) %>%
    mutate(
      sub_id = 1:(sub_n*2),
      sub_cond = rep(c("easy","hard"), each = sub_n) # between-subjects factor
    )
  
  stim_cors <- c(stim_i_cor, stim_i_cor, stim_i_cor,
                 stim_s_cor, stim_s_cor,
                 stim_s_cor)
  stim <- rnorm_multi(
    n = stim_n, 
    vars = 4, 
    cors = stim_cors, 
    mu = 0, # means of random intercepts and slopes are always 0
    sd = c(stim_sd, stim_version_sd, stim_cond_sd, stim_cond_version_sd),
    varnames = c("stim_i", "stim_version_slope", "stim_cond_slope", "stim_cond_version_slope")
  ) %>%
    mutate(
      stim_id = 1:stim_n
    )
  
  trials <- expand.grid(
    sub_id = sub$sub_id, # get subject IDs from the sub data table
    stim_id = stim$stim_id, # get stimulus IDs from the stim data table
    stim_version = c("congruent", "incongruent") # all subjects see both congruent and incongruent versions of all stimuli
  ) %>%
    left_join(sub, by = "sub_id") %>% # includes the intercept, slope, and conditin for each subject
    left_join(stim, by = "stim_id") %>%   # includes the intercept and slopes for each stimulus
    mutate(err = rnorm(nrow(.), 0, error_sd))
  
  return(trials)
}

dat_code <- function(trials,
                     grand_i = 400,
                     sub_cond_eff = 0,
                     stim_version_eff = 0,
                     cond_version_ixn = 0,
                     hard = 0.5,
                     easy = -0.5,
                     congr = -0.5,
                     incon = 0.5) {
  dat <- trials %>%
    mutate(
      # code subject condition and stimulus version
      sub_cond.code = recode(sub_cond, "hard" = hard, "easy" = easy),
      stim_version.code = recode(stim_version, "congruent" = congr, "incongruent" = incon),
      sub_cond.e = recode(sub_cond, "hard" = 0.5, "easy" = -0.5),
      stim_version.e = recode(stim_version, "congruent" = -0.5, "incongruent" = 0.5),
      # calculate trial-specific effects by adding overall effects and slopes
      version_eff = stim_version_eff + stim_version_slope + sub_version_slope,
      cond_eff = sub_cond_eff + stim_cond_slope,
      cond_version_eff = cond_version_ixn + stim_cond_version_slope,
      # calculate DV from intercepts, effects, and error
      dv = grand_i + sub_i + stim_i + err +
        (sub_cond.e * cond_eff) + 
        (stim_version.e * version_eff) + 
        (sub_cond.e * stim_version.e * cond_version_eff)
    )
  
  return(dat)
}

descr <- function(dat) {
  sub_table <- dat %>%
    group_by(sub_id, sub_cond, stim_version) %>%
    summarise(dv = mean(dv)) %>%
    ungroup() %>%
    group_by(sub_cond, stim_version) %>%
    summarise(sd = sd(dv), n = n()) %>%
    ungroup() %>%
    unite(cell, sub_cond, stim_version, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "subjects", stat = "sd")
  
  stim_table <- dat %>%
    group_by(stim_id, sub_cond, stim_version) %>%
    summarise(dv = mean(dv)) %>%
    ungroup() %>%
    group_by(sub_cond, stim_version) %>%
    summarise(sd = sd(dv), n = n()) %>%
    ungroup() %>%
    unite(cell, sub_cond, stim_version, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "stimuli", stat = "sd")
  
  all_table <- dat %>%
    group_by(sub_cond, stim_version) %>%
    summarise(sd = sd(dv), n = n()) %>%
    ungroup() %>%
    unite(cell, sub_cond, stim_version, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "all", stat = "sd")
  
  all_table_dv <- dat %>%
    group_by(sub_cond, stim_version) %>%
    summarise(dv = mean(dv), n = n()) %>%
    ungroup() %>%
    unite(cell, sub_cond, stim_version, sep = " ") %>%
    spread(cell, dv) %>%
    mutate(`grouped by` = "", stat = "mean")
  
  bind_rows(
    all_table_dv,
    all_table,
    sub_table,
    stim_table
  ) %>%
    select(`grouped by`, n, stat, `easy congruent`:`hard incongruent`)
}
