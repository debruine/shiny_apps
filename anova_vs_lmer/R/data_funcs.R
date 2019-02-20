
# get reasonable figures for grand_i, stim_sd, sub_sd, and error_sd
# library(tidyverse)
# library(lmer)
# dat_orig <- read_csv("https://ndownloader.figshare.com/files/8542045")
# dat_long <- dat_orig %>%
#   mutate(rater_id = row_number()) %>%
#   gather(face_id, rating, X001:X173)
# 
# mod <- lmer(rating ~ 1 + (1 | face_id) + (1 | rater_id), data = dat_long)
# 
# grand_i <- fixef(mod) %>% unname() # 3.02
# stim_sd <-  VarCorr(mod) %>% as_tibble() %>% 
#   filter(grp == "face_id") %>% pull(sdcor)    # 0.77
# sub_sd <- VarCorr(mod) %>% as_tibble() %>% 
#   filter(grp == "rater_id") %>% pull(sdcor)   # 0.84
# error_sd <- VarCorr(mod) %>% as_tibble() %>% 
#   filter(grp == "Residual") %>% pull(sdcor)   # 1.09

sim_dat <- function(...) {
  trials <- sim_trials(...)
  dat_code(trials, ...)
}

sim_trials  <- function(sub_n = 50,
                        sub_sd = 0.84,
                        stim_n = 10,
                        stim_sd = 0.77,
                        stim_type_sd = 0.50,
                        error_sd = 1.10, ...) {
  sub <- tibble(
    sub_id = 1:sub_n,
    sub_i = rnorm(sub_n, 0, sub_sd),
    sub_stim_type_s = rnorm(sub_n, 0, stim_type_sd)
  )
  
  stim <- tibble(
    stim_id = 1:(stim_n*2),
    stim_i = rnorm((stim_n*2), 0, stim_sd),
    stim_type = rep(c("A", "B"), each = stim_n)
  )
  
  trials <- expand.grid(
    sub_id = sub$sub_id, # get subject IDs from the sub data table
    stim_id = stim$stim_id # get stimulus IDs from the stim data table
  ) %>%
    left_join(sub, by = "sub_id") %>% # includes the intercept and slope for each subject
    left_join(stim, by = "stim_id") %>%   # includes the intercept and type for each stimulus
    mutate(err = rnorm(nrow(.), 0, error_sd))
  
  return(trials)
}

dat_code <- function(trials,
                     grand_i = 3.0,
                     stim_type_eff = 0,
                     A_code = -0.5,
                     B_code = +0.5, ...) {
  dat <- trials %>%
    mutate(
      # code stimulus type
      stim_type.code = recode(stim_type, "A" = A_code, "B" = B_code),
      stim_type.e = recode(stim_type, "A" = -0.5, "B" = 0.5),
      
      # calculate DV from intercepts, effects, and error
      trial_i = grand_i + sub_i + stim_i + err,
      dv = trial_i + (stim_type.e * (stim_type_eff + sub_stim_type_s)),
      dv = dv %>% pmin(7) %>% pmax(1), # truncate to 1-7
      dv_null = trial_i + (stim_type.e * sub_stim_type_s),
      dv_null = dv_null %>% pmin(7) %>% pmax(1)
    )
  
  return(dat)
}

descr <- function(dat) {
  sub_table <- dat %>%
    group_by(sub_id, stim_type) %>%
    summarise(dv = mean(dv)) %>%
    ungroup() %>%
    group_by(stim_type) %>%
    summarise(sd = sd(dv), n = n()) %>%
    ungroup() %>%
    unite(cell, stim_type, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "subjects", stat = "sd")
  
  stim_table <- dat %>%
    group_by(stim_id, stim_type) %>%
    summarise(dv = mean(dv)) %>%
    ungroup() %>%
    group_by(stim_type) %>%
    summarise(sd = sd(dv), n = n()*2) %>% # x2 because 2 groups of stim
    ungroup() %>%
    unite(cell, stim_type, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "stimuli", stat = "sd")
  
  all_table <- dat %>%
    group_by(stim_type) %>%
    summarise(sd = sd(dv), n = n()) %>%
    ungroup() %>%
    unite(cell, stim_type, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "all", stat = "sd")
  
  all_table_dv <- dat %>%
    group_by(stim_type) %>%
    summarise(dv = mean(dv), n = n()) %>%
    ungroup() %>%
    unite(cell, stim_type, sep = " ") %>%
    spread(cell, dv) %>%
    mutate(`grouped by` = "", stat = "mean")
  
  bind_rows(
    all_table_dv,
    all_table,
    sub_table,
    stim_table
  ) %>%
    select(`grouped by`, n, stat, A:B)
}
