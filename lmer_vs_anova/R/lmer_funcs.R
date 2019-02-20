sim_lmer <- function(dat) {
  mod <- lmer(dv ~ sub_cond.code * stim_version.code +
                (1 + stim_version.code | sub_id) + 
                (1 + stim_version.code*sub_cond.code | stim_id),
              data = dat)
  
  return(mod)
}

sim_sub_anova <- function(dat) {
  dat_sub <- dat %>%
    group_by(sub_id, sub_cond, sub_cond.code, stim_version, stim_version.code) %>%
    summarise(dv = mean(dv))
  
  mod <- afex::aov_4(dv ~ sub_cond.code + (stim_version.code| sub_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_sub)
  
  mod.sum <- anova(mod)
  
  return(mod.sum)
}

sim_stim_anova <- function(dat) {
  dat_stim <- dat %>%
    group_by(stim_id, sub_cond, sub_cond.code, stim_version, stim_version.code) %>%
    summarise(dv = mean(dv))
  
  mod <- afex::aov_4(dv ~ (sub_cond.code * stim_version.code | stim_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_stim)
  
  mod.sum <- anova(mod)
  
  return(mod.sum)
}

sim_power <- function(rep = 0,
                      sub_n = 100,
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
                      grand_i = 400,
                      hard_congr = 0,
                      hard_incon = 0,
                      easy_congr = 0,
                      easy_incon = 0,
                      sub_cond_eff = NULL,
                      stim_version_eff = NULL,
                      cond_version_ixn = NULL,
                      error_sd = 50) {
  
  dat <- sim_dat(
    sub_n = sub_n,
    sub_sd = sub_sd,
    sub_version_sd = sub_version_sd, 
    sub_i_version_cor = sub_i_version_cor,
    stim_n = stim_n,
    stim_sd = stim_sd,
    stim_version_sd = stim_version_sd,
    stim_cond_sd = stim_cond_sd,
    stim_cond_version_sd = stim_cond_version_sd,
    stim_i_cor = stim_i_cor,
    stim_s_cor = stim_s_cor,
    grand_i = grand_i,
    hard_congr = hard_congr,
    hard_incon = hard_incon,
    easy_congr = easy_congr,
    easy_incon = easy_incon,
    sub_cond_eff = sub_cond_eff,
    stim_version_eff = stim_version_eff,
    cond_version_ixn = cond_version_ixn,
    error_sd = error_sd
  )
  
  mod.lmer <- sim_lmer(dat)
  mod.sub <- sim_sub_anova(dat)
  mod.stim <- sim_stim_anova(dat)
  
  
  table.lmer <- mod.lmer$coefficients %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = Estimate, p = 6) %>%
    mutate(analysis = "lmer")
  
  table.sub <- mod.sub %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = ges, p = 7) %>%
    mutate(analysis = "anova_sub")
  
  table.stim <- mod.stim %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = ges, p = 7) %>%
    mutate(analysis = "anova_stim")
  
  bind_rows(table.lmer, table.sub, table.stim) %>%
    mutate(rep = rep)
}

