nparams <- function(k) {
  # calculate number of total parameters from all interactions of k factors
  2^k - 1
}

npred_sim <- function(sub_n = 100, var_n = 5) {
  if (var_n > 0) {
    vars <- paste0("V", rep(1:var_n, sub_n))
    vals <- rnorm(var_n*sub_n)
    # vals <- sample(0:1, var_n*sub_n, replace = T)
    eq <- paste("dv ~", paste(paste0("V", 1:var_n), collapse = "*"))
  } else {
    vars <- "1"
    vals = 1
    eq <- "dv ~ 1"
  }

  # create data frame with random variables
  dat <- tibble(
    id = rep(1:sub_n, each = max(1, var_n)),
    var = vars,
    val = vals
  ) %>%
    spread(var, val) %>%
    # add random DV
    mutate(dv = rnorm(sub_n))

  # run linear regression on random data
  mod <- lm(eq, data = dat)

  # return the proportion variance explained by this model
  summary(mod)$r.squared
}

npred_plot <- function(n, vars, reps, progress) {
  # set to each 25th integer between first and last if n length > 1
  o <- seq(n[1], n[length(n)], by = 25)
  # set to all integers between first and last if vars length > 1
  v =  vars[1]:vars[length(vars)]

  expand.grid(rep = 1:reps,
              var_n = v,
              obs_n = o
              ) %>%
    mutate(pv = map2_dbl(obs_n, var_n, function(o, v) {
      # increment progress bar
      progress$inc(1/nrow(.))
      npred_sim(o, v)
    }),
           var_n = paste0(var_n, " (", nparams(var_n)," terms)"),
           obs_n = paste(obs_n, "observations"),
           var_n = factor(var_n, levels = paste0(v, " (", nparams(v)," terms)")),
           obs_n = factor(obs_n, levels = paste(o, "observations"))
    ) %>%
    ggplot() +
    geom_freqpoly(aes(pv, color = var_n), bins = 21) +
    facet_grid(obs_n~.) +
    xlim(0, 1) +
    xlab("Proportion Variance Predicted") +
    labs(color = "Number of\nVariables")
}
