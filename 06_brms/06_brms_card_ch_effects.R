# Bayes modeling ----------------------------------------------------------
rm(list = ls())
library(tidylog)
library(effects)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(tidybayes) #for analysis of posterior draws of a Bayesian model
library(magrittr)
library(ggeffects)
library(bayestestR)
library(bayesplot)
library(parallel)

load('data/tab.Rdata')
tab_ch= tab%>%filter(!reoffer_unch,reoffer_ch)

num_cores=detectCores()


# null_model --------------------------------------------------------------
prior_null_medium = c(
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "Intercept"
  )
)

# Bayes model for reward ------------------------------------

#priors
prior_rw_strong = c(
  set_prior(
    prior = "normal(0,0.09)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.09)",
    class = "b",
    coef = "rw_oneback"
  )
)
###
prior_rw_medium = c(
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "rw_oneback"
  )
)
prior_rw_weak = c(
  set_prior(
    prior = "normal(0,0.4)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.4)",
    class = "b",
    coef = "rw_oneback"
  )
)
###
prior_rw_cond_strong = c(
  set_prior(
    prior = "normal(0,0.09)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.09)",
    class = "b",
    coef = "rw_oneback"
  ),
  set_prior(
    prior = "normal(0,0.09)",
    class = "b",
    coef = "cond"
  ),
  set_prior(
    prior = "normal(0,0.09)",
    class = "b",
    coef = "rw_oneback:cond"
  )
)
###
prior_rw_cond_medium = c(
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "rw_oneback"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "cond"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "rw_oneback:cond"
  )
)
prior_rw_cond_weak = c(
  set_prior(
    prior = "normal(0,0.4)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.4)",
    class = "b",
    coef = "rw_oneback"
  ),
  set_prior(
    prior = "normal(0,0.4)",
    class = "b",
    coef = "cond"
  ),
  set_prior(
    prior = "normal(0,0.4)",
    class = "b",
    coef = "rw_oneback:cond"
  )
)

# null_models -------------------------------------------------------------

bayes_null_ch_medium <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_null_medium
  )

save(bayes_null_ch_medium, file = '06_brms/model_null_ch_medium.Rdata')
# main_effect models ------------------------------------------------------

#strong prior
bayes_rw_ch_strong <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_strong
  )

#medium prior
bayes_rw_ch_medium <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_medium
  )

#weak prior
bayes_rw_ch_weak <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback+ (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_weak
  )

#uniform prior
bayes_rw_ch_uniform <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123
  )


#save models_main_effect

save(bayes_rw_ch_weak, file = '06_brms/model_rw_ch_weak.Rdata')
save(bayes_rw_ch_medium, file = '06_brms/model_rw_ch_medium.Rdata')
save(bayes_rw_ch_strong, file = '06_brms/model_rw_ch_strong.Rdata')
save(bayes_rw_ch_uniform, file = '06_brms/model_rw_ch_uniform.Rdata')

# interaction_models ------------------------------------------------------

#strong prior
bayes_rw_ch_cond_strong <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_cond_strong
  )

#medium prior
bayes_rw_ch_cond_medium <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_cond_medium
  )

#weak prior
bayes_rw_ch_cond_weak <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_cond_weak
  )

#uniform prior
bayes_rw_ch_cond_uniform <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123
  )


#save models_cond

save(bayes_rw_ch_cond_weak, file = '06_brms/model_rw_ch_cond_weak.Rdata')
save(bayes_rw_ch_cond_medium, file = '06_brms/model_rw_ch_cond_medium.Rdata')
save(bayes_rw_ch_cond_strong, file = '06_brms/model_rw_ch_cond_strong.Rdata')
save(bayes_rw_ch_cond_uniform, file = '06_brms/model_rw_ch_cond_uniform.Rdata')


#models for prior predictive check sampling only from prior --------------

bayes_rw_ch_medium_prior_sampling <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior=prior_rw_medium,
    sample_prior = "only"
  )
save(bayes_rw_ch_medium_prior_sampling, file = '06_brms/bayes_rw_ch_medium_prior_sampling.Rdata')

bayes_rw_cond_ch_medium_prior_sampling <-
  brm(
    formula = stay_frc_ch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_ch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior=prior_rw_cond_medium,
    sample_prior = "only"
  )
save(bayes_rw_cond_ch_medium_prior_sampling, file = '06_brms/bayes_rw_cond_ch_medium_prior_sampling.Rdata')
