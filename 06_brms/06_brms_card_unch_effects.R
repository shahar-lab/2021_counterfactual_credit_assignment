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
library(data.table)


load('data/analysis_data/tab_unch.Rdata')
load('data/analysis_data/tab_unch_two.Rdata')
load('data/analysis_data/cars_unch.Rdata')

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

# bayes_model_for_reward_twoback ------------------------------------------
prior_rw_two_cond_medium = c(
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "rw_twoback"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "cond"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "rw_twoback:cond"
  )
)

# bayes_model_for_reward_cars ------------------------------------------
prior_rw_cars_medium = c(
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,0.2)",
    class = "b",
    coef = "reward_n1back"
  )
)

# models_bridgesampling ---------------------------------------------------

bayes_null_unch_medium <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 6000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_null_medium,
    save_pars = save_pars(all = TRUE)
  )

save(bayes_null_unch_medium, file = 'data/brms_data/model_null_unch_medium.Rdata')
#medium prior
bayes_rw_unch_medium <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 6000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_medium,
    save_pars = save_pars(all = TRUE)
  )
save(bayes_rw_unch_medium, file = 'data/brms_data/model_rw_unch_medium.Rdata')


#medium prior
bayes_rw_unch_cond_medium <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 6000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_cond_medium,
    save_pars = save_pars(all = TRUE)
  )

save(bayes_rw_unch_cond_medium, file = 'data/brms_data/model_rw_unch_cond_medium.Rdata')
# null_models -------------------------------------------------------------

# main_effect models ------------------------------------------------------

#strong prior
bayes_rw_unch_strong <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_strong
  )


#weak prior
bayes_rw_unch_weak <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback+ (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
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
bayes_rw_unch_uniform <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123
  )


#save models_main_effect

save(bayes_rw_unch_weak, file = 'data/brms_data/model_rw_unch_weak.Rdata')
save(bayes_rw_unch_strong, file = 'data/brms_data/model_rw_unch_strong.Rdata')
save(bayes_rw_unch_uniform, file = 'data/brms_data/model_rw_unch_uniform.Rdata')

# interaction_models ------------------------------------------------------

#strong prior
bayes_rw_unch_cond_strong <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_cond_strong
  )

#weak prior
bayes_rw_unch_cond_weak <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
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
bayes_rw_unch_cond_uniform <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123
  )


#save models_cond

save(bayes_rw_unch_cond_weak, file = 'data/brms_data/model_rw_unch_cond_weak.Rdata')
save(bayes_rw_unch_cond_strong, file = 'data/brms_data/model_rw_unch_cond_strong.Rdata')
save(bayes_rw_unch_cond_uniform, file = 'data/brms_data/model_rw_unch_cond_uniform.Rdata')


#models for prior predictive check sampling only from prior --------------

bayes_rw_unch_medium_prior_sampling <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_rw_medium,
    sample_prior = "only"
  )
save(bayes_rw_unch_medium_prior_sampling, file = 'data/brms_data/bayes_rw_unch_medium_prior_sampling.Rdata')

bayes_rw_cond_unch_medium_prior_sampling <-
  brm(
    formula = stay_frc_unch ~ 0 + Intercept +rw_oneback*cond + (1+rw_oneback*cond|prolific_id),
    data = tab_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    inits = "0",
    cores = 4,
    seed = 123,
    prior=prior_rw_cond_medium,
    sample_prior = "only"
  )
save(bayes_rw_cond_unch_medium_prior_sampling, file = 'data/brms_data/bayes_rw_cond_unch_medium_prior_sampling.Rdata')

# rw_twoback --------------------------------------------------------------

#medium prior
bayes_rw_twoback_unch_cond_medium <-
  brm(
    formula = stay_frc_unch_two ~ 0+Intercept+rw_twoback*cond + (1+rw_twoback*cond|prolific_id),
    data = tab_unch_two,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 6000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_two_cond_medium,
    save_pars = save_pars(all = TRUE)
  )

save(bayes_rw_twoback_unch_cond_medium, file = 'data/brms_data/model_rw_twoback_unch_cond_medium.Rdata')

# cars --------------------------------------------------------------


#medium prior
bayes_rw_cars_unch_medium <-
  brm(
    formula = stay_car_unch ~ 0+Intercept+reward_n1back + (1+reward_n1back|ID),
    data = cars_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 6000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_rw_cars_medium,
    save_pars = save_pars(all = TRUE)
  )

save(bayes_rw_cars_unch_medium, file = 'data/brms_data/model_rw_cars_unch_medium.Rdata')
