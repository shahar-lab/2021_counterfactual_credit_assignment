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

load('data/tab.Rdata')
tab_unch= tab%>%filter(!reoffer_ch,reoffer_unch)

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

# bayes_model_dual_task ---------------------------------------------------
prior_medium_dual_unch=c(set_prior(
  prior = "normal(0,0.2)",
  class = "b",
  coef = "Intercept"
),
set_prior(
  prior = "normal(0,0.2)", 
  class = "b",
  coef = "rw_when_unch_last_appeared"
))

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

save(bayes_null_unch_medium, file = '06_brms/model_null_unch_medium.Rdata')
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
save(bayes_rw_unch_medium, file = '06_brms/model_rw_unch_medium.Rdata')


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

save(bayes_rw_unch_cond_medium, file = '06_brms/model_rw_unch_cond_medium.Rdata')
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

save(bayes_rw_unch_weak, file = '06_brms/model_rw_unch_weak.Rdata')
save(bayes_rw_unch_strong, file = '06_brms/model_rw_unch_strong.Rdata')
save(bayes_rw_unch_uniform, file = '06_brms/model_rw_unch_uniform.Rdata')

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

save(bayes_rw_unch_cond_weak, file = '06_brms/model_rw_unch_cond_weak.Rdata')
save(bayes_rw_unch_cond_strong, file = '06_brms/model_rw_unch_cond_strong.Rdata')
save(bayes_rw_unch_cond_uniform, file = '06_brms/model_rw_unch_cond_uniform.Rdata')


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
save(bayes_rw_unch_medium_prior_sampling, file = '06_brms/bayes_rw_unch_medium_prior_sampling.Rdata')

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
save(bayes_rw_cond_unch_medium_prior_sampling, file = '06_brms/bayes_rw_cond_unch_medium_prior_sampling.Rdata')

# rw_twoback --------------------------------------------------------------

tab$stay_frc_unch_two     <-
  tab$ch == shift(tab$unch,
                  n = 2,
                  type = 'lag',
                  fill = 0) * 1
tab$rw_twoback            <- shift(tab$rw,
                                   n = 2,
                                   type = 'lag',
                                   fill = 0)
tab$reoffer_ch_two        <-
  (
    tab$frcA == shift(
      tab$ch,
      n = 2,
      type = 'lag',
      fill = 0
    ) | tab$frcB == shift(
      tab$ch,
      n = 2,
      type = 'lag',
      fill = 0
    )
  )
tab$reoffer_unch_two      <-
  (
    tab$frcA == shift(
      tab$unch,
      n = 2,
      type = 'lag',
      fill = 0
    ) | tab$frcB == shift(
      tab$unch,
      n = 2,
      type = 'lag',
      fill = 0
    )
  )

tab_unch_two=tab%>%filter(trl-2==lag(trl,2),!reoffer_ch_two,reoffer_unch_two)

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

save(bayes_rw_twoback_unch_cond_medium, file = '06_brms/model_rw_twoback_unch_cond_medium.Rdata')

# cars --------------------------------------------------------------
load('data/replications/yaelT.Rdata')
task_unch=task%>%filter(reoffered_unchosen==1,reoffered_chosen==0)

#medium prior
bayes_rw_cars_unch_medium <-
  brm(
    formula = stay_car_unch ~ 0+Intercept+reward_n1back + (1+reward_n1back|ID),
    data = task_unch,
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

save(bayes_rw_cars_unch_medium, file = '06_brms/model_rw_cars_unch_medium.Rdata')

# dual-task ---------------------------------------------------------------

load('data/replications/idoBA.Rdata')
cards=as.data.frame.data.frame(cards)
cards=cards%>%mutate(stay_unch_card=ch_card==lag(unch_card,default=0),
                                             reoffer_unch_card_oneback=card_right==lag(unch_card,default=0)|card_left==lag(unch_card,default=0),
                                             reoffer_ch_card_oneback=card_right==lag(ch_card,default=0)|card_left==lag(ch_card,default=0))
cards_unch=cards%>%filter(subtrial==1,reoffer_unch_card_oneback,!reoffer_ch_card_oneback)
cards_unch%>%group_by(reward_oneback)%>%summarise(mean(stay_unch_card))

bayes_unch_dual_rw <-
  brm(
    formula=stay_unch_card~0+Intercept+rew+(1+rw_when_unch_last_appeared|subj),
    data = cards_unch,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 6000,
    chains = num_cores,
    inits = "0",
    cores = num_cores,
    seed = 123,
    prior = prior_medium_dual_unch
  )
save(bayes_unch_dual_rw, file = '06_brms/model_rw_dual_unch_medium.Rdata')

