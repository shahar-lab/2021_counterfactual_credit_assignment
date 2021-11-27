rm(list = ls())
# effect of reward on unchosen --------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(brms)
library(tidybayes) 
library(bayestestR)
library(bayesplot)
library(Rcpp)
library(see)
# load data ---------------------------------------------------------------

load('data/tab.Rdata')
tab_unch= tab%>%filter(!reoffer_ch,reoffer_unch)

#load models ------------------------------------------------------------

#reward
load('06_brms/model_rw_unch_strong.Rdata')
load('06_brms/model_rw_unch_medium.Rdata')
load('06_brms/model_rw_unch_weak.Rdata')

#reward*cond
load('06_brms/model_rw_unch_cond_strong.Rdata')
load('06_brms/model_rw_unch_cond_medium.Rdata')
load('06_brms/model_rw_unch_cond_weak.Rdata')

#prior_only
load('06_brms/bayes_rw_unch_medium_prior_sampling.Rdata')
load('06_brms/bayes_rw_cond_unch_medium_prior_sampling.Rdata')

#twoback
load('06_brms/model_rw_twoback_unch_cond_medium.Rdata')
#cars
load('06_brms/model_rw_cars_unch_medium.Rdata')
#dual-task
load('06_brms/model_rw_dual_unch_medium.Rdata')
#Bayesian visualisation -------------------------------------------------
source('07_bayes_comparisons/plot_posterior.R')
plot_posterior(bayes_rw_unch_medium,"b_rw_oneback","Reward previous offer")
plot_posterior(bayes_rw_unch_cond_medium,"b_rw_oneback:cond","Reward X condition")


# Bayes Factors -----------------------------------------------------------

# Vs the null -------------------------------------------------------------

#reward_only

strong_bf_rw_unch=bayesfactor_parameters(bayes_rw_unch_strong)
strong_bf_rw_unch
plot(strong_bf_rw_unch)
save(strong_bf_rw_unch, file = '07_bayes_comparisons/strong_bf_rw_unch.Rdata')

medium_bf_rw_unch=bayesfactor_parameters(bayes_rw_unch_medium)
medium_bf_rw_unch
plot(medium_bf_rw_unch)
save(medium_bf_rw_unch, file = '07_bayes_comparisons/medium_bf_rw_unch.Rdata')

weak_bf_rw_unch=bayesfactor_parameters(bayes_rw_unch_weak)
weak_bf_rw_unch
plot(weak_bf_rw_unch)
save(weak_bf_rw_unch, file = '07_bayes_comparisons/weak_bf_rw_unch.Rdata')

#reward and condition

strong_bf_rw_cond_unch=bayesfactor_parameters(bayes_rw_unch_cond_strong)
strong_bf_rw_cond_unch
plot(strong_bf_rw_cond_unch)
save(strong_bf_rw_cond_unch, file = '07_bayes_comparisons/strong_bf_rw_cond_unch.Rdata')

medium_bf_rw_cond_unch=bayesfactor_parameters(bayes_rw_unch_cond_medium)
medium_bf_rw_cond_unch
plot(medium_bf_rw_cond_unch)
save(medium_bf_rw_cond_unch, file = '07_bayes_comparisons/medium_bf_rw_cond_unch.Rdata')

weak_bf_rw_cond_unch=bayesfactor_parameters(bayes_rw_unch_cond_weak)
weak_bf_rw_cond_unch
plot(weak_bf_rw_cond_unch)
save(weak_bf_rw_cond_unch, file = '07_bayes_comparisons/weak_bf_rw_cond_unch.Rdata')

#twoback
medium_bf_rw_unch_twoback=bayesfactor_parameters(bayes_rw_twoback_unch_cond_medium)
medium_bf_rw_unch_twoback
plot(medium_bf_rw_unch_twoback)
save(medium_bf_rw_unch_twoback, file = '07_bayes_comparisons/medium_bf_rw_unch_twoback.Rdata')
#cars
medium_bf_rw_cars_unch=bayesfactor_parameters(bayes_rw_cars_unch_medium)
medium_bf_rw_cars_unch
plot(medium_bf_rw_cars_unch)
save(medium_bf_rw_cars_unch, file = '07_bayes_comparisons/medium_bf_rw_cars_unch.Rdata')
#dualtask
medium_bf_rw_dual_unch=bayesfactor_parameters(bayes_unch_dual_rw)
medium_bf_rw_dual_unch
plot(medium_bf_rw_dual_unch)
save(medium_bf_rw_dual_unch, file = '07_bayes_comparisons/medium_bf_rw_dual_unch.Rdata')
# Vs the ROPE -------------------------------------------------------------

#reward_only

strong_bf_rw_rope_unch=bayesfactor_parameters(bayes_rw_unch_strong,null=c(-0.0132, 0.0132))
strong_bf_rw_rope_unch
plot(strong_bf_rw_rope_unch)
save(strong_bf_rw_rope_unch, file = '07_bayes_comparisons/strong_bf_rw_rope_unch.Rdata')

medium_bf_rw_rope_unch=bayesfactor_parameters(bayes_rw_unch_medium,null=c(-0.0132, 0.0132))
medium_bf_rw_rope_unch
plot(medium_bf_rw_rope_unch)
save(medium_bf_rw_rope_unch, file = '07_bayes_comparisons/medium_bf_rw_rope_unch.Rdata')

weak_bf_rw_rope_unch=bayesfactor_parameters(bayes_rw_unch_weak,null=c(-0.0132, 0.0132))
weak_bf_rw_rope_unch
plot(weak_bf_rw_rope_unch)
save(weak_bf_rw_rope_unch, file = '07_bayes_comparisons/weak_bf_rw_rope_unch.Rdata')

#reward and condition

strong_bf_rw_cond_rope_unch=bayesfactor_parameters(bayes_rw_unch_cond_strong,null=c(-0.0132, 0.0132))
strong_bf_rw_cond_rope_unch
plot(strong_bf_rw_cond_rope_unch)
save(strong_bf_rw_cond_rope_unch, file = '07_bayes_comparisons/strong_bf_rw_cond_rope_unch.Rdata')

medium_bf_rw_cond_rope_unch=bayesfactor_parameters(bayes_rw_unch_cond_medium,null=c(-0.0132, 0.0132))
medium_bf_rw_cond_rope_unch
plot(medium_bf_rw_cond_rope_unch)
save(medium_bf_rw_cond_rope_unch, file = '07_bayes_comparisons/medium_bf_rw_cond_rope_unch.Rdata')

weak_bf_rw_cond_rope_unch=bayesfactor_parameters(bayes_rw_unch_cond_weak,null=c(-0.0132, 0.0132))
weak_bf_rw_cond_rope_unch
plot(weak_bf_rw_cond_rope_unch)
save(weak_bf_rw_cond_rope_unch, file = '07_bayes_comparisons/weak_bf_rw_cond_rope_unch.Rdata')

#twoback
medium_bf_rw_unch_twoback_rope=bayesfactor_parameters(bayes_rw_twoback_unch_cond_medium,null=c(-0.0132, 0.0132))
medium_bf_rw_unch_twoback_rope
plot(medium_bf_rw_unch_twoback_rope)
save(medium_bf_rw_unch_twoback_rope, file = '07_bayes_comparisons/medium_bf_rw_unch_twoback_rope.Rdata')

#cars
medium_bf_rw_cars_unch_rope=bayesfactor_parameters(bayes_rw_cars_unch_medium,null=c(-0.0132, 0.0132))
medium_bf_rw_cars_unch_rope
plot(medium_bf_rw_cars_unch_rope)
save(medium_bf_rw_cars_unch_rope, file = '07_bayes_comparisons/medium_bf_rw_cars_unch_rope.Rdata')

#dualtask
medium_bf_rw_dual_unch_rope=bayesfactor_parameters(bayes_unch_dual_rw,null=c(-0.0132, 0.0132))
medium_bf_rw_dual_unch_rope
plot(medium_bf_rw_dual_unch_rope)
save(medium_bf_rw_dual_unch_rope, file = '07_bayes_comparisons/medium_bf_rw_dual_unch_rope.Rdata')

# describe results --------------------------------------------------------

#reward
describe_posterior_rw_unch_strong=describe_posterior(bayes_rw_unch_strong, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_unch_medium=describe_posterior(bayes_rw_unch_medium, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_unch_weak=describe_posterior(bayes_rw_unch_weak, rope_range = c(-0.0132, 0.0132))

#reward and condition
describe_posterior_rw_unch_cond_strong=describe_posterior(bayes_rw_unch_cond_strong, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_unch_cond_medium=describe_posterior(bayes_rw_unch_cond_medium, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_unch_cond_weak=describe_posterior(bayes_rw_unch_cond_weak, rope_range = c(-0.0132, 0.0132))

#extra results
describe_posterior_rw_unch_twoback=describe_posterior(bayes_rw_twoback_unch_cond_medium, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_unch_cars=describe_posterior(bayes_rw_cars_unch_medium, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_unch_dual=describe_posterior(bayes_unch_dual_rw, rope_range = c(-0.0132, 0.0132))

save(describe_posterior_rw_unch_strong, file = '07_bayes_comparisons/describe_posterior_rw_unch_strong.Rdata')
save(describe_posterior_rw_unch_medium, file = '07_bayes_comparisons/describe_posterior_rw_unch_medium.Rdata')
save(describe_posterior_rw_unch_weak, file = '07_bayes_comparisons/describe_posterior_rw_unch_weak.Rdata')

save(describe_posterior_rw_unch_cond_strong, file = '07_bayes_comparisons/describe_posterior_rw_unch_cond_strong.Rdata')
save(describe_posterior_rw_unch_cond_medium, file = '07_bayes_comparisons/describe_posterior_rw_unch_cond_medium.Rdata')
save(describe_posterior_rw_unch_cond_weak, file = '07_bayes_comparisons/describe_posterior_rw_unch_cond_weak.Rdata')

save(describe_posterior_rw_unch_twoback, file = '07_bayes_comparisons/describe_posterior_rw_unch_twoback.Rdata')
save(describe_posterior_rw_unch_cars, file = '07_bayes_comparisons/describe_posterior_rw_unch_cars.Rdata')
save(describe_posterior_rw_unch_dual, file = '07_bayes_comparisons/describe_posterior_rw_unch_dual.Rdata')

#prior_predictive_check reward model medium prior
y=as.numeric(tab_unch$stay_frc_unch)
yrep=posterior_predict(bayes_rw_unch_medium_prior_sampling)
group_vec=tab_unch$rw_oneback==1
png(file="/home/shared/0004_Ido_Set-size_effects/Counterfactual-credit-assignemnt-for-deliberated-and-unchosen-options-/graphics/prior_predictive_unch.png")
ppc_stat_grouped(y, yrep, group_vec)

dev.off()
#posterior_predictive_check
y=as.numeric(tab_unch$stay_frc_unch)
yrep=posterior_predict(bayes_rw_unch_medium)
group_vec=tab_unch$rw_oneback==1
png(file="/home/shared/0004_Ido_Set-size_effects/Counterfactual-credit-assignemnt-for-deliberated-and-unchosen-options-/graphics/posterior_predictive_unch.png")
ppc_stat_grouped(y, yrep, group_vec)
dev.off()