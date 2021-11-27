rm(list = ls())
# effect of reward on chosen --------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(brms)
library(tidybayes) 
library(bayestestR)
library(bayesplot)
library(Rcpp)

# load data ---------------------------------------------------------------

load('data/tab.Rdata')

tab_ch=tab%>%filter(reoffer_ch,!reoffer_unch)

#load models ------------------------------------------------------------

#reward
load('06_brms/model_rw_ch_strong.Rdata')
load('06_brms/model_rw_ch_medium.Rdata')
load('06_brms/model_rw_ch_weak.Rdata')

#reward*cond
load('06_brms/model_rw_ch_cond_strong.Rdata')
load('06_brms/model_rw_ch_cond_medium.Rdata')
load('06_brms/model_rw_ch_cond_weak.Rdata')

#prior_only
load('06_brms/bayes_rw_ch_medium_prior_sampling.Rdata')
load('06_brms/bayes_rw_cond_ch_medium_prior_sampling.Rdata')

#Bayesian visualisation -------------------------------------------------
source('07_bayes_comparisons/plot_posterior.R')
plot_posterior(bayes_rw_medium,"b_rw_oneback","Reward previous offer")
plot_posterior(bayes_rw_cond_medium,"b_rw_oneback:cond","Reward X condition")


# Bayes Factors -----------------------------------------------------------

# Vs the null -------------------------------------------------------------

#reward_only

strong_bf_rw=bayesfactor_parameters(bayes_rw_strong)
strong_bf_rw
plot(strong_bf_rw)
save(strong_bf_rw, file = '07_bayes_comparisons/strong_bf_rw')

medium_bf_rw=bayesfactor_parameters(bayes_rw_medium)
medium_bf_rw
plot(medium_bf_rw)
save(medium_bf_rw, file = '07_bayes_comparisons/medium_bf_rw')

weak_bf_rw=bayesfactor_parameters(bayes_rw_weak)
weak_bf_rw
plot(weak_bf_rw)
save(weak_bf_rw, file = '07_bayes_comparisons/weak_bf_rw')

#reward and condition

strong_bf_rw_cond=bayesfactor_parameters(bayes_rw_cond_strong)
strong_bf_rw_cond
plot(strong_bf_rw_cond)
save(strong_bf_rw_cond, file = '07_bayes_comparisons/strong_bf_rw_cond')

medium_bf_rw_cond=bayesfactor_parameters(bayes_rw_cond_medium)
medium_bf_rw_cond
plot(medium_bf_rw_cond)
save(medium_bf_rw_cond, file = '07_bayes_comparisons/medium_bf_rw_cond')

weak_bf_rw_cond=bayesfactor_parameters(bayes_rw_cond_weak)
weak_bf_rw_cond
plot(weak_bf_rw_cond)
save(weak_bf_rw_cond, file = '07_bayes_comparisons/weak_bf_rw_cond')

# Vs the ROPE -------------------------------------------------------------

#reward_only

strong_bf_rw_rope=bayesfactor_parameters(bayes_rw_strong,null=c(-0.0132, 0.0132))
strong_bf_rw_rope
plot(strong_bf_rw_rope)
save(strong_bf_rw_rope, file = '07_bayes_comparisons/strong_bf_rw_rope')

medium_bf_rw_rope=bayesfactor_parameters(bayes_rw_medium,null=c(-0.0132, 0.0132))
medium_bf_rw_rope
plot(medium_bf_rw_rope)
save(medium_bf_rw_rope, file = '07_bayes_comparisons/medium_bf_rw_rope')

weak_bf_rw_rope=bayesfactor_parameters(bayes_rw_medium,null=c(-0.0132, 0.0132))
medium_bf_rw_rope
plot(medium_bf_rw_rope)
save(weak_bf_rw_rope, file = '07_bayes_comparisons/weak_bf_rw_rope')

#reward and condition

strong_bf_rw_cond_rope=bayesfactor_parameters(bayes_rw_cond_strong,null=c(-0.0132, 0.0132))
strong_bf_rw_cond_rope
plot(strong_bf_rw_cond_rope)
save(strong_bf_rw_cond_rope, file = '07_bayes_comparisons/strong_bf_rw_cond_rope')

medium_bf_rw_cond_rope=bayesfactor_parameters(bayes_rw_cond_medium,null=c(-0.0132, 0.0132))
medium_bf_rw_cond_rope
plot(medium_bf_rw_cond_rope)
save(medium_bf_rw_cond_rope, file = '07_bayes_comparisons/medium_bf_rw_cond_rope')

weak_bf_rw_cond_rope=bayesfactor_parameters(bayes_rw_cond_weak,null=c(-0.0132, 0.0132))
weak_bf_rw_cond_rope
plot(weak_bf_rw_cond_rope)
save(weak_bf_rw_cond_rope, file = '07_bayes_comparisons/weak_bf_rw_cond_rope')

#describe results
describe_posterior_rw_strong=describe_posterior(bayes_rw_strong, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_medium=describe_posterior(bayes_rw_medium, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_weak=describe_posterior(bayes_rw_weak, rope_range = c(-0.0132, 0.0132))

describe_posterior_rw_cond_strong=describe_posterior(bayes_rw_cond_strong, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_cond_medium=describe_posterior(bayes_rw_cond_medium, rope_range = c(-0.0132, 0.0132))
describe_posterior_rw_cond_weak=describe_posterior(bayes_rw_cond_weak, rope_range = c(-0.0132, 0.0132))

save(describe_posterior_rw_strong, file = '07_bayes_comparisons/describe_posterior_rw_strong.Rdata')
save(describe_posterior_rw_medium, file = '07_bayes_comparisons/describe_posterior_rw_medium.Rdata')
save(describe_posterior_rw_weak, file = '07_bayes_comparisons/describe_posterior_rw_weak.Rdata')

save(describe_posterior_rw_cond_strong, file = '07_bayes_comparisons/describe_posterior_rw_cond_strong.Rdata')
save(describe_posterior_rw_cond_medium, file = '07_bayes_comparisons/describe_posterior_rw_cond_medium.Rdata')
save(describe_posterior_rw_cond_weak, file = '07_bayes_comparisons/describe_posterior_rw_cond_weak.Rdata')

#prior_predictive_check reward model medium prior
y=as.numeric(tab_ch$stay_frc_ch)
yrep=posterior_predict(bayes_rw_medium_prior_sampling)
group_vec=tab_ch$rw_oneback==1
png(file="/home/shared/0004_Ido_Set-size_effects/Counterfactual-credit-assignemnt-for-deliberated-and-unchosen-options-/graphics/prior_predictive_ch.png")
ppc_stat_grouped(y, yrep, group_vec)
dev.off()
#posterior_predictive_check
y=as.numeric(tab_ch$stay_frc_ch)
yrep=posterior_predict(bayes_rw_medium)
png(file="/home/shared/0004_Ido_Set-size_effects/Counterfactual-credit-assignemnt-for-deliberated-and-unchosen-options-/graphics/posterior_predictive_ch.png")
group_vec=tab_ch$rw_oneback==1
ppc_stat_grouped(y, yrep, group_vec)
dev.off()