rm(list=ls())
library(brms)
library(bayestestR)
####reward_oneback effects---------------
load('./data/empirical_data/brms_unchosen_weakly_informative_priors.rdata')
describe_posterior(model)
load('./data/empirical_data/brms_unchosen_wide_priors.rdata')

load('./data/empirical_data/brms_unchosen_narrow_priors.rdata')


####condition effects-------------------
load('./data/empirical_data/brms_unchosen~reward_onebackXcondition.rdata')
describe_posterior(model)


####reward_twoback effects---------------
load('./data/empirical_data/brms_unchosen_twoback_weakly_informative_priors.rdata')
describe_posterior(model)

####rt_oneback effects-------------------
load('./data/empirical_data/brms_unchosen_weakly_informative_priors_rt_oneback.rdata')
conditional_effects(model)


####deltaEV_oneback effects-------------------
load('./data/empirical_data/brms_unchosen_weakly_informative_priors_delta_abs_exp.rdata')
model_h1=model
load('./data/empirical_data/brms_unchosen_weakly_informative_priors_delta_abs_exp_comparision.rdata')
model_h0=model
model_h1=loo(model_h1)
model_h0=loo(model_h0)
loo_compare(model_h0,model_h1)
#stacking weights
stack_models <- cbind(model_h1$pointwise[,"elpd_loo"],
                      model_h0$pointwise[,"elpd_loo"])
loo::stacking_weights(stack_models)
