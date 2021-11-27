rm(list = ls())
library("bayesplot")
library("loo")
library("brms")
library(dplyr)
library(tidyverse)
library(bayestestR)
#load data
load('data/tab.Rdata')
tab_unch=tab%>%filter(reoffer_unch,!reoffer_ch)

#load models
load('06_brms/model_null_unch_medium.Rdata')
load('06_brms/model_rw_unch_medium.Rdata')
load('06_brms/model_rw_unch_cond_medium.Rdata')

loo_null_unch <- loo(bayes_null_unch_medium)
save(loo_null_unch, file = '08_loo_model_comparison/loo_null_unch.Rdata')
loo_rw_unch <- loo(bayes_rw_unch_medium)
save(loo_rw_unch, file = '08_loo_model_comparison/loo_rw_unch.Rdata')
loo_rw_unch_cond <- loo(bayes_rw_unch_cond_medium)
save(loo_rw_unch_cond, file = '08_loo_model_comparison/loo_rw_unch_cond.Rdata')

#compary with loo
loo_cmpr_unch=loo_compare(loo_null_unch,loo_rw_unch,loo_rw_unch_cond)
save(loo_cmpr_unch, file = '08_loo_model_comparison/loo_cmpr_unch.Rdata')
loo_cmpr_unch

#stacking weights
lpd_point_unch <- cbind(loo_null_unch$pointwise[,"elpd_loo"],
                      loo_rw_unch$pointwise[,"elpd_loo"],
                      loo_rw_unch_cond$pointwise[,"elpd_loo"])
stacking_weights(lpd_point_unch)

save(lpd_point_unch, file = '08_loo_model_comparison/lpd_point_unch.Rdata')

#compare with bayesfactor_models
BF_model_comparison_unch <- bayesfactor_models(bayes_rw_unch_medium,bayes_rw_unch_cond_medium, denominator =bayes_null_unch_medium)
BF_model_comparison_unch
BF_model_inclusion_unch=bayesfactor_inclusion(BF_model_comparison_unch)
save(BF_model_comparison_unch, file = '08_model_comparison/BF_model_comparison_unch.Rdata')

