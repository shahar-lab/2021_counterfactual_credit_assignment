rm(list = ls())
library("bayesplot")
library("loo")
library("brms")
library(dplyr)
library(tidyverse)
library(bayestestR)
#load data
load('data/analysis_data/tab.Rdata')
load('data/analysis_data/tab_ch.Rdata')

#load models
load('data/brms_data/model_null_ch_medium.Rdata')
load('data/brms_data/model_rw_ch_medium.Rdata')
load('data/brms_data/model_rw_ch_cond_medium.Rdata')

loo_null_ch <- loo(bayes_null_ch_medium)
save(loo_null_ch, file = 'data/brms_model_comparison_data/loo_null_ch.Rdata')
loo_rw_ch <- loo(bayes_rw_ch_medium)
save(loo_rw_ch, file = 'data/brms_model_comparison_data/loo_rw_ch.Rdata')
loo_rw_ch_cond <- loo(bayes_rw_ch_cond_medium)
save(loo_rw_ch_cond, file = 'data/brms_model_comparison_data/loo_rw_ch_cond.Rdata')

#compary with loo
loo_cmpr_ch=loo_compare(loo_null_ch,loo_rw_ch,loo_rw_ch_cond)
save(loo_cmpr_ch, file = 'data/brms_model_comparison_data/loo_cmpr_ch.Rdata')
loo_cmpr_ch

#stacking weights
lpd_point_ch <- cbind(loo_null_ch$pointwise[,"elpd_loo"],
                   loo_rw_ch$pointwise[,"elpd_loo"],
                   loo_rw_ch_cond$pointwise[,"elpd_loo"])
stacking_weights(lpd_point_ch)

save(lpd_point_ch, file = 'data/brms_model_comparison_data/lpd_point_ch.Rdata')

#compare with bayesfactor_models
BF_model_comparison_ch <- bayesfactor_models(bayes_rw_ch_medium, bayes_rw_ch_cond_medium, denominator =bayes_null_ch_medium)
BF_model_comparison_ch
save(BF_model_comparison_ch, file = 'data/brms_model_comparison_data/BF_model_comparison_ch.Rdata')



