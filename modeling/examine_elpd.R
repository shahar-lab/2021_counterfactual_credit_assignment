#This code plot recovered parameters against the true parameters

rm(list=ls())
# samples x observations

#--------------------------------------------------------------------------------------------------------
library(loo)

mymodel='null'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
dim(like)
null=elpd(like)

mymodel='deliberation'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
dim(like)
deliberation=elpd(like)


mymodel='single_prediction_error'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial_and_chain.rdata'))
dim(like)
single_prediction_error=elpd(like)


mymodel='double_updating'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
dim(like)
double_updating=elpd(like)


mymodel='approach_avoid'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
dim(like)
approach_avoid=elpd(like)


loo_compare(null,double_updating)
t.test(null,deliberation)

#compare model trial-by-trial-----------------------------------------
load('./data/empirical_data/df.rdata')
