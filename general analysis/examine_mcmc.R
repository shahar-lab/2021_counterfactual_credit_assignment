rm(list=ls())
myfolder='./model_double_updating/'
#--------------------------------------------------------------------------------------------------------


rl_fit=readRDS(paste0(myfolder,'modelfit_based_on_empirical_data.rds'))
library(bayesplot)
library(ggplot2)
library(rstan)

mypars = c('population_locations[1]',
           'population_locations[2]',
           'population_scales[1]',
           'population_scales[2]')

#Trace plots
mcmc_trace(rl_fit,pars=mypars)
mcmc_pairs(rl_fit,pars=mypars)
