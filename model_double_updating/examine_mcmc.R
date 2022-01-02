rm(list=ls())
model_name=c('null')
folder_name=c('')

rl_fit=readRDS(paste('./data/',model_name,'_RDSfile.rds',sep=""))
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
