# Here we examine whether parameters of the winning model, approach-avoid, predicts the reward_oneback effect over unchosen actions

rm(list=ls())
source('./functions/my_packages.r')


#load parameters of avoid-approach
load('./data/model_approach_avoid/modelfit_based_on_empirical_data.rdata')
par_alpha=apply(plogis(pars$alpha_ch_random_effect),2,mean)
par_beta =apply(plogis(pars$beta_random_effect),2,mean)
par_omega=apply(plogis(pars$omega_random_effect),2,mean)

#load unchosen effect from brms fit
load('./data/empirical_data/reward_oneback_effect_on_unchosen_hierarchical_fit_brms.rdata')


#plot histogram for individual effects----------------------------------------------
source('./functions/my_posteriorplot.R')

my_posteriorplot(x       = reward_oneback_effect_on_unchosen,
                 myxlim  = c(-.75,+.25),
                 my_vline= 0, 
                 myxlab  = expression(beta['previous-outcome']),
                 mycolor = "blanchedalmond")+ylab('density')


#correlation between omega and model-independent effect
library(BayesFactor)
result <- correlationBF(reward_oneback_effect_on_unchosen, apply(plogis(pars$omega_random_effect),2,mean))
describe_posterior(result)

#xyplot for the association between omega and reward_oneback influence on unchosen 
source('./functions/my_xyplot.R')
my_xyplot(par_omega,reward_oneback_effect_on_unchosen,expression(omega),expression(beta['previous-outcome']),'brown')+geom_smooth(method='lm')

