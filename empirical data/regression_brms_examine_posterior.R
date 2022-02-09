# This code produces plots and distributions for the brms model results describing 
# the effect of previous reward on the probability of taking the unchosen option


rm(list=ls())
source('./functions/my_packages.r')

#----------------------------------------------------------------------------------
load('./data/empirical_data/brms_unchosen_weakly_informative_priors.rdata')

#plot reward fixed effects
plot(conditional_effects(model), plot = FALSE)[[1]] + 
  ggtitle("(A) empirical data") + 
  xlab("previous-outcome") +
  ylab("pStay(unchosen)") + theme_bw()+
  scale_x_discrete(labels=c("Unrewarded","Rewarded"))

#plot historgram for individual effects
source('./functions/my_posteriorplot.R')
posterior_samples = insight::get_parameters(model)
my_posteriorplot(x       = posterior_samples$b_reward_onebackrewarded,
                 myxlim  = c(-.75,+.25),
                 my_vline= 0, 
                 myxlab  = expression(beta['previous-outcome']),
                 mycolor = "blanchedalmond")+ylab('density')

#---------------------------------------------------------------------------------

load('./data/empirical_data/brms_unchosen_weakly_informative_priors.rdata')
load('./data/empirical_data/brms_unchosen_twoback_weakly_informative_priors.rdata')

describe_posterior(model)










