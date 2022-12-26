library(brms)
rm(list=ls())
load('./data/empirical_data/df.rdata')


#### reward_oneback*condition -----------------------------------------------
rm(list=ls())
library(brms)
library(dplyr)
load('./data/empirical_data/df.rdata')

contrasts(df$reward_oneback)
contrasts(df$condition)=c(-1,1)

#tune priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackwin"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "condition1"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackwin:condition1"))

#compile
model= brm(stay_frc_unch ~ 0 + Intercept + reward_oneback*condition + (1+reward_oneback*condition| subject), 
           data   = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1,
           iter   = 2,    
           cores  = 1,
           chains = 1,
           prior=mypriors,
           backend = "cmdstan")

prior_summary(model)

#sample
model=update(model, 
             iter   = 2000, 
             warmup = 1000, 
             #sample_prior = 'only', 
             chains = 4,
             cores  = 4)

save(model,file='./data/empirical_data/brms_unchosen~reward_onebackXcondition.rdata')



#### reward_oneback + condition for comparision-----------------------------------------------
rm(list=ls())
library(brms)
library(dplyr)
load('./data/empirical_data/df.rdata')

contrasts(df$reward_oneback)
contrasts(df$condition)=c(-1,1)

#tune priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackwin")
           )

#compile
model= brm(stay_frc_unch ~ 0 + Intercept + reward_oneback + (1+reward_oneback*condition| subject), 
           data   = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1,
           iter   = 2,    
           cores  = 1,
           chains = 1,
           prior=mypriors,
           backend = "cmdstan")

prior_summary(model)

#sample
model=update(model, 
             iter   = 2000, 
             warmup = 1000, 
             #sample_prior = 'only', 
             chains = 4,
             cores  = 4)

save(model,file='./data/empirical_data/brms_unchosen~reward_oneback_withrwXcond_randeff.rdata')