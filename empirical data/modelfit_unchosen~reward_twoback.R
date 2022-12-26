library(brms)
rm(list=ls())
load('./data/empirical_data/df.rdata')


#-----------------------------------------------------------------------------------
#two-back analysis with weakly informative priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_twobackrewarded"))

model= brm(stay_frc_unch_twoback ~ 0 + Intercept+reward_twoback+(1+reward_twoback| subject), 
           data = df%>%filter(reoffer_ch_twoback==F,reoffer_unch_twoback==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_twoback_weakly_informative_priors.rdata')

