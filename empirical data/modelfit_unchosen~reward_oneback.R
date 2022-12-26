library(brms)
rm(list=ls())
load('./data/empirical_data/df.rdata')


#-----------------------------------------------------------------------------------
#set weakly informative priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackrewarded"))

model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback+(1+reward_oneback| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_weakly_informative_priors.rdata')


#-----------------------------------------------------------------------------------
#set wide priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.4)", class = "b",coef = "reward_onebackrewarded"))



model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback+(1+reward_oneback| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_wide_priors.rdata')




#-----------------------------------------------------------------------------------
#narrow
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.09)", class = "b",coef = "reward_onebackrewarded"))

model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback+(1+reward_oneback| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_narrow_priors.rdata')
