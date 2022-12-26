library(brms)
library(dplyr)
rm(list=ls())
load('./data/empirical_data/df.rdata')


#-----------------------------------------------------------------------------------
#moderation by previous trial rt

mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackrewarded"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "rt_oneback"),
           set_prior(prior = "normal(0,0.1)", class = "b",coef = "reward_onebackrewarded:rt_oneback")
)
df$rt_oneback=df$rt_oneback/1000

model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback*rt_oneback+(1+reward_oneback*rt_oneback| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           backend = "cmdstan",
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_weakly_informative_priors_rt_oneback.rdata')

#-----------------------------------------------------------------------------------
#moderation by previous trial rt

mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackrewarded"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "rt_oneback"))

df$rt_oneback=df$rt_oneback/1000

model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback+rt_oneback+(1+reward_oneback*rt_oneback| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           backend = "cmdstan",
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_weakly_informative_priors_rt_oneback_comparision.rdata')

