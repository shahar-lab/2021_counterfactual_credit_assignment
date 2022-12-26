# This code fit and describe hierarchical bayesian regrssion model to estimate the effect 
# of previous reward on the probability of taking the unchosen option

rm(list=ls())
source('./functions/my_packages.r')


# load data 
load('./data/empirical_data/df.rdata')
df$reward_oneback=factor(df$reward_oneback,labels=c('unrewarded','rewarded'))
df$reward_twoback=factor(df$reward_twoback,labels=c('unrewarded','rewarded'))

df<-na.omit(df)

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


#-----------------------------------------------------------------------------------
#moderation by delta ev
df$delta_abs_exp_value_oneback=abs(df$delta_exp_value_oneback)

mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackrewarded"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "delta_abs_exp_value_oneback"),
           set_prior(prior = "normal(0,0.1)", class = "b",coef = "reward_onebackrewarded:delta_abs_exp_value_oneback")
           )

model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback*delta_abs_exp_value_oneback+(1+reward_oneback*delta_abs_exp_value_oneback| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           backend = "cmdstan",
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_weakly_informative_priors_delta_abs_exp.rdata')
#note2self: I also ran priorpredictive check and save the model in the data folder



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

