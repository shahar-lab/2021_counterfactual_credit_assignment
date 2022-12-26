library(brms)
rm(list=ls())
load('./data/empirical_data/df.rdata')


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
#comparision model
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



