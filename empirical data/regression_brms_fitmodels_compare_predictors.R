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
#reward_oneback X conditions as weakly informative priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackrewarded"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "cond"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackrewarded:cond"))

model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback*cond+(1+reward_oneback*cond| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_reward_oneback_x_cond_weakly_informative_priors_with_reward_onebackXcond_random.rdata')


#-----------------------------------------------------------------------------------
#set weakly informative priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_onebackrewarded"))

model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback+(1+reward_oneback*cond| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_reward_oneback_weakly_informative_priors_with_reward_onebackXcond_random.rdata')




#-----------------------------------------------------------------------------------
#null model with only intercept and weakly informative priors
mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"))

model= brm(stay_frc_unch ~ 0 + Intercept+(1+reward_oneback*cond| subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_empty_model_weakly_informative_priors_with_reward_onebackXcond_random.rdata')
