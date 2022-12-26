library(brms)
rm(list=ls())
load('./data/empirical_data_replication_1_YT/df.rdata')

#note2self - Yael coded 0 as safe 1 an harm, so coding is opp to what we are used too
df$reward_oneback = factor(lag(df$reward),level=c(0,1),labels=c('rewarded','unrewarded'))
df$subject        = factor(df$ID)
df$unchosen       = ifelse(df$offer1==lag(df$choice),df$offer2,df$offer1)
df$reoffer_ch     = (df$offer1==lag(df$choice)  | df$offer2==lag(df$choice))
df$reoffer_unch   = (df$offer1==lag(df$unchosen)| df$offer2==lag(df$unchosen))
df$stay_frc_ch    = (df$choice==lag(df$choice))*1
df$stay_frc_unch  = (df$choice==lag(df$unchosen))*1


df$reward_oneback = relevel(df$reward_oneback, ref = 'unrewarded')
contrasts(df$reward_oneback)


df$reward_n1back

#fit hierarchical regression and save individual estimates
model= glmer(stay_frc_unch ~ reward_n1back+(reward_n1back| subject), 
             data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)


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
           backend = 'cmdstan',
           prior=mypriors)

save(model,file='./data/empirical_data/brms_unchosen_replication_YT.rdata')
