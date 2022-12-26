library(brms)
rm(list=ls())
df=read.csv('./data/empirical_data_replication_2_MP/df.csv')


df$unchosen      = ifelse(df$choice_student==df$offer1,df$offer2,df$offer1)
df$stay_frc_unch = (df$choice_student == lag(df$unchosen))*1
df$reoffer_ch    = ((df$offer1 == lag(df$choice_student)) | (df$offer2 == lag(df$choice_student)))*1 
df$reoffer_unch  = ((df$offer1 == lag(df$unchosen)) | (df$offer2 == lag(df$unchosen)))*1 

df$reward_oneback = relevel(df$reward_oneback, ref = 'unrewarded')
contrasts(df$reward_oneback)


df$reward_n1back

#fit hierarchical regression and save individual estimates
model= glmer(stay_frc_unch ~ reward_oneback+(reward_oneback| subject), 
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
