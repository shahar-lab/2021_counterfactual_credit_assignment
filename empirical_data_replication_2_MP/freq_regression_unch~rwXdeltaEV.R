
rm(list=ls())
df=read.csv('./data/empirical_data_replication_2_MP/df.csv')

df$unchosen      = ifelse(df$choice_student==df$offer1,df$offer2,df$offer1)
df$stay_frc_unch = (df$choice_student == lag(df$unchosen))*1
df$delta_exp_value_oneback=abs(df$prob_diff)
df$reoffer_ch    = ((df$offer1 == lag(df$choice_student)) | (df$offer2 == lag(df$choice_student)))*1 
df$reoffer_unch  = ((df$offer1 == lag(df$unchosen)) | (df$offer2 == lag(df$unchosen)))*1 

hist(df$prob_diff)

model= glmer(stay_frc_unch ~ reward_oneback*delta_exp_value_oneback+(reward_oneback*delta_exp_value_oneback| subject), 
             data = df%>%filter(reoffer_ch==F,reoffer_unch==T,reveal_oneback==0,reveal==0), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

library(effects)
plot(effect('reward_oneback:delta_exp_value_oneback',model,xlevels=3))
library(car)
Anova(model)
