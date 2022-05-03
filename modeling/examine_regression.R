# Aim: Run some basic sanity checks using linear / logistic regression
rm(list=ls())


#--------------------------------------------------------------------------------------------------------
mymodel   =read.table('./modeling/working_model.txt')
data_path =paste0('./data/model_',mymodel)
model_path=paste0('./modeling/model_',mymodel,'/',mymodel,'_')

load(paste0(data_path,'./data/simdata_based_on_empirical_parameters.rdata'))
load(paste0(data_path,'/simulate_data_based_on_artificial_parameters.Rdata'))



df=df%>%mutate(reoffer_ch             =(offer1==lag(choice)|offer2==lag(choice)),
               reoffer_unch           =(offer1==lag(unchosen)|offer2==lag(unchosen)),
               stay_frc_ch            =(choice==lag(choice)),
               stay_frc_unch          =(choice==lag(unchosen)),
               reward_oneback         =lag(reward))
               


model= glmer(stay_frc_ch ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==T,reoffer_unch==F), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)


model= glmer(stay_frc_unch ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)

model= glmer(stay_frc_ch ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==T,reoffer_unch==T), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

library(effects)
plot(effect('reward_oneback',model),ylim=c(-0.4,0))
#note ido ran the brms on his computer