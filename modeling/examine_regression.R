# Aim: Run some basic sanity checks using linear / logistic regression
rm(list=ls())
myfolder='./model_double_updating/'
#--------------------------------------------------------------------------------------------------------


load(paste0(myfolder,'./data/simdata_based_on_empirical_parameters.rdara'))
load(paste0(myfolder,'./data/simulate_data_based_on_artificial_parameters.rdata'))



df=df%>%mutate(delta_exp_value        =abs(expval_ch-expval_unch),
               deltaQ                 =abs(Qval_ch - Qval_unch),)%>%
        mutate(delta_exp_value_oneback=lag(delta_exp_value),
               deltaQ_oneback         =lag(deltaQ),
               reoffer_ch             =(offer1==lag(choice)|offer2==lag(choice)),
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
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

model= glmer(stay_frc_ch ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==T,reoffer_unch==T), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

library(effects)
plot(effect('reward_oneback',model))
library(car)
Anova(model)
library(lme4)
model= lmer(Qval_unch ~ expval_unch+(expval_unch| subject), 
             data = df)
