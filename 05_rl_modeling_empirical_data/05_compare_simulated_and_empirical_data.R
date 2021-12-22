rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(effects)
source('./functions/my_xyplot.r')

load('./data/tab.Rdata')
#----------------------------------------------------------------------------------------------------------------------
#compare cor across Qvalue from simulation based on individual parameters and unchosen reward effect

#calculate mean cor score for each individual based on simulation data with EV=0.5 for all bandits
load('./data/tab_simdata_based_on_2PEs_extracted_parameters_allbanditaat50%.Rdata')

df=
df%>%group_by(subject)%>%
     summarise(Qval_cor=mean(cor(Qbandit1,Qbandit2),
                             cor(Qbandit1,Qbandit3),
                             cor(Qbandit1,Qbandit4),
                             cor(Qbandit2,Qbandit3),
                             cor(Qbandit3,Qbandit4)))%>%as.data.frame()

#get reward effect on unchosen option
library(lme4)
m.unchosen<-glmer(stay_frc_unch ~ 1+rw_oneback + (1+rw_oneback|subj), 
                  data = tab%>%filter(!reoffer_ch,reoffer_unch), 
                  family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 1)

df$unchosen_reward_effect=coef(m.unchosen)$subj[,2]
my_xyplot(elpd_df$coef_ch,elpd_df$diff,expression(beta['previous-outcome']),'elpd_diff (2pe - null)','blue')

df%>%select(unchosen_reward_effect,Qval_cor)
cor.test(df$cor1,df$unchosen_reward_effect)
t.test(df$Qval_cor)

df=df%>%mutate(stay_chosen=(choice==lag(choice,default=0))*1,
               stay_unchosen=(choice==lag(unchosen,default=0))*1,
               reward_oneback=lag(reward,default=0),
               reoffer_ch=(offer1==lag(choice) | offer2==lag(choice)),
               reoffer_unch=(offer1==lag(unchosen) | offer2==lag(unchosen)))

#chosen
model<-glmer(stay_chosen ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch,!reoffer_unch), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
Anova(model)
plot(effect('reward_oneback',model))

m<-glmer(stay_frc_ch ~ 1+rw_oneback + (1+rw_oneback|subj), 
         data = tab%>%filter(reoffer_ch,!reoffer_unch), 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 0)
Anova(m)
plot(effect('rw_oneback',m))

plot(ranef(model)$subject[,2],ranef(m)$subj[,'rw_oneback'])

ggplot(data.frame(
  x =coef(model)$subject[-110,2], 
  y =coef(m)$subj[-110,2]),
  aes(x=x,y=y))+geom_point(col='orange',alpha=0.7)+
  ggtitle('',subtitle = paste('r=',round(cor(coef(model)$subject[-110,2],coef(m)$subj[-110,2]),3)))+
  xlab(expression(paste('simulated ',beta['previous-outcome'])))+ylab(expression(paste('observed ',beta['previous-outcome'])))+ 
  #xlim(0,1)+ylim(0,1)+
  theme_classic()


#unchosen

model<-glmer(stay_unchosen ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(!reoffer_ch,reoffer_unch), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))


m<-glmer(stay_frc_unch ~ rw_oneback + (rw_oneback|subj), 
         data = tab%>%filter(!reoffer_ch,reoffer_unch), 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 0)
Anova(m)
library(effects)
plot(effect('rw_oneback',m,xlevels=2))
plot(ranef(model)$subject[-110,2],ranef(m)$subj[-110,2],)
cor.test(ranef(model)$subject[-110,2],ranef(m)$subj[-110,2],)
ggplot(data.frame(
  x =coef(model)$subject[-110,2], 
  y =coef(m)$subj[-110,2]),
  aes(x=x,y=y))+geom_point(col='purple',alpha=0.7)+
  ggtitle('',subtitle = paste('r=',round(cor(coef(model)$subject[-110,2],coef(m)$subj[-110,2]),3)))+
  xlab(expression(paste('simulated ',beta['previous-outcome'])))+ylab(expression(paste('observed ',beta['previous-outcome'])))+ 
  #xlim(0,1)+ylim(0,1)+
  theme_classic()



###loo
rm(list=ls())
----------compare
library(loo)
load(file='./data/tab_fitted_with_null_lo-block-out.rdata')
null<-do.call(rbind,my_log_lik)
null<-elpd(null)
load(file='./data/tab_fitted_with_2PEs_lo-block-out.rdata')
twoPE<-do.call(rbind,my_log_lik)
twoPE<-elpd(twoPE)

elpd_df=data.frame(subject = 1:178,
                   null_elpd   = null$pointwise[,'elpd'],
                   twoPE_elpd  = twoPE$pointwise[,'elpd'])
elpd_df$diff=elpd_df$twoPE_elpd-elpd_df$null_elpd

library(lme4)
library(dplyr)
load('./data/tab.Rdata')

m<-glmer(stay_frc_ch ~ 1+rw_oneback + (1+rw_oneback|subj), 
         data = tab%>%filter(reoffer_ch,!reoffer_unch), 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 1)

elpd_df$ranef_ch=ranef(m)$subj[,2]
elpd_df$coef_ch=coef(m)$subj[,2]

m<-glmer(stay_frc_unch ~ rw_oneback + (rw_oneback|subj), 
         data = tab%>%filter(!reoffer_ch,reoffer_unch), 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 1)

elpd_df$ranef_unch=ranef(m)$subj[,2]
elpd_df$coef_unch=coef(m)$subj[,2]

plot(elpd_df$ranef_ch,elpd_df$diff)

source('./functions/my_xyplot.r')
my_xyplot(elpd_df$coef_ch,elpd_df$diff,expression(beta['previous-outcome']),'elpd_diff (2pe - null)','blue')
my_xyplot(elpd_df$coef_unch,elpd_df$diff,expression(beta['previous-outcome']),'elpd_diff (2pe - null)','blue')
cor.test(elpd_df$coef_unch[-129],elpd_df$diff[-129])

cor.test(elpd_df$coef_ch,elpd_df$diff)

plot(elpd_df$ranef_unch,elpd_df$diff)
cor.test(elpd_df$ranef_unch,elpd_df$diff)

load('./data/tab_fitted_with_2PEs_extracted_parameters.rdata')
elpd_df$alpha_ch  =apply(pars$alpha_ch,2,median)
elpd_df$alpha_unch=apply(pars$alpha_unch,2,median)
elpd_df$beta      =apply(pars$beta,2,median)

library(car)
library(effects)
m=lm(ranef_ch~alpha_ch+alpha_unch+beta,data=elpd_df)
Anova(m)
plot(effect('alpha_ch',m))
plot(effect('alpha_unch',m))
plot(effect('beta',m))

plot(elpd_df$alpha_ch,elpd_df$ranef_ch)

m2=lm(ranef_unch~alpha_ch+alpha_unch+beta,data=elpd_df)
Anova(m2)
plot(effect('alpha_ch',m2))
plot(effect('alpha_unch',m2))
plot(effect('beta',m2))

plot(effect('alpha_unch',m))
plot(elpd_df$alpha_unch,elpd_df$ranef_unch)

plot(effect('alpha_ch',m))
plot(elpd_df$alpha_ch,elpd_df$ranef_unch)

m=lm(diff~alpha_ch+alpha_unch+beta,data=elpd_df)
Anova(m)
plot(effect('alpha_ch',m),xlab=c(expression(alpha['chosen'])),ylab=c('elpd diff (2PE - null)'))
plot(effect('alpha_unch',m),xlab=c(expression(alpha['unchosen'])),ylab=c('elpd diff (2PE - null)'))
plot(effect('beta',m),xlab=c(expression(beta)),ylab=c('elpd diff (2PE - null)'))


tab%>%group_by(subj)%>%summarise(meanReward=mean(rw))%>%
