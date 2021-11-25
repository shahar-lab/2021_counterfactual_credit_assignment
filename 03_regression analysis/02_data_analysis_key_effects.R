rm(list=ls())
library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(brms)
library(parallel)
detectCores()
load('myfolder/03_data/01_raw_data/03_raw_clean_data/tab.Rdata')
load('myfolder/03_data/01_raw_data/03_raw_clean_data/oci.Rdata')
load('myfolder/03_data/01_raw_data/03_raw_clean_data/bdi.Rdata')
load('myfolder/03_data/01_raw_data/03_raw_clean_data/stai.Rdata')


##### key stay effect --------------------------------------------------------
#means

tab%>%
  filter(!reoffer_ch,!reoffer_unch)%>%
  group_by(subj,cond,rw_oneback)%>%
  summarise(pStay_key=mean(stay_key))%>%
  pivot_wider(names_from=c(cond,rw_oneback),values_from  =pStay_key)%>%
  colMeans()

#GLM Logistic Regression

library(lme4)
library(effects)
bdi<-na.omit(bdi)
names(tab)

m<-glmer(stay_key ~ 1+rw_oneback*cond + (1+rw_oneback*cond|prolific_id), 
         data = tab%>%filter(!reoffer_ch,!reoffer_unch), 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 1)
Anova(m)
plot(effect('rw_oneback:cond',m))

task.scores<-data.frame(prolific_id=rownames(coef(m)$prolific_id),key_effect=coef(m)$prolific_id[,2],key_cond_interaction=coef(m)$prolific_id[,4])
df<-merge(oci,task.scores,by=c('prolific_id'))
df<-merge(df,bdi,by=c('prolific_id'))
df<-merge(df,stai,by=c('prolific_id'))
names(df)

Anova(m1<-lm(key_effect~comp+bdi+state+trait,data=df))
Anova(m1<-lm(key_cond_interaction~comp+bdi+state+trait,data=df))
Anova(m2<-lm(comp~key_effect+key_cond_interaction,data=df))
Anova(m2<-lm(comp~key_effect+key_cond_interaction+bdi+state+trait,data=df))


library(effects)
plot(m1)
plot(effect('comp',m1))
plot(effect('comp',m2))

#HLM
m<-brm(stay_key ~ 1+rw_oneback + (1+rw_oneback|subj), data = tab, family = bernoulli(link = "logit"),cores=4)
#not run: save(m,file='myfolder/02_models/brms_stay_key.Rdata')
load('myfolder/02_models/brms_stay_key.Rdata')
summary(m)
plot(m, pars = c("rw_oneback")) 
library(tidybayes)
get_variables(m)
m %>%
  spread_draws(r_subjcondition,term) %>%
  median_qi(b_Intercept)






























######archive------------------------------------
sr<-merge(oci,bdi,by=c('prolific_id'))
sr<-merge(sr,stai,by=c('prolific_id'))
names(sr)
sr<-na.omit(sr)
m<-lm(comp~bdi+state+trait,data=sr)
sr<-data.frame(prolific_id=sr$prolific_id,comp_res=m$residuals)
sr<-merge(data.frame(prolific_id=oci$prolific_id),sr,by=c('prolific_id'),all=T)

#add clinical scores complsivity

for (ind in unique(tab$prolific_id)) {
  tab[tab$prolific_id == ind, "comp"] = oci[oci$prolific_id == ind, "comp"]
  tab[tab$prolific_id == ind, "bdi"] = bdi[bdi$prolific_id == ind, "bdi"]
  tab[tab$prolific_id == ind, "state"] = stai[stai$prolific_id == ind, "state"]
  tab[tab$prolific_id == ind, "trait"] = stai[stai$prolific_id == ind, "trait"]
  tab[tab$prolific_id == ind, "comp_res"] = sr[sr$prolific_id == ind, "comp_res"]
}
#for ido power analysis
df<-
tab%>%
  filter(!reoffer_ch,!reoffer_unch)%>%
  mutate(rw_oneback=factor(rw_oneback,labels = c('unrewarded','rewarded')))%>%
  group_by(subj,rw_oneback)%>%
  summarise(pStay_key=mean(stay_key))%>%
  pivot_wider(names_from=c(rw_oneback),values_from  =pStay_key)%>%as.data.frame

save(df,file = 'key_reward_effect_inbal_alon_study.Rdata')
library(effsize)
cohen.d(df$unrewarded,df$rewarded)


        