rm(list=ls())
library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(brms)
library(parallel)
library(lme4)
library(raincloudplots)
num_cores=detectCores()
load('data/tab.Rdata')
load('data/oci.Rdata')
load('data/bdi.Rdata')
load('data/stai.Rdata')


##### Fractal stay effect --------------------------------------------------------
#means
tab%>%
  filter(reoffer_ch==T,reoffer_unch==F)%>%
  group_by(subj,cond,rw_oneback)%>%
  summarise(pStay_ch=mean(stay_frc_ch))%>%
  pivot_wider(names_from=c(cond,rw_oneback),values_from  =pStay_ch)%>%
  colMeans()

# ggplot
df<-
  tab%>%
  filter(reoffer_ch==T,reoffer_unch==F)%>%
  mutate(rw_oneback=factor(rw_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(subj,cond,rw_oneback)%>%
  summarise(pStay_ch=mean(stay_frc_ch))%>%
  pivot_wider(names_from=c('rw_oneback'),values_from=c('pStay_ch'))

df<-data_1x1( array_1 = df$unrewarded,array_2 = df$rewarded,jit_distance = .09, jit_seed = 321)

raincloud_1 <- raincloud_1x1_repmes(data = df,colors = (c('dodgerblue', 'darkorange')), fills = (c('dodgerblue', 'darkorange')),line_color = 'gray',
                                    line_alpha = .3,size = 1,  alpha = .6,  align_clouds = FALSE) +  
  scale_x_continuous(breaks=c(1,2), labels=c("unrewarded", "rewarded"), limits=c(0, 3)) +
  xlab("previous-outcome") +  ylab("P(stay_chosen)") + theme_classic()

raincloud_1

#ggsave('myfolder/03_data/03 graphics/fig_chosen.png', raincloud_1,width = 3, height = 2)


#HLM
# m<-brm(stay_frc_ch ~ 1+rw_oneback + (1+rw_oneback|subj), data = tab[tab$reoffer_ch==T&reoffer_unch==F,], family = bernoulli(link = "logit"),cores=2)
# save(m,file='myfolder/02_models/brms_stay_chosen.Rdata')
# load('myfolder/02_models/brms_stay_chosen.Rdata')
# summary(m)
# plot(m, pars = c("rw_oneback")) 



#Maximum Likelihood
m_ch<-glmer(stay_frc_ch ~ 1+rw_oneback*cond + (1+rw_oneback*cond|prolific_id), 
         data = tab%>%filter(reoffer_ch,!reoffer_unch), 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 1)
Anova(m_ch)

task.scores<-data.frame(prolific_id=rownames(coef(m_ch)$prolific_id),effect_ch=coef(m_ch)$prolific_id[,2],effect_chxcond=coef(m_ch)$prolific_id[,4])
df<-merge(oci,task.scores,by=c('prolific_id'))
df<-merge(df,bdi,by=c('prolific_id'))
df<-merge(df,stai,by=c('prolific_id'))
Anova(m_ch<-lm(effect_ch~comp,data=df))
Anova(m_ch<-lm(effect_ch~comp+bdi+state+trait,data=df))
Anova(m_ch<-lm(effect_chxcond~comp,data=df))
Anova(m_ch<-lm(effect_chxcond~comp+bdi+state+trait,data=df))

##### Counterfactual stay effect --------------------------------------------------------
#means
tab%>%
  filter(reoffer_ch==F,reoffer_unch==T)%>%
  group_by(subj,cond,rw_oneback)%>%
  summarise(pStay_unch=mean(stay_frc_unch))%>%
  pivot_wider(names_from=c(cond,rw_oneback),values_from  =pStay_unch)%>%
  colMeans()

# ggplot
df<-
tab%>%
  filter(reoffer_ch==F)%>%
  mutate(rw_oneback=factor(rw_oneback,levels=c(0,1),labels=c('unrewarded','rewarded')))%>%
  group_by(subj,cond,rw_oneback)%>%
  summarise(pStay_unch=mean(stay_frc_unch))%>%
  pivot_wider(names_from=c('rw_oneback'),values_from=c('pStay_unch'))

df<-data_1x1( array_1 = df$unrewarded,array_2 = df$rewarded,jit_distance = .09, jit_seed = 321)

raincloud_2 <- raincloud_1x1_repmes(data = df,colors = (c('dodgerblue', 'darkorange')), fills = (c('dodgerblue', 'darkorange')),line_color = 'gray',
                                    line_alpha = .3,size = 1,  alpha = .6,  align_clouds = FALSE) +  
  scale_x_continuous(breaks=c(1,2), labels=c("unrewarded", "rewarded"), limits=c(0, 3)) +
  xlab("previous-outcome") +  ylab("P(stay_unchosen)") + theme_classic()

raincloud_2

#ggsave('myfolder/03_data/03 graphics/fig_unchosen.png', raincloud_2,width = 3, height = 2)


#HLM
#m<-brm(stay_frc_unch ~ 1+rw_oneback + (1+rw_oneback|subj), data = tab[tab$reoffer_ch==F&reoffer_unch==T,], family = bernoulli(link = "logit"),cores=2)
#save(m,file='myfolder/02_models/brms_stay_unchosen.Rdata')
# load('myfolder/02_models/brms_stay_unchosen.Rdata')
# summary(m)
# plot(m, pars = c("rw_oneback")) 


#Maximum Likelihood

tab_unch= tab%>%filter(!reoffer_ch,reoffer_unch)
tab_unch_two= tab%>%filter(!reoffer_ch_two,reoffer_unch_two)

m_unch<-glmer(stay_frc_unch ~ 1+rw_oneback*cond + (1+rw_oneback*cond|prolific_id), 
         data = tab_unch, 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 1)
Anova(m_unch)

m_unch_two<-glmer(stay_frc_unch_two ~ 1+rw_twoback*cond + (1+rw_twoback*cond|prolific_id), 
              data = tab_unch_two, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 1)
Anova(m_unch_two)


task.scores2<-data.frame(prolific_id=rownames(coef(m_unch)$prolific_id),effect_unc=coef(m_unch)$prolific_id[,2])
df<-merge(oci,task.scores,by=c('prolific_id'))
df<-merge(df,bdi,by=c('prolific_id'))
df<-merge(df,stai,by=c('prolific_id'))
Anova(m_unch<-lm(effect_unc~comp,data=df))
Anova(m_unch<-lm(effect_unc~comp+bdi+state+trait,data=df))



#corr effects
df<-merge(task.scores,task.scores2,by=c('prolific_id'))
cor.test(df$effect_ch,df$effect_unc)


# YaelTroudart ------------------------------------------------------------

load('data/replications/yaelT.Rdata')
task%>%mutate(stay_unch_card=)
task_unch=task%>%filter(reoffered_chosen==0,reoffered_unchosen=="Yes")
task_unch%>%group_by(r)
#Archive----------------------------------------------------------------------
# credit assignment to the previous trial ---------------------------------
#C D (selected C) /  A B (selected A) / C D are offered again.
#we check credit assignment to C as a function of reward in the second trial (where A was selected)

#add columns
tab$rw_twoback            <-shift(tab$rw,n=2,type='lag',fill=0)
tab$stay_frc_ch_twoback   <-tab$ch==shift(tab$ch,n=2,type='lag',fill=0)*1
tab$stay_frc_unch_twoback <-tab$ch==shift(tab$unch,n=2,type='lag',fill=0)*1
tab$stay_key_twoback     <-tab$key==shift(tab$key,n=2,type='lag',fill=0)
tab$reoffer_ch_twoback   <-(tab$frcA==shift(tab$ch,n=2,type='lag',fill=0)|tab$frcB==shift(tab$ch,n=2,type='lag',fill=0))
tab$reoffer_unch_twoback <-(tab$frcA==shift(tab$unch,n=2,type='lag',fill=0)|tab$frcB==shift(tab$unch,n=2,type='lag',fill=0))


tab%>%
  filter(reoffer_ch==F,reoffer_unch==F,reoffer_ch_twoback==T,reoffer_unch_twoback==T)%>%
  group_by(subj,rw_oneback)%>%
  summarise(pStay_ch=mean(stay_frc_ch_twoback))%>%
  na.omit() %>%
  pivot_wider(names_from=c(rw_oneback),values_from  =pStay_ch)%>%
  colMeans()

library(lme4)
tab%>%
  filter(reoffer_ch==F,reoffer_unch==F,reoffer_ch_twoback==T,reoffer_unch_twoback==T)%>%
  glmer(stay_frc_ch_twoback ~ rw_oneback+(rw_oneback | subj), data = ., family = binomial, control = glmerControl(optimizer = "bobyqa"),nAGQ = 0)%>%
  summary()

tab%>%
  filter(reoffer_ch==F,reoffer_unch==F,reoffer_ch_twoback==T,reoffer_unch_twoback==T)%>%
  brm(stay_frc_ch_twoback ~ rw_oneback+(rw_oneback | subj), 
      data = ., 
      family = bernoulli(link = "logit"),cores=2)%>%
  summary()






