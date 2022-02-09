library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(raincloudplots)

rm(list=ls())
load('./data/empirical_data_replication_2_MP/df.rdata')



model= glmer(stay_frc_ch ~ reward_oneback*prob1_oneback*prob2_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==T,reoffer_unch==F), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

model= glmer(stay_frc_unch ~ reward_oneback*prob1_oneback*prob2_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
anova(model,model0)
library(effects)
plot(effect('reward_oneback:prob1_oneback:prob2_oneback',model,xlevels=4))
library(car)
Anova(model)
library(lme4)
library(utils)
library(ggplot2)
library(tidyr)

#plot predicted values over dependent values (create a newdata frame, then predict and finally plot)
df.predict  =expand.grid(subject=seq(1,178,1),delta_exp_value_oneback=seq(-.6,.6,0.1),reward_oneback=c(0,1))
df.predict$y=predict(model,newdata=df.predict,allow.new.levels = T,type='response')
df.predict  =df.predict%>%group_by(reward_oneback,delta_exp_value_oneback)%>%summarise(y=mean(y))
ggplot(df.predict,aes(x=delta_exp_value_oneback,y=y,group=reward_oneback,colour=reward_oneback))+
  geom_smooth()+theme_bw()

df.data     =df%>%filter(reoffer_ch==F,reoffer_unch==T)%>%
  mutate(delta_exp_value_oneback=cut(delta_exp_value_oneback, breaks =3, labels(1:3),right = FALSE))%>%
  group_by(reward_oneback,delta_exp_value_oneback)%>%summarise(y=mean(stay_frc_unch))
ggplot(df.data,aes(x=delta_exp_value_oneback,y=y,group=reward_oneback,colour=reward_oneback))+
  geom_smooth()+theme_bw()


ggplot(df.data,aes(x=reward_oneback,y=y,group=delta_exp_value_oneback))+
  geom_smooth()+theme_bw()

df.predict  =df.predict%>%pivot_wider(names_from=reward_oneback,values_from=y)%>%mutate(y=`1`-`0`)
ggplot(df.predict,aes(x=delta_exp_value_oneback,y=y))+
  geom_line()+theme_bw()






model=lm(prob1~prob2,data=df)

model= glmer(reward ~ prob1*prob2+(1| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

plot(effect('prob2',model,xlevels=2))



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
  xlab("Previous-outcome") +  ylab("P(select_unchosen)") + theme_classic()

raincloud_2

#ggsave('myfolder/03_data/03 graphics/fig_unchosen.png', raincloud_2,width = 3, height = 2)








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
  xlab("Previous-outcome") +  ylab("P(select_unchosen)") + theme_classic()

raincloud_2

#ggsave('myfolder/03_data/03 graphics/fig_unchosen.png', raincloud_2,width = 3, height = 2)








model= glmer(stay_frc_ch ~ reward_oneback*delta_exp_value_oneback+(reward_oneback*delta_exp_value_oneback| subject), 
             data = df%>%filter(reoffer_ch==T,reoffer_unch==F), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

model= glmer(stay_frc_unch ~ reward_oneback*delta_exp_value_oneback+(reward_oneback*delta_exp_value_oneback| subject), 
             data = df%>%filter(reoffer_ch==F,reoffer_unch==T), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

library(effects)
plot(effect('reward_oneback:delta_exp_value_oneback',model,xlevels=3))
library(car)
Anova(model)
library(lme4)
model= lmer(Qval_unch ~ expval_unch+(expval_unch| subject), 
            data = df)






model= glmer(stay_frc_ch ~ reward_oneback*delta_exp_value_oneback*acc_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==T,reoffer_unch==F), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)