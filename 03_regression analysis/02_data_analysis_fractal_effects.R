rm(list=ls())
library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(raincloudplots)
load('data/analysis_data/tab.Rdata')


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






