# This code examine posterior and prior predictive checks for brms model
# that examine the effect of previous reward on the probability of taking the unchosen option


rm(list=ls())
source('./functions/my_packages.r')

# load data 
load('./data/empirical_data/df.rdata')
df$reward_oneback=factor(df$reward_oneback,labels=c('unrewarded','rewarded'))
df<-na.omit(df)

#set empirical y estimates
y   =df%>%filter(reoffer_ch==F,reoffer_unch==T)%>%mutate(stay_frc_unch=stay_frc_unch*1)%>%select(stay_frc_unch)%>%as.vector
y   =as.numeric(y$stay_frc_unch)

#set reward effect vector
group_vec=df%>%filter(reoffer_ch==F,reoffer_unch==T)%>%select(reward_oneback)

#-----------------------------------------------------------------------------------
#weakly informative 
#load model
load('./data/empirical_data/brms_unchosen_weakly_informative_priors.rdata')

#posterior_predictive_check
yrep=posterior_predict(model)
ppc_stat_grouped(y, yrep, group_vec$reward_oneback)

#-----------------------------------------------------------------------------------
#prior only
#load model
load('./data/empirical_data/brms_unchosen_weakly_informative_priors_only_priors.rdata')

#predictive_check
yrep=posterior_predict(model)
ppc_stat_grouped(y, yrep, group_vec$reward_oneback)

install.packages("devtools")
devtools::install_github("shahar-lab/RLR")



