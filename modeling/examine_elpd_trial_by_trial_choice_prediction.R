#This code plot posterior predictive checks

source('./functions/my_starter.R')
#load model
load('data/empirical_data/df.rdata')
df$selected_offer                 =(df$ch==df$frcB)*1+1

load(paste0('data/model_',mymodel,'/yrep.rdata'))


library(bayesplot)
ppc_rootogram(
  df$stay_frc_unch[(df$reoffer_unch==T & df$reoffer_ch==F)]*1,
  yrep[1:4000,(df$reoffer_unch==T & df$reoffer_ch==F)],
  freq=FALSE,
  style = "suspended",
  group=df$reward_oneback[(df$reoffer_unch==T & df$reoffer_ch==F)]
)
index=(df$reoffer_unch==T & df$reoffer_ch==F)
  ppc_stat_grouped(  df$stay_frc_unch[index]*1, yrep[1:4000,index],df$reward_oneback[index],
                     freq=F,
                     xlim=c(0,1))
