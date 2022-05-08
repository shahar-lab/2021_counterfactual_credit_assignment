#This code generate yrep for posterior predictive checks

rm(list=ls())
# samples x observations
source('./functions/my_starter.R')

#--------------------------------------------------------------------------------------------------------
#load model
load('data/empirical_data/df.rdata')
df$selected_offer                 =(df$ch==df$frcB)*1+1
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))

#convert to choice prediction
action_prob=exp(like)
yrep=selected_offer_rep=like*0

for (sample in 1:4000){
  print(sample)
  previous_action=1
  
    for (observation in 1:dim(like)[2]){
      
    if (df$selected_offer[observation]==2){
      selected_offer_rep[sample,observation]=sample(c(df$frcA[observation],df$frcB[observation]),1,prob=c(1-action_prob[sample,observation],action_prob[sample,observation]))
      
    }
    if (df$selected_offer[observation]==1){
      selected_offer_rep[sample,observation]=sample(c(df$frcB[observation],df$frcA[observation]),1,prob=c(1-action_prob[sample,observation],action_prob[sample,observation]))
    }
      
      yrep[sample,observation]=(selected_offer_rep[sample,observation]==previous_action)*1
      previous_action=df$unchosen[observation]
}
}


save(yrep,file=paste0('data/model_',mymodel,'/yrep.rdata'))
