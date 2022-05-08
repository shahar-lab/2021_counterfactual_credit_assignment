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

    for (observation in 1:dim(like)[2]){
      
      selected_offer_rep[sample,observation]=sample(0:1,1,prob=c(1-action_prob[sample,observation],action_prob[sample,observation]))
      
   }
}

yrep=selected_offer_rep
save(yrep,file=paste0('data/model_',mymodel,'/yrep_selected_offer.rdata'))
