#Aim: generate artificial data for each model in the five models included in our model space
rm(list=ls())

load('./data/modeling_data/mymodels.rdata')
load('./data/modeling_data/true_parameters.rdata')
source('./functions/make_mystandata.R')

####task variables-----------------------------------------------
Nsubjects =200
cfg = list(Nblocks         =4,
           Ntrials_perblock=50,
           Narms           =4,    
           Nraffle         =2,  
           rndwlk          =read.csv('./data/modeling_data/rndwlk.csv',header=F)[,1:50])




####run simulation across five models for all artificial subjects------------------------
simulated_data=list()
for (m in mymodels[[3]]){
  print(m)

  #source model
  source(paste('./models/',m,'.R',sep=""))  
  
  #simulate data
  df=data.frame()
  for (subject in 1:Nsubjects) {
    df=rbind(df, sim.block(subject=subject, parameters=true_parameters[[m]][subject,],cfg=cfg))
  }
  
  df$fold = df$block #the var "block" is reserved in stan, and since we are going to do block loo, fold=block
  
  #convert df to a standata object
  df         <-make_mystandata(data=df, 
                                 subject_column     =df$subject,
                                 block_column       =df$block,
                                 var_toinclude      =c(
                                   'first_trial_in_block',
                                   'fold',
                                   'trial',
                                   'offer1',
                                   'offer2',
                                   'choice',
                                   'unchosen',
                                   'reward',
                                   'selected_offer'),
                                 additional_arguments=list(Narms=4, Nraffle=2))
  
  simulated_data[[m]]=df
}


####check & save ------------------------------
save(simulated_data,file='./data/modeling_data/simulated_data.rdata')

