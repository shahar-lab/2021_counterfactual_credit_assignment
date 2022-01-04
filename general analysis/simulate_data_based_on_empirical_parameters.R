#This code generate artificial data based on simulated parameters

rm(list=ls())
myfolder='./model_double_updating/'
#--------------------------------------------------------------------------------------------------------

#load parameters
load(paste0(myfolder,'data/modelfit_estimated_parameters_based_on_empirical_data.rdata'))

#set sample size
Nsubjects =dim(pars$alpha_ch)[2] 
parameters=
cbind(subject=seq(1,Nsubjects),
           alpha_chosen   =apply(pars$alpha_ch,2,mean),
           beta           =apply(pars$beta,2,mean),
           alpha_unchosen =apply(pars$alpha_unch,2,mean))


#set task variables 
cfg = list(Nblocks         =4,
           Ntrials_perblock=50,
           Narms           =4,  #number of arms in the task 
           Nraffle         =2,  #number of arms offered for selection each trial
           rndwlk          =read.csv('./functions/rndwlk.csv',header=F))

#run simulation
source(paste0(myfolder,'files/model.R'))

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=parameters[subject,],cfg=cfg))
}

#save
save(df,file=paste0(myfolder,'data/simulate_data_based_on_empirical_parameters.Rdata'))


