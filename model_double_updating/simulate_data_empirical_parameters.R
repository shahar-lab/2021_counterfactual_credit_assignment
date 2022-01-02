#This code generate artificial data based on simulated parameters

rm(list=ls())



#load parameters
load('./model_double_updating/estimated_parameters_empirical_data.Rdata')

#set sample size
Nsubjects =dim(pars$alpha_ch)[2] 
parameters=
data.frame(subject=seq(1,Nsubjects),
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
source('./model_double_updating/model.R')

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=parameters[subject,],cfg=cfg))
}

#save
save(df,file='./model_double_updating/simdata_using_empirical_parameters.Rdata')


