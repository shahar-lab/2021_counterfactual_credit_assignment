#This code generate artificial data based on simulated parameters

rm(list=ls())
myfolder='./model_approach_avoid/'
#--------------------------------------------------------------------------------------------------------

#load parameters
load('./data/model_approach_avoid/modelfit_based_on_empirical_data.rdata')
par_alpha=apply(plogis(pars$alpha_ch_random_effect),2,mean)
par_beta =apply(plogis(pars$beta_random_effect),2,mean)
par_omega=apply(plogis(pars$omega_random_effect),2,mean)

#set sample size
Nsubjects =length(par_alpha)
parameters=
cbind(subject=seq(1,Nsubjects),
           alpha_chosen   =par_alpha,
           beta           =par_beta,
           omega          =par_omega)


#set task variables 
cfg = list(Nblocks         =20,
           Ntrials_perblock=50,
           Narms           =4,  #number of arms in the task 
           Nraffle         =2,  #number of arms offered for selection each trial
           rndwlk          =read.csv('./functions/rndwlk.csv',header=F))

#run simulation
source('./modeling/model_approach_avoid/approach_avoid_model.r')

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=parameters[subject,],cfg=cfg))
}

#save
save(df,file='data/model_approach_avoid/simulated_data_based_on_empirical_parameters.Rdata')


