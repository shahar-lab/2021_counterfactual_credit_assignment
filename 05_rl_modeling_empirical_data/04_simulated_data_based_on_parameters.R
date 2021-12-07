
rm(list=ls())

load('./data/tab_fitted_with_2PEs_extracted_parameters.rdata')
#copy parameters ----------------------------------------------------------
parameters=cbind(
  #subject=seq(1,Nsubjects),
  alpha_ch  =apply(pars$alpha_ch, 2, mean),
  alpha_unch=apply(pars$alpha_unch, 2, mean),
  beta      =apply(pars$beta, 2, mean)
)
psych::multi.hist(parameters,density=F,nrow=1)

# generate data -----------------------------------------------------------
model_name=c('2PEs')

Nsubjects           =178        
Nblocks             =1
Ntrials_perblock    =50
Ntrials             =Nblocks*Ntrials_perblock
Narms               =4
rndwlk              =matrix(0.5,4,Ntrials_perblock)#read.csv('./data/rndwlk_4frc_1000trials.csv',header=F)[,1:Ntrials_perblock]
Nraffle             =2

cfg = list(Nblocks         =Nblocks,
           Ntrials_perblock=Ntrials_perblock,
           Narms           =Narms,    
           Nraffle         =Nraffle,  #(i.e., offer Nraffle arms each trial from a deck of Narms)
           rndwlk          =rndwlk)

source('./models/simulation_Narmed_bandit_task_twoPEs.R')

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=parameters[subject,],cfg=cfg))
}
save(df,file=paste('./data/tab_simdata_based_on_2PEs_extracted_parameters_allbanditaat50%.Rdata',sep=""))
