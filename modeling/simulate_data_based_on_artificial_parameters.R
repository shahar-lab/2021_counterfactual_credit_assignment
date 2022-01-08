#This code generate artificial data based on simulated parameters

rm(list=ls())
mymodel   =read.table('./modeling/working_model.txt')
data_path =paste0('./data/model_',mymodel)
model_path=paste0('./modeling/model_',mymodel,'/',mymodel,'_')
#--------------------------------------------------------------------------------------------------------

#load parameters
load(paste0(data_path,'/simulate_individual_parameters.Rdata'))


#set sample size
Nsubjects =dim(individual_parameters)[1] 

#set task variables 
cfg = list(Nblocks         =4,
           Ntrials_perblock=50,
           Narms           =4,  #number of arms in the task 
           Nraffle         =2,  #number of arms offered for selection each trial
           rndwlk          =read.csv('./functions/rndwlk.csv',header=F))

#run simulation
source(paste0(model_path,'model.r'))

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=individual_parameters[subject,],cfg=cfg))
}

#save
save(df,file=paste0(data_path,'/simulate_data_based_on_artificial_parameters.Rdata'))


###convert to a standata format ###----------------------------------------------------------------------------------
  
source('./functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=df, 
                               subject_column     =df$subject,
                               block_column       =df$block,
                               var_toinclude      =c(
                                 'first_trial_in_block',
                                 'trial',
                                 'offer1',
                                 'offer2',
                                 'choice',
                                 'unchosen',
                                 'reward',
                                 'selected_offer'),
                               additional_arguments=list(Narms=4, Nraffle=2))

save(data_for_stan,file=paste0(data_path,'/simulate_standata_based_on_artificial_parameters.Rdata'))


