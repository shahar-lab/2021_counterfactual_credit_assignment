
#This code generate artificial model parameters in an hierarchical structure

rm(list=ls())
mymodel   =read.table('./modeling/working_model.txt')
data_path =paste0('./data/model_',mymodel)
model_path=paste0('./modeling/model_',mymodel)
#--------------------------------------------------------------------------------------------------------


#parameters: learning rate chosen, noise parameter and learning rate unchosen 
Nsubjects =50 

#true population level parameters
population_locations    =c(qlogis(0.5),4,4) #population mean 
population_scales       =c(1,1.5,1.5)                  #population sd for
population_parameters   =list(mymodel,Nsubjects,population_locations,population_scales)

save(population_parameters, file=paste0(data_path,'/simulate_population_parameters.Rdata'))

#individual parameters 
alpha             = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects))
beta_rw           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects))
beta_unrw         =       (population_locations[3]+population_scales[3]*rnorm(Nsubjects))


#save
individual_parameters= 
  
  cbind(subject=seq(1,Nsubjects),
        alpha          =alpha,
        beta_rw           =beta_rw,
        beta_unrw=beta_unrw)

save(individual_parameters,file=paste0(data_path,'/simulate_individual_parameters.Rdata'))
