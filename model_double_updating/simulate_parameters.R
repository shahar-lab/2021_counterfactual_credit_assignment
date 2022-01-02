#This code generate artificial model parameters in an hierarchical structure

rm(list=ls())

# set sample size
Nsubjects =200 

#parameters: learning rate chosen, noise parameter and learning rate unchosen 

#true population level parameters
population_locations    =c(qlogis(0.5),4,qlogis(0.5)) #population mean 
population_scales       =c(1,1.5,1)                  #population sd for

#individual parameters 
alpha_chosen   = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects))
beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects))
alpha_unchosen = plogis(population_locations[3]+population_scales[3]*rnorm(Nsubjects))

#save
true.parameters= 
  
  cbind(subject=seq(1,Nsubjects),
        alpha_chosen   =alpha_chosen,
        beta           =beta,
        alpha_unchosen =alpha_unchosen)

save(true.parameters,file='./model_double_updating/true_parameters.Rdata')
