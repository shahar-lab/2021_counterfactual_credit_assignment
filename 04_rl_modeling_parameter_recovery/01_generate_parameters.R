#Aim: generate artificial parameters fro each of the five models included in our model space investigating the update mechanism  for an unchosen option 
rm(list=ls())

####preparation------------------------------

#create list with the names of all five models (to be used for saving models and parameters)
mymodels=list('model1_null',
              'model2_single_prediction_error',
              'model3_inverted outcome',
              'model4_two_prediction_errors',
              'model5_fixed_discount')


#general sample variables and pre-allocation
Nsubjects           =200        
true_parameters     =list()




####model1_null------------------------------
#parameters: learning rate and noise parameters

#true population level parameters 
population_locations    =c(qlogis(0.5),4) #population mean  
population_scales       =c(1,1.5)        #population sd 


#individual parameters 
alpha          = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects))
beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects))

#save
true_parameters[[mymodels[[1]]]]= 
  
  cbind(subject=seq(1,Nsubjects),
        alpha  =alpha,
        beta   =beta)




####model2_single_prediction_error------------------------------
#parameters:  learning rate, noise parameter and w 

#true population level parameters
population_locations    =c(qlogis(0.5),4,qlogis(0.75)) #population mean 
population_scales       =c(1,1.5,0.5)                  #population sd 

#individual parameters 
alpha          = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects))
beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects))
w              = plogis(population_locations[3]+population_scales[3]*rnorm(Nsubjects))

#save
true_parameters[[mymodels[[2]]]]= 
  
  cbind(subject=seq(1,Nsubjects),
        alpha  =alpha,
        beta   =beta,
        w      =w)


####model3_inverted outcome------------------------------
#parameters: learning rate chosen, noise parameter and learning rate unchosen 

#true population level parameters
population_locations    =c(qlogis(0.5),4,qlogis(0.5)) #population mean 
population_scales       =c(1,1.5,1)                  #population sd

#individual parameters 
alpha_chosen   = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects))
beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects))
alpha_unchosen = plogis(population_locations[3]+population_scales[3]*rnorm(Nsubjects))

#save
true_parameters[[mymodels[[3]]]]= 
  
  cbind(subject=seq(1,Nsubjects),
        alpha_chosen   =alpha_chosen,
        beta           =beta,
        alpha_unchosen =alpha_unchosen)


####model4_two_prediction_errors------------------------------
#parameters: learning rate chosen, noise parameter and learning rate unchosen 

#true population level parameters
population_locations    =c(qlogis(0.5),4,qlogis(0.5)) #population mean 
population_scales       =c(1,1.5,1)                  #population sd for

#individual parameters 
alpha_chosen   = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects))
beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects))
alpha_unchosen = plogis(population_locations[3]+population_scales[3]*rnorm(Nsubjects))

#save
true_parameters[[mymodels[[4]]]]= 
  
  cbind(subject=seq(1,Nsubjects),
        alpha_chosen   =alpha_chosen,
        beta           =beta,
        alpha_unchosen =alpha_unchosen)



####model5_fixed_discount------------------------------
#parameters: learning rate, noise, intercept and slope for the discounting of the unchosen option as a function of outcome

#true population level parameters
population_locations    =c(qlogis(0.5),4,0,0) #population mean 
population_scales       =c(1,1.5,1,1)             #population sd 

#individual parameters 
alpha          = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects))
beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects)) 
intercept      =       (population_locations[3]+population_scales[3]*rnorm(Nsubjects)) 
slope          =       (population_locations[4]+population_scales[4]*rnorm(Nsubjects)) 

#save
true_parameters[[mymodels[[5]]]]= 
  
  cbind(subject=seq(1,Nsubjects),
        alpha_chosen   =alpha_chosen,
        beta           =beta,
        intercept      =intercept,
        slope          =slope)


####check & save ------------------------------


save(mymodels,file='./data/modeling_data/mymodels.rdata')
save(true_parameters,file='./data/modeling_data/true_parameters.rdata')

