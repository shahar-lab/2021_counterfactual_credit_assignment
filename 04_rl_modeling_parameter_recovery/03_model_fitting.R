#Aim: estimate parameters of artificial data for each model in the five models included in our model space using mcmc
rm(list=ls())

load('./data/mymodels.rdata')
load('./data/simulated_data.rdata')


# fit stan model  --------------------------------------------
library(rstan) 
library(parallel)

recovered_parameters=list()
for (m in mymodels){
  print(m)

  fit<- sampling(mystan_models[[m]], 
                 data=simulated_data[[m]], 
                 iter=20,
                 warmup = 2,
                 chains=1,
                 cores =1) 


  recovered_parameters[[m]] <- rstan::extract(fit, permuted = TRUE)
}

save(recovered_parameters,file='./models/recovered_parameters.rdata')