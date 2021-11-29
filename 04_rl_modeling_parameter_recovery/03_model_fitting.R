#Aim: estimate parameters of artificial data for each model in the five models included in our model space using mcmc
rm(list=ls())

load('./data/modeling_data/mymodels.rdata')
load('./data/modeling_data/simulated_data.rdata')
load('./models/mystan_models.rdata')

# fit stan model  --------------------------------------------
library(rstan) 
library(parallel)

recovered_parameters=list()
for (m in mymodels){
  print(m)

  fit<- sampling(mystan_models[[m]], 
                 data=simulated_data[[m]], 
                 iter=2000,
                 warmup = 1000,
                 chains=8,
                 cores =8) 


  recovered_parameters[[m]] <- rstan::extract(fit, permuted = TRUE)
}

save(recovered_parameters,file='./models/recovered_parameters.rdata')