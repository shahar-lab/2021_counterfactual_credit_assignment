#Aim: estimate elpd for loo-block across the spcae of five data sets and five models which will allow model recovery estimates
rm(list=ls())

load('./data/modeling_data/mymodels.rdata')
load('./data/modeling_data/simulated_data.rdata')
load('./data/modeling_data/mystan_models_loo.rdata')
# fit stan model  --------------------------------------------
library(rstan) 
library(parallel)
library(loo)

my_log_lik=
lapply(mymodels, function(d) {
  lapply(mymodels[[3]], function(m) {
    lapply(1:4, function(mytestfold) {
    print(Sys.time())
    print(d)
    print(m)
    print(mytestfold)
  simulated_data[[d]]$testfold=mytestfold
      
  fit<- sampling(mystan_models_loo[[m]], 
                 data=simulated_data[[d]], 
                 iter=2000,
                 warmup = 1000,
                 chains=4,
                 cores =4) 
  
  extract_log_lik(fit)
  
  
    })
  })
  
  
  
})
  
  
#save(my_log_lik,file='./data/modeling_data/my_log_lik_model_recovery.rdata')
