#aim: Hierarchical fit Stan 
#contributor: Nitzan Shahar, 2021


rm(list=ls())
load('./data/modeling_data/mymodels.rdata')
load('./data/modeling_data/mystan_models_loo.rdata')

# lo-block-o null  --------------------------------------------
library(rstan) 
library("loo")
library(parallel)
detectCores()

load('./data/modeling_data/tab_standata.Rdata')

my_log_lik=
lapply(mymodels, function(m) {
  lapply(1:4, function(mytestfold) {
    print(Sys.time())
    print(m)
    print(mytestfold)
    data_for_stan$testfold=mytestfold

    fit<- sampling(mystan_models_loo[[m]], 
                   data=data_for_stan, 
                   pars=c('log_lik'),
                   iter=2000,
                   warmup = 1000,
                   chains=8,
                   cores =8) 
    
    extract_log_lik(fit)
})
})

save(my_log_lik, file='./data/modeling_data/loo-block.rdata')

