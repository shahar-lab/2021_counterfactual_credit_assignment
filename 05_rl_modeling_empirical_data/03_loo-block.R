#aim: Hierarchical fit Stan 
#contributor: Nitzan Shahar, 2021


rm(list=ls())

# lo-block-o null  --------------------------------------------
library(rstan) 
library("loo")
library(parallel)
detectCores()

load('./data/tab_standata.Rdata')

my_log_lik=list()
for (mytestfold in 1:4){
data_for_stan$testfold=mytestfold
{
  start_time <- Sys.time()

    rl_fit<- stan(file = './models/null_loo.stan', 
                data=data_for_stan, 
                pars=c('log_lik'),
                iter=2000,
                warmup = 1000,
                chains=8,
                cores =8) 

  end_time <- Sys.time()
  end_time-start_time
}

my_log_lik[[mytestfold]]=extract_log_lik(rl_fit)
save(my_log_lik, file='./data/tab_fitted_with_null_lo-block-out.rdata')
rm(rl_fit)
}
