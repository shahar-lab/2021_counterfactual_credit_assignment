#aim: Hierarchical fit Stan 
#contributor: Nitzan Shahar, 2021


rm(list=ls())

# fit stan model  --------------------------------------------
library(rstan) 
load('./data/tab_standata.Rdata')
library(parallel)
detectCores()
{
  start_time <- Sys.time()

    rl_fit<- stan(file = './models/null.stan', 
                data=data_for_stan, 
                iter=2000,
                warmup = 1000,
                chains=8,
                cores =8) 

  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, './data/tab_fitted_with_null_RDSfile.rds')

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file='./data/tab_fitted_with_null_extracted_parameters.rdata')

library(bridgesampling)
bs = bridge_sampler(rl_fit)
save(bs, file=paste('data/tab_fitted_with_null_bridge_sampler.rdata',sep=""))

-----------------------------------------------------------------------------------------
#2PEs
rm(list=ls())
library(rstan) 
load('./data/tab_standata.Rdata')
library(parallel)
detectCores()
{
  start_time <- Sys.time()
  
  rl_fit<- stan(file = './models/2PEs.stan', 
                data=data_for_stan, 
                iter=2000,
                warmup = 1000,
                chains=8,
                cores =8) 
  
  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, './data/tab_fitted_with_2PEs_RDSfile.rds')

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file='./data/tab_fitted_with_2PEs_extracted_parameters.rdata')

library(bridgesampling)
bs = bridge_sampler(rl_fit)
save(bs, file=paste('data/tab_fitted_with_2PEs_bridge_sampler.rdata',sep=""))


-----------------------------------------------------------------------------------------
#1PE
rm(list=ls())
library(rstan) 
load('./data/tab_standata.Rdata')
library(parallel)
detectCores()
{
  start_time <- Sys.time()
  
  rl_fit<- stan(file = './models/1PE.stan', 
                data=data_for_stan, 
                iter=2000,
                warmup = 1000,
                chains=8,
                cores =8) 
  
  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, './data/tab_fitted_with_1PE_RDSfile.rds')

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file='./data/tab_fitted_with_1PE_extracted_parameters.rdata')

library(bridgesampling)
bs = bridge_sampler(rl_fit)
save(bs, file=paste('data/tab_fitted_with_1PE_bridge_sampler.rdata',sep=""))

