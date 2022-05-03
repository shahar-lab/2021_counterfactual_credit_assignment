#aim: Hierarchical fit Stan 
#contributor: Nitzan Shahar, 2021


rm(list=ls())

# fit stan model  --------------------------------------------
library(rstan) 
load('./data/modeling_data/1PE_100subjects_4blocks_50trials_4arms_standata.Rdata')
library(parallel)
detectCores()
{
  start_time <- Sys.time()

    rl_fit<- stan(file = './models/1PE.stan', 
                data=data_for_stan, 
                iter=2000,#for full fitting use 2000iter, 1000warmup,8chains
                warmup = 1000,
                chains=8,
                cores =8) 

  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, './data/modeling_data/2PEs_100subjects_4blocks_50trials_4arms_RDSfile.rds')

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file='./data/modeling_data/2PEs_100subjects_4blocks_50trials_4arms_extracted_parameters.rdata')


