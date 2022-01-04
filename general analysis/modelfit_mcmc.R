#aim: Hierarchical fit Stan 
rm(list=ls())
myfolder='./model_double_updating/'
#--------------------------------------------------------------------------------------------------------

library(rstan) 
detectCores()

# fit stan model--------------------------------------------------

#load data
load('./empirical data/data/empirical_data_standata.rdata')
load(paste0(myfolder,'data/modelfit_compile.rdata'))

{
  start_time <- Sys.time()

    rl_fit<- sampling(my_compiledmodel, 
                data=data_for_stan, 
                iter=2000,
                warmup = 1000,
                chains=4,
                cores =4) 

  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, paste0(myfolder,'data/modelfit_based_on_empirical_data.rds'))

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file=paste0(myfolder,'data/modelfit_based_on_empirical_data.rdata'))



