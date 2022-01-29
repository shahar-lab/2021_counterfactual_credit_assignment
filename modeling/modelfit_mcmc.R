#aim: Hierarchical fit Stan 
rm(list=ls())
source('./functions/my_starter.R')

#--------------------------------------------------------------------------------------------------------

#load data
load('./data/empirical_data/standata.rdata')
#load(paste0(data_path,'/simulate_standata_based_on_artificial_parameters.rdata'))
load(paste0(data_path,'/modelfit_compile.rdata'))

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
saveRDS(rl_fit, paste0(data_path,'/modelfit_based_on_empirical_data.rds'))

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file=paste0(data_path,'/modelfit_based_on_empirical_data.rdata'))



