#aim: Hierarchical fit Stan 


rm(list=ls())
library(rstan) 
detectCores()

# fit stan model--------------------------------------------------

#load data
load('./data/null_standata.Rdata')
load('./data/my_compiledmodel.rdata')

{
  start_time <- Sys.time()

    rl_fit<- sampling(my_compiledmodel, 
                data=data_for_stan, 
                iter=100,
                warmup = 50,
                chains=4,
                cores =4) 

  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, paste('./data/',model_name,'_RDSfile.rds',sep=""))

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file='./data/recovered_parameters.rdata')


