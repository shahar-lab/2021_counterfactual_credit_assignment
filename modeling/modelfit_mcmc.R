#aim: Hierarchical fit Stan 
rm(list=ls())
mymodel   =read.table('./modeling/working_model.txt')
data_path =paste0('./data/model_',mymodel)
model_path=paste0('./modeling/model_',mymodel,'/',mymodel,'_')
#--------------------------------------------------------------------------------------------------------

#load data
load('./data/empirical data/empirical_data_standata.rdata')
#load(paste0(data_path,'/simulate_standata_based_on_artificial_parameters.rdata'))
load(paste0(data_path,'/modelfit_compile.rdata'))

{
  start_time <- Sys.time()

    rl_fit<- sampling(my_compiledmodel, 
                data=data_for_stan, 
                iter=1000,
                warmup = 500,
                chains=2,
                cores =2) 

  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, paste0(data_path,'/modelfit_based_on_empirical_data.rds'))

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file=paste0(data_path,'/modelfit_based_on_artificial_data.rdata'))



