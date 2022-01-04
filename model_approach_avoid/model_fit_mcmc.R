#aim: Hierarchical fit Stan 
rm(list=ls())
myfolder='./model_approach_avoid/'
#--------------------------------------------------------------------------------------------------------

library(rstan) 
detectCores()

# fit stan model--------------------------------------------------

#load data
load('./empirical data/empirical_standata.rdata')
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






### fit loo----------------------------------------------------------------
rm(list=ls())
load('./model_approach_avoid/data/mypath.txt')

library(rstan) 
library(loo)
library(parallel)
detectCores()

load('./data/empirical_data-standata.rdata')
load(paste0(mypath,'modelfit_compile_loo.rdata'))

like=
    lapply(1:4, function(mytestfold) {
      print(Sys.time())
      print(mytestfold)
      data_for_stan$testfold=mytestfold
      
      fit<- sampling(my_compiledmodel, 
                     data=data_for_stan, 
                     pars=c('log_lik'),
                     iter=2000,
                     warmup = 1000,
                     chains=4,
                     cores =4) 
      pars=extract(fit)
      pars$log_lik
    })


like=like[[1]]+like[[2]]+like[[3]]+like[[4]]
like_temp=list()



save(like, file='./data/model_approach_avoid-loo_block.rdata')

# save mean predicted probability per trial (across samples)
load('./model_approach_avoid/data/modelfit_loo-block.rdata')
like   =t(sapply(1:dim(like)[1], function(i){x=c(t(like[i,,c(10:49,60:99,110:149,160:196)]))
                                          x[x==0]<-NA
                                          x=na.omit(x)}))
like=apply(like,2,mean)

save(like, file='./model_approach_avoid/data/modelfit_like_per_trial.rdata')


