#aim: Hierarchical fit Stan with loo cv

rm(list=ls())

myfolder='./model_double_updating/'
#--------------------------------------------------------------------------------------------------------



library(rstan) 
library(loo)
library(parallel)
detectCores()

#load data
load('./empirical data/data/empirical_data_standata.rdata')
load(paste0(myfolder,'data/modelfit_compile_loo.rdata'))

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



save(like, file=paste0(myfolder,'data/modelfit_like_per_trial_and_chain.rdata'))

# save mean predicted probability per trial (across samples)
like   =t(sapply(1:dim(like)[1], function(i){x=c(t(like[i,,]))
                                             x[x==0]<-NA
                                             x=na.omit(x)}))
like=apply(like,2,mean)

save(like, file=paste0(myfolder,'data/modelfit_like_per_trial.rdata'))





