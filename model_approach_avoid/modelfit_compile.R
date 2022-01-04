#aim: compile and save stan model (so you wont have to redo this everytime you re-run the model with different parameters)
rm(list=ls())
myfolder='./model_approach_avoid/'
#--------------------------------------------------------------------------------------------------------

library(rstan) 

# compile stan model----------------------------------------------

my_compiledmodel=stan_model(paste0(myfolder,'files/model.stan'))
save(my_compiledmodel,file=paste0(myfolder,'data/modelfit_compile.rdata'))

my_compiledmodel=stan_model(paste0(myfolder,'files/model_loo.stan'))
save(my_compiledmodel,file=paste0(myfolder,'data/modelfit_compile_loo.rdata'))
