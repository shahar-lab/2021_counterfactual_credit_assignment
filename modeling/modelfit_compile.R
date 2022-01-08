#aim: compile and save stan model (so you wont have to redo this everytime you re-run the model with different parameters)
source('./functions/my_starter.R')


#--------------------------------------------------------------------------------------------------------

# compile stan model----------------------------------------------

my_compiledmodel=stan_model(paste0(model_path,'model.stan'))
save(my_compiledmodel, file=paste0(data_path,'/modelfit_compile.rdata'))

my_compiledmodel=stan_model(paste0(model_path,'model_loo.stan'))
save(my_compiledmodel,file=paste0(data_path,'/modelfit_compile_loo.rdata'))
