#Aim: compile all five models to stan object to relief compilation time when re-fitting
rm(list=ls())
library(rstan) 
load('./data/modeling_data/mymodels.rdata')


#compile models with out generated quantities 
mystan_models    =list()
mystan_models_loo=list()

for (m in mymodels){
  print(m)
  mystan_models[[m]]    =stan_model(paste('./models/',m,'.stan',sep=""))
  #mystan_models_loo[[m]]=stan_model(paste('./models/',m,'_loo.stan',sep=""))
  
}

save(mystan_models,file='./data/modeling_data/mystan_models.rdata')
save(mystan_models_loo,file='./data/modeling_data/mystan_models_loo.rdata')
