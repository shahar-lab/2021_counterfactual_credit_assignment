#Aim: compile all five models to stan object to relief compilation time when re-fitting
rm(list=ls())
library(rstan) 
load('./data/mymodels.rdata')

mystan_models=list()

for (m in mymodels){
  print(m)
  mystan_models[[m]]=stan_model(paste('./models/',m,'.stan',sep=""))
}

save(mystan_models,file='./models/mystan_models.rdata')