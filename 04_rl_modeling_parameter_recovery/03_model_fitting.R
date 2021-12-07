#Aim: estimate parameters of artificial data for each model in the five models included in our model space using mcmc
rm(list=ls())

load('./data/modeling_data/mymodels.rdata')
load('./data/modeling_data/simulated_data.rdata')
load('./data/modeling_data/mystan_models.rdata')

# fit stan model  --------------------------------------------
library(rstan) 
library(parallel)

pars=list()
for (m in mymodels[[3]]){
  print(m)

  fit<- sampling(mystan_models[[m]], 
                 data=simulated_data[[m]], 
                 iter=2000,
                 warmup = 1000,
                 chains=8,
                 cores =8) 


  pars[[m]] <- rstan::extract(fit, permuted = TRUE)
}

save(pars,file='./data/modeling_data/pars.rdata')



# save recovered parameters----------------------------------
#here we average across mcmc samples to have a single estimate for each individual, parameter and model
Nsubjects                 =200 
recovered_parameters      =list()
recovered_parameters[[mymodels[[1]]]]= cbind(subject=seq(1,Nsubjects),
                                             alpha  =apply(pars[[mymodels[[1]]]]$alpha, 2, mean),
                                             beta   =apply(pars[[mymodels[[1]]]]$beta, 2, mean))

recovered_parameters[[mymodels[[2]]]]= cbind(subject=seq(1,Nsubjects),
                                             alpha  =apply(pars[[mymodels[[2]]]]$alpha, 2, mean),
                                             beta   =apply(pars[[mymodels[[2]]]]$beta, 2, mean),
                                             w      =apply(pars[[mymodels[[2]]]]$w, 2, mean))

recovered_parameters[[mymodels[[3]]]]= cbind(subject=seq(1,Nsubjects),
                                             alpha_chosen   =apply(pars[[mymodels[[3]]]]$alpha_ch, 2, mean),
                                             beta           =apply(pars[[mymodels[[3]]]]$beta, 2, mean),
                                             lambda         =apply(pars[[mymodels[[3]]]]$lambda, 2, mean))

recovered_parameters[[mymodels[[4]]]]= cbind(subject=seq(1,Nsubjects),
                                             alpha_chosen   =apply(pars[[mymodels[[4]]]]$alpha_ch, 2, mean),
                                             beta           =apply(pars[[mymodels[[4]]]]$beta, 2, mean),
                                             alpha_unchosen =apply(pars[[mymodels[[4]]]]$alpha_unch, 2, mean))



save(recovered_parameters,file='./data/modeling_data/recovered_parameters.rdata')
