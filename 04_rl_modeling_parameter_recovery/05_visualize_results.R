#Aim: Visualize the results of parameter recovery across the five models included in our model space
rm(list=ls())

load('./data/modeling_data/mymodels.rdata')
load('./data/modeling_data/true_parameters.rdata')
load('./data/modeling_data/recovered_parameters.rdata')
load('./data/modeling_data/pars.rdata')


corr_true_vs_recovered=list()

for (m in mymodels){
  corr_true_vs_recovered[[m]]=diag(cor(true_parameters[[m]][,-1],recovered_parameters[[m]][,-1]))
}
source('./functions/my_posteriorplot.r')
library(ggplot2)
library(bayestestR)
my_posteriorplot(pars[[3]]$population_locations[,3],'','blue',c(-5,5),0)

#model recovery--------------------
library(loo)
load('./data/modeling_data/my_log_lik_model_recovery.rdata')

loglike=my_log_lik[[1]]
loglike=
  lapply(1:4,   function(model_index){
   l=lapply(1:200, function(subject) {do.call(cbind,list(loglike[[model_index]][[1]][,subject],
                                                         loglike[[model_index]][[2]][,subject],
                                                         loglike[[model_index]][[3]][,subject],
                                                         loglike[[model_index]][[4]][,subject]))})
   do.call(cbind,l)
})

myelpd=lapply(1:4, function (model_index) {elpd(loglike[[model_index]])})

loo_compare(myelpd[[1]],myelpd[[2]],myelpd[[4]])
