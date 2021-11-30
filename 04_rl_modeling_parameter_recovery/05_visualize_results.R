#Aim: Visualize the results of parameter recovery across the five models included in our model space
rm(list=ls())

load('./data/modeling_data/mymodels.rdata')
load('./data/modeling_data/true_parameters.rdata')
load('./data/modeling_data/recovered_parameters.rdata')


corr_true_vs_recovered=list()

for (m in mymodels){
  corr_true_vs_recovered[[m]]=diag(cor(true_parameters[[m]][,-1],recovered_parameters[[m]][,-1]))
}
