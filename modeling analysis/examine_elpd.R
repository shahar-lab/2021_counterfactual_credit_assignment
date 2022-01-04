#This code plot recovered parameters against the true parameters

rm(list=ls())
myfolder='./model_double_updating/'
#--------------------------------------------------------------------------------------------------------

load(paste0(myfolder,'data/modelfit_like_per_trial.rdata'))
