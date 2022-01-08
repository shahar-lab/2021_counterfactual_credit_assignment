#clear workspace
rm(list=ls())

#load project packages
library(rstan) 
library(loo)
library(parallel)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(effects)

#load path string for the model you are working on
#note that in order no to create code duplicates you can
#change the model name in 'working model.txt'
mymodel   =read.table('./modeling/working_model.txt')
data_path =paste0('./data/model_',mymodel)
model_path=paste0('./modeling/model_',mymodel,'/',mymodel,'_')
print(paste0(mymodel,' is the current working model'))
print(paste0('data  folder: ',data_path))
print(paste0('model folder: ',model_path))
#source user-made functions
source('./functions/my_posteriorplot.R')
source('./functions/my_xyplot.R')


