#This code plot recovered parameters against the true parameters

source('./functions/my_starter.R')

#--------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(bayestestR)

#population level parameters
load(paste0(data_path,'/simulate_population_parameters.Rdata'))
load(paste0(data_path,'/modelfit_based_on_artificial_data.rdata'))

my_posteriorplot(x       = plogis(pars$population_locations[,1]),
                     myxlim  = c(0,1),
                     my_vline= plogis(population_parameters[[3]][1]), 
                     myxlab  = expression(alpha['location']),
                     mycolor = "pink")


my_posteriorplot(x       = pars$population_locations[,2],
                     myxlim  = c(0.5,5),
                     my_vline= population_parameters[[3]][2], 
                     myxlab  = expression(beta['location']),
                     mycolor = "pink")


#-------------------------------------------------------------------------------------------------------------
# individual level parameters
load(paste0(data_path,'/simulate_individual_parameters.Rdata'))

my_xyplot(individual_parameters[,'alpha_chosen'],apply(pars$alpha_ch, 2, mean),'true','recovered','navy')
my_xyplot(individual_parameters[,'beta'],        apply(pars$beta, 2, mean),'true','recovered','navy')

