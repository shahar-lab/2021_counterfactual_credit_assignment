# Bayes modeling ----------------------------------------------------------
rm(list = ls())
library(tidylog)
library(effects)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidybayes) #for analysis of posterior draws of a Bayesian model

load('data/tab.Rdata')
tab_unch= tab%>%filter(!reoffer_ch,reoffer_unch)

tab_unch%>%group_by(rw_oneback)%>%summarise(mean(stay_frc_unch))
