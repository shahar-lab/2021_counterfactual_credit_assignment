rm(list = ls())
library(tidyverse)

load('data/analysis_data/tab_unch.Rdata')
load('data/analysis_data/tab_unch_two.Rdata')
load('data/analysis_data/cars_unch.Rdata')
#show basic effect
tab_unch%>%group_by(rw_oneback)%>%summarise(mean(stay_frc_unch))

#show twoback effect
tab_unch_two%>%group_by(rw_twoback)%>%summarise(mean(stay_frc_unch_two))

#show replicated effect
cars_unch%>%group_by(reward_n1back)%>%summarise(mean(stay_car_unch))
