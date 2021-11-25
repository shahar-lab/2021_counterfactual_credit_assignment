#Aim: Generate data in a format that is good for stan (i.e., subject x trials matricies with padding for missing values)
#Contributor: Nitzan Shahar, 2021



#empirical data-------------------------------------------------
rm(list=ls())
load('./data/tab.Rdata')
colnames(tab)
library(dplyr)

tab$action        =tab$ch+1
tab$unchosen      =tab$unch+1
tab$offer1        =tab$frcA+1
tab$offer2        =tab$frcB+1
tab$selected_offer=(tab$action==tab$offer2)*1+1
tab$reward        = tab$rw
tab$fold          = tab$blk
tab=tab%>%mutate(first_trial_in_block=(blk!=lag(blk,default = 0))*1)%>%as.data.frame()

#make standata
source('./functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=tab, 
                               subject_column     =tab$subj,
                               block_column       =tab$blk,
                               var_toinclude      =c(
                                 'fold',
                                 'first_trial_in_block',
                                 'trial',
                                 'offer1',
                                 'offer2',
                                 'action',
                                 'unchosen',
                                 'reward',
                                 'selected_offer'),
                               additional_arguments=list(Narms=4, Nraffle=2))

save(data_for_stan,file='data/tab_standata.rdata')






