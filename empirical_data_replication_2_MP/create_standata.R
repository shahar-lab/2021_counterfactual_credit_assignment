#Aim: Generate data in a format that is good for stan (i.e., subject x trials matricies with padding for missing values)
#Contributor: Nitzan Shahar, 2021



#empirical data-------------------------------------------------
rm(list=ls())
load('./data/empirical_data_replication_2_MP/df.rdata')
colnames(df)
library(dplyr)

df$choice        =df$ch+1
df$unchosen      =df$unch+1
df$offer1        =df$frcA+1
df$offer2        =df$frcB+1
df$selected_offer=(df$choice==df$offer2)*1+1
df$reward        = df$rw
df$fold          = df$blk
df=df%>%mutate(first_trial_in_block=(blk!=lag(blk,default = 0))*1)%>%as.data.frame()

#make standata
source('./functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=df, 
                               subject_column     =df$subj,
                               block_column       =df$blk,
                               var_toinclude      =c(
                                 'fold',
                                 'first_trial_in_block',
                                 'trial',
                                 'offer1',
                                 'offer2',
                                 'choice',
                                 'unchosen',
                                 'reward',
                                 'selected_offer'),
                               additional_arguments=list(Narms=4, Nraffle=2))

save(data_for_stan,file='./data/empirical_data_replication_2_MP/empirical_standata.rdata')

