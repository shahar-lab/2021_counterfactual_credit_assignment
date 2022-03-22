#Aim: Generate data in a format that is good for stan (i.e., subject x trials matricies with padding for missing values)
#Contributor: Nitzan Shahar, 2021



#empirical data-------------------------------------------------
rm(list=ls())
df=read.csv('./data/empirical_data_replication_2_MP/df.csv')
colnames(df)
library(dplyr)

df$choice                         =df$choice_student
df$unchosen                       =df$offer1
df$unchosen[df$choice==df$offer1] =df$offer2[df$choice==df$offer1]
df$selected_offer                 =(df$choice==df$offer2)*1+1
df$fold                           = df$running_block
df=df%>%mutate(first_trial_in_block=(running_block!=lag(running_block,default = 0))*1)%>%as.data.frame()

#make standata
source('./functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=df, 
                               subject_column     =df$subject,
                               block_column       =df$running_block,
                               var_toinclude      =c(
                                 'fold',
                                 'first_trial_in_block',
                                 'offer1',
                                 'offer2',
                                 'choice',
                                 'unchosen',
                                 'reward',
                                 'selected_offer'),
                               additional_arguments=list(Narms=4, Nraffle=2))

save(data_for_stan,file='./data/empirical_data_replication_2_MP/empirical_standata.rdata')

