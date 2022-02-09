#this codes takes YT data set and converts to standata format for modeling

rm(list=ls())


load('./data/empirical_data_replication_1_YT/data_afterpreprocessing.rdata')

#correct variables names and levels to fit what we alrady have in the previous data set
df=task
df$choice=as.numeric(as.factor(df$car_chosen))
df$unchosen=as.numeric(as.factor(df$unchosen_car))
df$offer1=as.numeric(as.factor(df$left))
df$offer2=as.numeric(as.factor(df$right))
df$selected_offer=(df$choice==df$offer2)*1+1
df$ID=as.factor(df$ID)
df$first_trial_in_block=(df$ID!=data.table::shift(df$ID))*1
df$first_trial_in_block[1]=1

df$subject=1
subj=1
for (i in (2:length(df$subject))){
  if(df$first_trial_in_block[i]==0){
     df$subject[i]=subj
    }
  
  if(df$first_trial_in_block[i]==1){
    print('2')
    subj=subj+1
    df$subject[i]=subj
  }

}
df$block=df$subject

save(df,file='./data/empirical_data_replication_1_YT/df.rdata')

#make stan data

source('./functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=df, 
                               subject_column     =df$subject,
                               block_column       =df$block,
                               var_toinclude      =c(
                                 'first_trial_in_block',
                                 'trial',
                                 'offer1',
                                 'offer2',
                                 'choice',
                                 'unchosen',
                                 'reward',
                                 'selected_offer'),
                               additional_arguments=list(Narms=4, Nraffle=2))

save(data_for_stan,file='./data/empirical_data_replication_1_YT/standata.Rdata')


