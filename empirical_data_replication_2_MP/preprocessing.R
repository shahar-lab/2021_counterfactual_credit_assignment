rm(list=ls())
library(data.table)
library(tidyverse)
df=read.csv('./empirical_data_replication/teacher_student_sessions1_2.csv')


df=df%>%mutate(delta_exp_value        =(prob1-prob2),
               offer1=frcA,
               offer2=frcB,
               choice=ch,
               unchosen=ifelse(ch==frcA,frcB,frcA),
               reward=rw,
               subject=subj,
               acc                    =(prob1>prob2)*1)%>%
  mutate(delta_exp_value_oneback=lag(delta_exp_value),
         reoffer_ch             =(offer1==lag(choice)|offer2==lag(choice)),
         reoffer_unch           =(offer1==lag(unchosen)|offer2==lag(unchosen)),
         stay_frc_ch            =(choice==lag(choice)),
         stay_frc_unch          =(choice==lag(unchosen)),
         stay_key               =(key==lag(key)),
         reward_oneback         =lag(reward),
         acc_oneback            =lag(acc),
         prob1_oneback          =lag(prob1),
         prob2_oneback          =lag(prob2))

df$rt[df$rt==-1]<-NA

####### omit subjects due to no response probability higher then 5%----------------------
#check proportion
x     =df%>%group_by(subject)%>%summarise(no_response=mean(is.na(rt)))
sum(x$no_response>0.05)

#remove subjects with more than 5% no response trials
df    =df[df$subj %in% x$subject[x$no_response<0.05],]

#remove all no response trials
df<-na.omit(df)



####### omit subjects due to staying with the same key more then 90% of the trials----------------------
#check key stay probability 
x           <-df%>%group_by(subject)%>%summarise(key_rep=mean(is.na(stay_key)))
sum(x$key_rep>=0.9)



#######omit subjects with more the 10% very long or very short RTs----------------------
#check long rt probability
x=df%>%group_by(subject)%>%summarise(short_rt=mean(rt<200),long_rt=mean(rt>4000))
sum(x$long_rt>=0.1)
sum(x$short_rt>=0.1)

#remove participants with more than 10% short rt or/and 10% long rt trials
df    =df[df$subj %in% x$subject[!(x$short_rt>=0.1 | x$long_rt>=0.1)],]


#######omit trials with quick RTs, slow RTs and first trial ----------------------
#tag all short and long trials
df=df%>%mutate(abort=(rt<200 | rt>4000 | trl==1))
df%>%group_by(subject)%>%summarise(pabort=mean(abort))%>%summarise(mean(pabort),max(pabort))

df=df%>%filter(!abort)
save(df,file='./empirical_data_replication/df.rdata')
