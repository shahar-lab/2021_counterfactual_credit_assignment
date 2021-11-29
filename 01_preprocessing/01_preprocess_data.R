rm(list=ls())
library(data.table)
library(tidyverse)
tab<-data.table(read.csv('./data/raw_data/02_raw_data_csv/tab.csv'))

###### house keeping ----------------------
#sort trials
tab<-tab[order(tab$subj,tab$blk,tab$trl),]
tab$trl<-tab$trl+1
rownames(tab) <- 1:nrow(tab)

#add columns
tab$rw[tab$cond=="neg"]   <-tab$rw[tab$cond=="neg"] +1        #add absolute outcome (1\0) column to tab
tab$unch                  <-NA
tab$unch[tab$ch==tab$frcA]<-tab$frcB[tab$ch==tab$frcA]
tab$unch[tab$ch==tab$frcB]<-tab$frcA[tab$ch==tab$frcB]
tab$stay_frc_ch           <-tab$ch==shift(tab$ch,n=1,type='lag',fill=0)*1
tab$stay_frc_unch         <-tab$ch==shift(tab$unch,n=1,type='lag',fill=0)*1
tab$stay_key              <-tab$key==shift(tab$key,n=1,type='lag',fill=0)
tab$rw_oneback            <-shift(tab$rw,n=1,type='lag',fill=0)
tab$reoffer_ch            <-(tab$frcA==shift(tab$ch,n=1,type='lag',fill=0)|tab$frcB==shift(tab$ch,n=1,type='lag',fill=0))
tab$reoffer_unch          <-(tab$frcA==shift(tab$unch,n=1,type='lag',fill=0)|tab$frcB==shift(tab$unch,n=1,type='lag',fill=0))
tab$blk.part              <-1-(tab$blk%%2)
tab$cond                  <-(tab$cond=='pos')*1
tab$acc                   <-(tab$prob1>tab$prob2)*1 #prob1 is prob.ch
tab$trial.total           <-tab$trl+(tab$blk-1)*50

#replace -1 rt with values
tab$rt[tab$rt==-1]<-NA

####### omit subjects due to no response probability higher then 5%----------------------
#check proportion
df           <-tab[,mean(is.na(rt)==T), by=subj]
colnames(df) <- c("subject", "no_response")
plot(density(df$no_response))
sum(df$no_response>0.05)

#remove subjects with more than 5% no response trials
tab<-tab[tab$subj %in% df[(df$no_response>0.05)==FALSE]$subj]

#remove all no response trials
tab<-na.omit(tab)



####### omit subjects due to staying with the same key more then 90% of the trials----------------------
#check key stay probability 
df<-tab[,mean(stay_key), by=c('subj')]
colnames(df) <- c("subject", "key_rep")
plot(density(df$key_rep))
sum(df$key_rep>=0.9)



#######omit subjects with more the 10% very long or very short RTs----------------------
#check long rt probability
df<-tab[,mean(rt>4000), by=subj]
colnames(df) <- c("subject", "long_rt")
plot(density(df$long_rt))
sum(df$long_rt>=0.1)

#check short rt probability
df<-tab[,mean(rt<200)>0.1, by=subj]
colnames(df) <- c("subject", "short_rt")
sum(df$short_rt)

#remove participants with more than 10% short rt trials
tab<-tab[tab$subj %in% df[df$short_rt==FALSE]$subj]



#######omit trials with quick RTs, slow RTs and first trial ----------------------
#count how many trials we are going to keep
dim(tab[rt>200 & rt<4000])[1]/dim(tab)[1]

#remove all short and long trials
tab<-tab[rt>200 & rt<4000]

#remove first trial of every block
tab<-tab[trl>1]

save(tab, file='data/analysis_data/tab.Rdata')
tab_ch = tab%>%filter(!reoffer_unch,reoffer_ch)
tab_unch= tab%>%filter(!reoffer_ch,reoffer_unch)
save(tab_ch, file='data/analysis_data/tab_ch.Rdata')
save(tab_unch, file='data/analysis_data/tab_unch.Rdata')
#####
#We omitted participants due to no response probability higher then 5 (four participants omitted), 
#participants that stayed with the same key more them 90% of the trials (no participants were omitted),
#participants with more then 10% very long reaction-times (no participants were omitted), or 10% of very short 
#reaction-times (21 participants omitted). 
#We then took out trials with implausible fast or slow reaction-times (<200ms or >4000ms), which resulted in omission of 1.79% of the remaining trials.
#Finally we took out the first trial in each block.



# tab_twoback -------------------------------------------------------------

tab_two=tab #we create a copy to avoid problems with additional NAs due to two_back considerations.

tab_two$stay_frc_unch_two     <-
  tab_two$ch == shift(tab_two$unch,
                  n = 2,
                  type = 'lag',
                  fill = 0) * 1
tab_two$rw_twoback            <- shift(tab_two$rw,
                                   n = 2,
                                   type = 'lag',
                                   fill = 0)
tab_two$reoffer_ch_two        <-
  (
    tab_two$frcA == shift(
      tab_two$ch,
      n = 2,
      type = 'lag',
      fill = 0
    ) | tab_two$frcB == shift(
      tab_two$ch,
      n = 2,
      type = 'lag',
      fill = 0
    )
  )
tab_two$reoffer_unch_two      <-
  (
    tab_two$frcA == shift(
      tab_two$unch,
      n = 2,
      type = 'lag',
      fill = 0
    ) | tab_two$frcB == shift(
      tab_two$unch,
      n = 2,
      type = 'lag',
      fill = 0
    )
  )
tab_unch_two=tab_two%>%filter(trl-2==lag(trl,2),!reoffer_ch_two,reoffer_unch_two)

save(tab_unch_two, file='data/analysis_data/tab_unch_two.Rdata')


# replication -------------------------------------------------------------
load('data/replications/yaelT_fixed.Rdata') #fixed reward direction of win/lose
cars_unch=task%>%filter(reoffered_chosen==0,reoffered_unchosen==1)

save(cars_unch, file='data/analysis_data/cars_unch.Rdata')
