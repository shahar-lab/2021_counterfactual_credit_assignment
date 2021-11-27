rm(list = ls())
library(data.table)

load('data/analysis_data/tab.Rdata')
subject_ids<- unique(tab$prolific_id) 

data<-data.table(read.csv('data/raw_data/02_raw_data_csv/demographic_data_from_prolific.csv'))
data<- data[data$participant_id %in% subject_ids,c('participant_id', 'age','Sex', 'First.Language')]
colnames(data)[1]<-'prolific_id'

####Age statistics
mean_age<-mean(data$age, na.rm = T)
max_age<-max(data$age, na.rm = T)
min_age<-min(data$age, na.rm = T)

####Sex statistics
females<-sum(data$Sex=='Female')
males<-sum(data$Sex=='Male')
unknown_sex<-sum(data$Sex=='CONSENT REVOKED')

###First language statistics
english_birth_speakers<-sum(data$First.Language=='English')


###add gender to task data
gender<-vector()  
for (i in 1:dim(tab)[1]){
  index    =data$prolific_id==tab$prolific_id[i]
  gender[i]=data$Sex[index]
}
tab$gender<-gender
tab[tab$gender=='CONSENT REVOKED',]<-NA
tab<-na.omit(tab)
tab$gender<-factor(tab$gender)
summary(tab)

