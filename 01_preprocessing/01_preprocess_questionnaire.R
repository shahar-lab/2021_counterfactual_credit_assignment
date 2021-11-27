rm(list=ls())
library(dplyr)
library(magrittr)
library(stringr)
library(data.table)
library(psych)
library(nFactors)

mainfolder<-paste(getwd(),'/data/raw_data/02_raw_data_csv/',sep="")
load('data/analysis_data/tab.Rdata')
good_subj<-unique(tab$subj)
#####OCI ------------------------------------------------------------------------------------
oci<-read.csv(paste(mainfolder,'oci.csv',sep=""))
names(oci)
oci$oci<-apply(oci[,1:18],1,sum)
oci%<>%
  mutate(hoard=rowMeans(cbind(item_1,item_7,item_13)),
         check=rowMeans(cbind(item_2,item_8,item_14)),
         order=rowMeans(cbind(item_3,item_9,item_15)),
         count=rowMeans(cbind(item_4,item_10,item_16)),
         clean=rowMeans(cbind(item_5,item_11,item_17)),
         obsess=rowMeans(cbind(item_6,item_12,item_18)))%>%
  filter(subj %in% good_subj)%>%
  select(prolific_id,subj,hoard,check,order,count,clean,obsess,oci)
head(oci)
hist(oci$oci)

#PCA

fit.sr <- principal(oci[,c(4:7)],nfactors = 1,rotate = 'promax',scores=TRUE)
scree(oci[,3:8],factors=TRUE,pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 
parallel<-fa.parallel(oci[,3:8], fm="ml", fa="pc", main = "Parallel Analysis Scree Plots",
                      n.iter=20,error.bars=T,ylabel=NULL,show.legend=TRUE,sim=TRUE)
print(fit.sr$loadings,cutoff = 0.5)
oci$comp=fit.sr$scores[,1]
#oci<-oci%>%mutate(comp=(check+order+count+clean)/4)

save(oci,file='data/analysis_data/oci.Rdata')


#####BDI ------------------------------------------------------------------------------------
bdi<-read.csv(paste(mainfolder,'bdi.csv',sep=""))
names(bdi)
bdi$bdi<-apply(bdi[,3:23],1,sum)
bdi%<>%select(prolific_id,subj,bdi)%>%filter(subj %in% good_subj)
hist(bdi$bdi)
save(bdi,file='data/analysis_data/bdi.Rdata')


#####STAI ------------------------------------------------------------------------------------
stai<-read.csv(paste(mainfolder,'stai.csv',sep=""))
stai<-stai[str_sort(colnames(stai),numeric = TRUE)]
names(stai)
stai$state<-rowSums(stai[,1:20])
stai$trait<-rowSums(stai[,21:40])
stai%<>%select(prolific_id,subj,state,trait)%>%filter(subj %in% good_subj)

save(stai,file='data/analysis_data/stai.Rdata')

#####SPQ ------------------------------------------------------------------------------------
spq<-read.csv(paste(mainfolder,'spq.csv',sep=""))
names(spq)
spq$spq<-apply(spq[,3:37],1,sum)
spq%<>%select(prolific_id,subj,spq)%>%filter(subj %in% good_subj)
hist(spq$spq)
save(spq,file='data/analysis_data/spq.Rdata')

