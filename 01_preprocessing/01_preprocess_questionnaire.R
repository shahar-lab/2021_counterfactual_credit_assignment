rm(list=ls())
library(dplyr)
library(magrittr)

mainfolder<-paste(getwd(),'/myfolder/03_data/01_raw_data/02_raw_data_csv/',sep="")

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
  select(prolific_id,subj,hoard,check,order,count,clean,obsess,oci)
head(oci)
hist(oci$oci)

#PCA
library(psych)
library(nFactors)
fit.sr <- principal(oci[,c(4:7)],nfactors = 1,rotate = 'promax',scores=TRUE)
scree(oci[,3:8],factors=TRUE,pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 
parallel<-fa.parallel(oci[,3:8], fm="ml", fa="pc", main = "Parallel Analysis Scree Plots",
                      n.iter=20,error.bars=T,ylabel=NULL,show.legend=TRUE,sim=TRUE)
print(fit.sr$loadings,cutoff = 0.5)
oci$comp=fit.sr$scores[,1]
#oci<-oci%>%mutate(comp=(check+order+count+clean)/4)

save(oci,file='myfolder/03_data/01_raw_data/03_raw_clean_data/oci.Rdata')


#####BDI ------------------------------------------------------------------------------------
bdi<-read.csv(paste(mainfolder,'bdi.csv',sep=""))
names(bdi)
bdi$bdi<-apply(bdi[,3:23],1,sum)
bdi%<>%select(prolific_id,subj,bdi)
hist(bdi$bdi)
save(bdi,file='myfolder/03_data/01_raw_data/03_raw_clean_data/bdi.Rdata')


#####STAI ------------------------------------------------------------------------------------
stai<-read.csv(paste(mainfolder,'stai.csv',sep=""))
stai<-stai[str_sort(colnames(stai), numeric = TRUE)]
names(stai)
stai$state<-rowSums(stai[,1:20])
stai$trait<-rowSums(stai[,21:40])
stai%<>%select(prolific_id,subj,state,trait)

save(stai,file='myfolder/03_data/01_raw_data/03_raw_clean_data/stai.Rdata')

#####SPQ ------------------------------------------------------------------------------------
spq<-read.csv(paste(mainfolder,'spq.csv',sep=""))
names(spq)
spq$spq<-apply(spq[,3:37],1,sum)
spq%<>%select(prolific_id,subj,spq)
hist(spq$spq)
save(spq,file='myfolder/03_data/01_raw_data/03_raw_clean_data/spq.Rdata')





##give participants with missing stai scores the mean score
#quest$state[is.na(quest$state)]<- mean(quest$state, 0, TRUE)
#quest$trait[is.na(quest$trait)]<- mean(quest$trait, 0, TRUE)

library(data.table)
library(stringr)
source('11_myfunc.R')

folder<-'myfolder/02_raw_data'
sub_folder<-'myfolder/03_raw_clean_data'
exported_file<-data.frame(read.csv('myfolder/00_raw_exported_data/exported_all.csv'))
names<-c('oci')
#names<-c('bdi', 'oci', 'stai', 'spq')

#vector to clean bad participants
tab<-read.csv(paste(sub_folder, '/', '01_tab','.csv', sep=""))
good_subj<-tab$subj
good_subj<-good_subj[!duplicated(good_subj)]

#clean bad subjects+add sum columns
for (i in 1:length(names)){
  assign(names[i], sum_statements(names[i], folder, good_subj))
}

plot(hist(oci$sum))

write.csv(oci, 'myfolder/03_raw_clean_data/01_oci.csv', row.names=FALSE)

#add exported data columns (age and sex)
# ids<-tab$prolific_id
# clean_prolific_data_file<-setDT(exported_file)[participant_id %in% ids,]
# names(clean_prolific_data_file)[names(clean_prolific_data_file) == "participant_id"] <- "prolific_id"
# for (i in 1:length(names)){
#   assign(names[i], add_exported_data(eval(parse(text = names[i])), exported_file))
# }

#save a filtered exported file
# write.csv(exported_file, 'myfolder/00_raw_exported_data/01_oci.csv', row.names=FALSE)

#save the data
# for (i in 1:length(names)){
#   save_data(eval(parse(text = names[i])), names[i], sub_folder)
# }