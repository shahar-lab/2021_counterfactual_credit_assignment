rm(list=ls())
library("rjson")
source('./functions/convert_sr_json.R')
source('./functions/convert_task_json.R')
mainfolder<-paste(getwd(),'/data/raw_data/01_raw_json_data',sep="")

subfolder=dir(mainfolder)
bdi<-oci<-spq<-stai<-tab<-data.frame()

print(length(subfolder))
for (i in 1:length(subfolder)){
  print(subfolder[i])
  print(i)
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))

  #self reports
  bdi <-con_sr_json('bdi',bdi,curnfolder,files,i)
  oci <-con_sr_json('oci',oci,curnfolder,files,i)
  stai<-con_sr_json('stai',stai,curnfolder,files,i)
  spq <-con_sr_json('spq',spq,curnfolder,files,i)

  #task
  tab<-con_task_json('test',tab,curnfolder,files,i) 
}  




#save csv
write.csv(tab,'myfolder/03_data/01_raw_data/02_raw_data_csv/tab.csv', row.names=FALSE)
write.csv(bdi,'myfolder/03_data/01_raw_data/02_raw_data_csv/bdi.csv', row.names=FALSE)
write.csv(oci,'myfolder/03_data/01_raw_data/02_raw_data_csv/oci.csv', row.names=FALSE)
write.csv(stai,'myfolder/03_data/01_raw_data/02_raw_data_csv/stai.csv', row.names=FALSE)
write.csv(spq,'myfolder/03_data/01_raw_data/02_raw_data_csv/spq.csv', row.names=FALSE)
