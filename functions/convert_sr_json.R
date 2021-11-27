con_sr_json<-function(sr_name,datafile,curnfolder,files,subnum) {
  #browser()
  #this function aggregates self reports from json to data frame
  prolific_id<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('starter',files)],sep="")))
  colnames(prolific_id)<-c('prolific_id')
  if (sum(grepl(sr_name,files))>0) {
    x <- as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl(sr_name,files)],sep="")))
    x <- x[,grepl('value',colnames(x))]
    colnames(x)<-sapply(1:ncol(x), function(cl) paste('item_',as.numeric(substr(gsub("([0-9]+).*$", "\\1", colnames(x)[cl]), start = 6,stop=1000000L))+1,sep=""))
    x$prolific_id<-rep(prolific_id$prolific_id,dim(x)[1])
    x$subj<-rep(subnum,dim(x)[1])
    library(gtools)    
    return(smartbind(datafile,x))}
  else{
    x<-as.data.frame(t(rep(NA,dim(datafile)[2])))
    colnames(x)<-colnames(datafile)
    x$prolific_id<-prolific_id$prolific_id
    x$subj<-subnum
    return(rbind(datafile,x))
  }
}