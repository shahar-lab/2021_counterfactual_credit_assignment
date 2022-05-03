


con_task_json<-function(task_name,datafile,curnfolder,files,subnum) {
  x <- fromJSON(fromJSON(file=paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
  i<-sapply(1:length(x), function(i) {exists('trial_num',x[[i]])})
  x<-x[i]
  i<-sapply(1:length(x), function(i) {exists('FB_probs',x[[i]])})
  df1<-as.data.frame(do.call(rbind,x[i]))
  df2<-as.data.frame(do.call(rbind,x[!i]))
  df1<-data.frame(rw=unlist(df1['feedback']),trl=unlist(df1['trial_num']),
                  prob1=unlist(do.call(rbind,df1['FB_probs'][,1])[,1]),
                  prob2=unlist(do.call(rbind,df1['FB_probs'][,1])[,2]),
                  blk=unlist(df1$block_number))
  df2<-data.frame(rt=unlist(df2['rt']),trl=unlist(df2['trial_num']),key=unlist(df2['key_press']),
                  frcA=unlist(do.call(rbind,df2['stim_order'][,1])[,1]),frcB=unlist(do.call(rbind,df2['stim_order'][,1])[,2]),
                  ch=unlist(df2['stim_selected']),cond=unlist(df2['current_condition']),
                  blk=unlist(df2$block_number))
  library(data.table)
  df<-merge(df1,df2,by=c('blk','trl'),all=T)
  
  prolific_id<-as.data.frame(fromJSON(file = paste(curnfolder,'/',files[grepl('starter',files)],sep="")))
  colnames(prolific_id)<-c('prolific_id')
  df$prolific_id<-rep(prolific_id$prolific_id,dim(df)[1])
  df$subj<-rep(subnum,dim(df)[1])
  
  #find bonus
  #browser()
  x <- fromJSON(fromJSON(file = paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
  i<-sapply(1:length(x), function(i) {x[[i]]$trial_id=='end_game'})
  x<-x[i]
  df$bonus<-rep(x[[1]]$final_bonus,dim(df)[1])
  return(rbind(datafile,df))
}