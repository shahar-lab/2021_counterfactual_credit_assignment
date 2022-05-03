#This code plot recovered parameters against the true parameters

rm(list=ls())
# samples x observations

#--------------------------------------------------------------------------------------------------------
library(loo)

mymodel='null'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
null=elpd(like)$pointwise[,1]

mymodel='double_updating'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
double_updating=elpd(like)$pointwise[,1]

mymodel='approach_avoid'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
approach_avoid=elpd(like)$pointwise[,1]

mymodel='single_prediction_error'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
single_prediction_error=elpd(like)$pointwise[,1]

mymodel='deliberation'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
deliberation=elpd(like)$pointwise[,1]



#compare model trial-by-trial-----------------------------------------
library(dplyr)
library(tidyr)
load('./data/empirical_data/df.rdata')

df=cbind(df,null=exp(null),double_updating=exp(double_updating),approach_avoid=exp(approach_avoid),
         single_prediction_error=exp(single_prediction_error),
         deliberation=exp(deliberation))

write.csv(df,file='elpd_forBruno.csv')

df=cbind(df,null=(null),double_updating=(double_updating),approach_avoid=(approach_avoid),
         single_prediction_error=(single_prediction_error),
         deliberation=(deliberation))



df=df%>%filter(subj==4)


df=
df%>%group_by(subj)%>%summarise(null=mean(null),
                                double_updating=mean(double_updating),
                                approach_avoid=mean(approach_avoid),
                                single_prediction_error=mean(single_prediction_error),
                                deliberation=mean(deliberation))

write.csv(df,file='elpd_wide_forjasp.csv')



library(ggplot2)
df=
  df%>%
  pivot_longer(cols=c(null,
                      double_updating,
                      approach_avoid,
                      single_prediction_error,
                      deliberation),
               values_to="elpd",
               names_to = "model")

write.csv(df,file='elpd_long_forjasp.csv')
ggplot(df,aes(x=trl,y=elpd,color=model))+
        geom_line()







# df$
# names(df)
# df%>%mutate(elpd_diff_winning_against_second_best=approach_avoid-double_updating,
#             elpd_diff_winning_against_null=approach_avoid-null)%>%
#      group_by(reoffer_unch)%>%
#      summarise(mean(elpd_diff_winning_against_null),
#                mean(elpd_diff_winning_against_second_best))
# 
# t.test(df$null[df$reoffer_unch &df$reoffer_unch_twoback],df$approach_avoid[df$reoffer_unch],paired=T)
# t.test(df$null[!df$reoffer_unch],df$approach_avoid[!df$reoffer_unch],paired=T)
# 
# t.test(df$null[df$reoffer_ch&df$reoffer_unch],df$double_updating[df$reoffer_ch&df$reoffer_unch],paired=T)
# t.test(df$null[!df$reoffer_ch&df$reoffer_unch],df$double_updating[!df$reoffer_ch&df$reoffer_unch],paired=T)
# 
# t.test(df$null[df$reoffer_unch]-df$approach_avoid[df$reoffer_unch])
# 
# 
