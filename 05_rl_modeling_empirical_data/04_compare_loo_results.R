# Niter x Nobs
# 
# 8000 x Nsubj*Nblock
# 8000 x 712

rm(list=ls())
----------compare
library(loo)
load('./data/modeling_data/loo-block.rdata')



myelpd=
  lapply(1:4,   function(model_index){
    l=lapply(1:178, function(subject) {do.call(cbind,list(my_log_lik[[model_index]][[1]][,subject],
                                                          my_log_lik[[model_index]][[2]][,subject],
                                                          my_log_lik[[model_index]][[3]][,subject],
                                                          my_log_lik[[model_index]][[4]][,subject]))})
    l=do.call(cbind,l)
    elpd(l)
  })


loo_compare(myelpd[[1]],myelpd[[2]],myelpd[[4]])


loo_compare(null,discount,inv1PE,onePE,twoPE)
library(ggplot2)
library(ggpubr)
p1=ggplot(data.frame(null_elpd   = null$pointwise[,'elpd'],
                  twoPE_elpd  = twoPE$pointwise[,'elpd']),
          aes(x=null_elpd,y=twoPE_elpd))+
          geom_point()+
          geom_abline(intercept=0,slope=1)+
          xlim(-35,-15)+ylim(-35,-15)
  

p2=ggplot(data.frame(null_elpd   = null$pointwise[,'elpd'],
                     twoPE_elpd  = twoPE$pointwise[,'elpd']),
          aes(x=(twoPE_elpd-null_elpd)))+
          geom_histogram()+
          xlim(-5,5)

annotate_figure(ggarrange(p1,p2,nrow=1,ncol=2), 
                top = text_grob("difference in pointwise predictive density for 2PEs vs. null mode", color = "black", face = "bold", size = 10))

