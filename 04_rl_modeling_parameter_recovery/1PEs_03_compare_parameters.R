#-------------------------------------------------------
rm(list=ls())
load('./data/modeling_data/1PE_100subjects_4blocks_50trials_4arms_parameters.Rdata')
load('./data/modeling_data/1PE_100subjects_4blocks_50trials_4arms_extracted_parameters.rdata')

library(ggplot2)
library(ggpubr)
#population level parameters
p1= ggplot(data.frame(x=pars$mu_alpha),aes(x=x))+geom_histogram(fill=alpha('purple',0.3))+ geom_vline(xintercept = 0.5, linetype="dotted",color = "blue", size=1.5)+
     xlab(expression(alpha))+ theme_classic()

p2= ggplot(data.frame(x=pars$mu_w),aes(x=x))+geom_histogram(fill=alpha('purple',0.3))+ geom_vline(xintercept = 0.75, linetype="dotted",color = "blue", size=1.5)+
      xlab(expression(omega))+ theme_classic()

p3= ggplot(data.frame(x=pars$mu_beta),aes(x=x))+geom_histogram(fill=alpha('brown',0.3))+ geom_vline(xintercept = 4, linetype="dotted",color = "blue", size=1.5)+
      xlab(expression(beta))+ theme_classic()


#compare individual level parameters
p4=ggplot(data.frame(x =true.parameters[,'alpha'], y =apply(pars$alpha, 2, mean)),aes(x=x,y=y))+geom_point(col='purple',alpha=0.7)+
    ggtitle('',subtitle = paste('r=',round(cor(true.parameters[,'alpha'], apply(pars$alpha, 2, mean)),3)))+
    xlab(expression(paste('simulated ',alpha)))+ylab(expression(paste('recovered ',alpha)))+ xlim(0,1)+ylim(0,1)+
    theme_classic()

p5=ggplot(data.frame(x =true.parameters[,'w'], y =apply(pars$w, 2, mean)),aes(x=x,y=y))+geom_point(col='purple',alpha=0.7)+
  ggtitle('',subtitle = paste('r=',round(cor(true.parameters[,'w'], apply(pars$w, 2, mean)),3)))+
  xlab(expression(paste('simulated ',omega)))+ylab(expression(paste('recovered ',omega)))+ xlim(0.5,1)+ylim(0.5,1)+
  theme_classic()
    
p6=ggplot(data.frame(x =true.parameters[,'beta'], y =apply(pars$beta, 2, mean)),aes(x=x,y=y))+geom_point(col='brown',alpha=0.7)+
    ggtitle('',subtitle = paste('r=',round(cor(true.parameters[,'beta'], apply(pars$beta, 2, mean)),3)))+
    xlab(paste('simulated ',expression(beta)))+ylab(paste('recovered ',expression(beta)))+ xlim(0,10)+ylim(0,10)+
    theme_classic()

ggarrange(p1,p2,p3,p4,p5,p6)

