#This code plot recovered parameters against the true parameters


rm(list=ls())
model_name=c('null')
load(paste('./data/',model_name,'_recovered_parameters.rdata',sep=""))
load(paste('./data/',model_name,'_true_parameters.Rdata',sep=""))


library(ggplot2)
library(ggpubr)
library(bayestestR)
#-------------------------------------------------------------------------------------------------------------
# #population level parameters
source('./functions/my_posteriorplot.R')


p1= my_posteriorplot(x       = plogis(pars$population_locations[,1]),
                     myxlim  = c(0,0.5),
                     my_vline= 0.3, 
                     myxlab  = expression(alpha['location']),
                     mycolor = "pink")


p2= my_posteriorplot(x       = pars$population_locations[,2],
                     myxlim  = c(0.5,1.5),
                     my_vline= 1, 
                     myxlab  = expression(beta['location']),
                     mycolor = "pink")


p3= my_posteriorplot(x       = pars$population_scales[,1],
                     myxlim  = c(0.5,1.5),
                     my_vline= 1, 
                     myxlab  = expression(alpha['scale']),
                     mycolor = "yellow")


p4= my_posteriorplot(x       = pars$population_scales[,2],
                     myxlim  = c(0,1),
                     my_vline= 0.5, 
                     myxlab  = expression(beta['scale']),
                     mycolor = "yellow")

annotate_figure(ggarrange(p1,p2,p3,p4,nrow=2,ncol=2), 
                top = text_grob("Population Level Parameters (fixed effects)", color = "black", face = "bold", size = 10))

#-------------------------------------------------------------------------------------------------------------
# individual level parameters

p1=ggplot(data.frame(x =true.parameters[,'alpha'], y =apply(pars$alpha, 2, mean)),aes(x=x,y=y))+geom_point()+
    labs(title='',
         subtitle = paste('r=',round(cor(true.parameters[,'alpha'], apply(pars$alpha, 2, mean)),2)),
         x=expression(alpha['true']),
         y=expression(alpha['recovered']))+ 
    xlim(0,1)+ylim(0,1)+
    theme_classic()
    
p2=ggplot(data.frame(x =true.parameters[,'beta'], y =apply(pars$beta, 2, mean)),aes(x=x,y=y))+geom_point()+
    labs(title='',
         subtitle = paste('r=',round(cor(true.parameters[,'beta'], apply(pars$beta, 2, mean)),2)),
         x=expression(beta['true']),
         y=expression(beta['recovered']))+ 
    xlim(0,10)+ylim(0,10)+
    theme_classic()

annotate_figure(ggarrange(p1,p2,nrow=1,ncol=2), 
                top = text_grob("Individual Level Parameters (random effects)", color = "black", face = "bold", size = 10))

