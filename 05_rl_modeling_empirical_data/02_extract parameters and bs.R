#aim: Hierarchical fit Stan 
#contributor: Nitzan Shahar, 2021


rm(list=ls())

# bridgesampler  --------------------------------------------
library(bridgesampling) 
load('./data/tab_fitted_with_1PE_bridge_sampler.rdata')
onePE=bs
load('./data/tab_fitted_with_2PEs_bridge_sampler.rdata')
twoPEs=bs
load('./data/tab_fitted_with_null_bridge_sampler.rdata')
null=bs


bf(twoPEs,null)
bf(onePE,null)
bf(twoPEs,onePE)

rm(list=ls())

--------------------------------------------------------------
#examine posterior
load('./data/tab_fitted_with_2PEs_extracted_parameters.rdata')

library(bayestestR)
library(ggplot2)
library(ggpubr)

#population level parameters
p1= ggplot(data.frame(x=pars$mu_alpha_ch),aes(x=x))+geom_histogram(fill=alpha('purple',0.3))+ 
      #mean line
      geom_vline(xintercept = mean(pars$mu_alpha_ch), linetype="dotted",color = "darkgray", size=1.5)+
      #hdi
      geom_segment(aes(x = hdi(pars$mu_alpha_ch, ci = 0.95)$CI_low, y = 1, xend = hdi(pars$mu_alpha_ch, ci = 0.95)$CI_high, yend = 1),color="darkgray",size=2,show.legend = F)+
      #labels
      xlab(expression(alpha['chosen']))+ ylab('frequency')+theme_classic()

p2= ggplot(data.frame(x=pars$mu_alpha_unch),aes(x=x))+geom_histogram(fill=alpha('blue3',0.3))+
      #mean line
      geom_vline(xintercept = mean(pars$mu_alpha_unch), linetype="dotted",color = "darkgray", size=1.5)+
      #hdi
      geom_segment(aes(x = hdi(pars$mu_alpha_unch, ci = 0.95)$CI_low, y = 1, xend = hdi(pars$mu_alpha_unch, ci = 0.95)$CI_high, yend = 1),color="darkgray",size=2,show.legend = F)+
      #labels
      xlab(expression(alpha['unchosen']))+ ylab('frequency')+theme_classic()

p3= ggplot(data.frame(x=pars$mu_beta),aes(x=x))+geom_histogram(fill=alpha('brown',0.3))+ 
      #mean line
      geom_vline(xintercept = mean(pars$mu_beta), linetype="dotted",color = "darkgray", size=1.5)+
      #hdi
      geom_segment(aes(x = hdi(pars$mu_beta, ci = 0.95)$CI_low, y = 1, xend = hdi(pars$mu_beta, ci = 0.95)$CI_high, yend = 1),color="darkgray",size=2,show.legend = F)+
      #labels
      xlab(expression(beta))+ ylab('frequency')+theme_classic()

ggarrange(p1,p2,p3)

#compare individual level parameters
p4=ggplot(data.frame(x =apply(pars$alpha_ch, 2, mean), y =apply(pars$alpha_unch, 2, mean)),aes(x=x,y=y))+geom_point(col='purple',alpha=0.7)+
  ggtitle('',subtitle = paste('r=',round(cor(apply(pars$alpha_ch, 2, mean), apply(pars$alpha_unch, 2, mean)),3)))+
  xlab(expression(paste('simulated ',alpha['chosen'])))+ylab(expression(paste('recovered ',alpha['chosen'])))+ xlim(0,1)+ylim(0,1)+
  theme_classic()

p4
x=hdi(pars$mu_alpha_ch, ci = 0.95)$CI_low
x$CI_low
hdi(pars$mu_alpha_unch, ci = 0.95)
hdi(pars$mu_beta, ci = 0.95)
