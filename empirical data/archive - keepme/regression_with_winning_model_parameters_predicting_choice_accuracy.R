# Here we examine whether parameters of the winning model, approach-avoid, predicts choice accuracy

rm(list=ls())
source('./functions/my_packages.r')

#load parameters of avoid-approach
load('./data/model_approach_avoid/modelfit_based_on_empirical_data.rdata')
par_alpha=apply(plogis(pars$alpha_ch_random_effect),2,mean)
par_beta =apply(plogis(pars$beta_random_effect),2,mean)
par_omega=apply(plogis(pars$omega_random_effect),2,mean)


#load empirical data
load('./data/empirical_data/df.rdata')
df<-na.omit(df)

#create standardized df with parameters and mean acc
acc=df%>%group_by(subject)%>%summarise(mean_acc=mean(acc))
df=data.frame(acc,par_alpha,par_beta,par_omega)

#calculate person correlation
library(BayesFactor)
result <- correlationBF(df$mean_acc, df$par_omega)
describe_posterior(result)

#plot omega over choice acc
source('./functions/my_xyplot.R')
my_xyplot(df$par_omega,df$mean_acc,expression(omega),'mean choice accuracy','navy')+geom_smooth(method='lm')


#fit linear regression
df=as.data.frame(scale(df))
model=brm(mean_acc~par_alpha+par_beta+par_omega,data=df)
describe_posterior(model)
pars=posterior_samples(model)




