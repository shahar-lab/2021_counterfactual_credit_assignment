#aim: Generated simulated data for Narmed bandit task using hierarchical group parameters and individual parameters
#author: Nitzan Shahar, 2021

rm(list=ls())

model_name=c('1PE')

Nsubjects           =100        
Nblocks             =4
Ntrials_perblock    =50
Ntrials             =Nblocks*Ntrials_perblock
Narms               =4
rndwlk              =read.csv('./data/rndwlk_4frc_1000trials.csv',header=F)[,1:Ntrials_perblock]
Nraffle             =2


# generate population and subject level parameters -----------------------------------------------------------

#population location parameters
  
  #true population level parameters
  mu_alpha   =0.5
  mu_w       =0.75
  mu_beta    =4
  

  #transform to aux scale
  beta_range =10
  w_range    =0.5
  w_shift    =0.5
  mu_aux     =c(qnorm(mu_alpha),qnorm((mu_w-w_shift)/w_range),qnorm(mu_beta/beta_range))
  
  #transform back to natural scale (just for practice)
  mu_alpha   =pnorm(mu_aux[1], 0,1);
  mu_w       =pnorm(mu_aux[2], 0,1)*w_range+w_shift;
  mu_beta    =pnorm(mu_aux[3], 0,1)*beta_range;
  print(paste(round(mu_alpha,2),round(mu_w,2),round(mu_beta,2)))
  
  #scale parameter for the random effect
  sigma_aux  =c(0.75,0.75,1) 
  


#individual level parameters
  
  #random effect for each individual
  alpha_individal_aux  =rnorm(Nsubjects,0, 1);
  w_individal_aux      =rnorm(Nsubjects,0, 1);
  beta_indvidial_aux   =rnorm(Nsubjects,0, 1);
  
  #RL parameters per individual given population and group effects
  alpha           = pnorm(mu_aux[1]  + sigma_aux[1]*alpha_individal_aux);
  w               = pnorm(mu_aux[2]  + sigma_aux[2]*w_individal_aux)*w_range+w_shift;
  beta            = pnorm(mu_aux[3]  + sigma_aux[3]*beta_indvidial_aux) * beta_range;

  #plot true parameters
  true.parameters=cbind(
    subject=seq(1,Nsubjects),
    alpha  =alpha,
    w      =w,
    beta   =beta
  )
  psych::multi.hist(true.parameters,density=F,nrow=1)

  #check that sample means is near true population means
  print(paste(round(mean(alpha),2),round(mean(w),2),round(mean(beta),2)))
  


# generate data -----------------------------------------------------------

cfg = list(Nblocks         =Nblocks,
           Ntrials_perblock=Ntrials_perblock,
           Narms           =Narms,    
           Nraffle         =Nraffle,  #(i.e., offer Nraffle arms each trial from a deck of Narms)
           rndwlk          =rndwlk)

source('./models/simulation_Narmed_bandit_task_onePE.R')

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
}

  



#save-------------------------------------------------------------------
save(df,file=paste('./data/',model_name,'_',Nsubjects,'subjects_',Nblocks,'blocks_',Ntrials_perblock,'trials_',Narms,'arms_simdata.Rdata',sep=""))
save(true.parameters,file=paste('./data/',model_name,'_',Nsubjects,'subjects_',Nblocks,'blocks_',Ntrials_perblock,'trials_',Narms,'arms_parameters.Rdata',sep=""))

#convert to standata format-------------------------------------------------

# add abort column to simulate missing trials 
max_precent_of_aborted_trials=0
df$abort<-0
Nsubjects=max(df$subject)
Ntrials  =df%>%group_by(subject)%>%summarise(Ntrials_max=(length(trial)))%>%summarise(max(Ntrials_max))%>%as.numeric()

for (subject in seq(1:max(df$subject))){
  index_abort           =sample(which(df$subject==subject),runif(1,min=0,max=max_precent_of_aborted_trials)*Ntrials)  #index of rows to abort
  df$abort[index_abort] =1
}

df%>%group_by(subject)%>%summarise(mean(abort)) #count and omit aborted trials
df<-df[df$abort==0,]
df%>%group_by(subject)%>%summarise(mean(abort))

#some housekeeping
library(dplyr)

df$action        =df$choice
df$unchosen      =df$offer1
df$unchosen[df$choice==df$offer1]=df$offer2[df$choice==df$offer1]
df$selected_offer=(df$choice==df$offer2)*1+1
df=df%>%mutate(first_trial_in_block=(block!=lag(block,default = 0))*1)%>%as.data.frame()
df$fold = df$block

#make standata
source('./functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=df, 
                               subject_column     =df$subject,
                               block_column       =df$block,
                               var_toinclude      =c(
                                 'fold',
                                 'first_trial_in_block',
                                 'trial',
                                 'offer1',
                                 'offer2',
                                 'action',
                                 'unchosen',
                                 'reward',
                                 'selected_offer'),
                               additional_arguments=list(Narms=4, Nraffle=2))

save(data_for_stan,file=paste('./data/',model_name,'_',Nsubjects,'subjects_',Nblocks,'blocks_',Ntrials_perblock,'trials_',Narms,'arms_standata.Rdata',sep=""))



#check simulated data -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(effects)

#sanity check 1: plot mean reward vs expected value
model<-glmer(reward ~ expval_ch+(1| subject),data = df, family = binomial)
plot(effect('expval_ch',model))


#sanity check 2: pStay model-agnostic analysis - chosen
df=df%>%mutate(stay=(choice==lag(choice,default=0))*1,
               reward_oneback=lag(reward,default=0))

model<-glmer(stay ~ reward_oneback+(reward_oneback| subject), 
             data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))

#sanity check 3: pStay model-agnostic analysis - unchosen
df=df%>%mutate(stay=(choice==lag(unchosen,default=0))*1,
               reward_oneback=lag(reward,default=0))

model<-glmer(stay ~ reward_oneback+(reward_oneback| subject), 
             data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))


#plot reward effect on pstay agianst parameters
df_plot=cbind(coef(model)$subject,as.data.frame(true.parameters))
gridExtra::grid.arrange(
  ggplot(df_plot,aes(x=alpha,y=reward_oneback ))+geom_point(size=3),
  ggplot(df_plot,aes(x=beta,y=reward_oneback ))+geom_point(size=3),
  ggplot(df_plot,aes(x=alpha,y=beta,color=reward_oneback ))+geom_point(size=3),
  ncol=3
)


#sanity check 3: plot mean reward vs parameters
model<-glmer(reward ~ poly(alpha,2)+beta+(1| subject), 
             data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('alpha',model)) #(if you use uniform alpha and fixed beta - you will be able to see a nice hyperbolic)
plot(effect('beta',model)) #(if you use uniform alpha - you will be able to see a nice hyperbolic)
