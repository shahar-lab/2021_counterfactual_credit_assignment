# Here we examine to what extent simulated data that was generated with individuals estimate parameters 
# from the winning model, approach-avoid, generates the reward_oneback effect seen in empirical data

rm(list=ls())
source('./functions/my_packages.r')


#load simulated data of avoid-approach model that is based on individual parameters
load('data/model_approach_avoid/simulated_data_based_on_empirical_parameters.Rdata')

df.sim=df%>%mutate(reoffer_ch             =(offer1==lag(choice)|offer2==lag(choice)),
                   reoffer_unch           =(offer1==lag(unchosen)|offer2==lag(unchosen)),
                   stay_frc_ch            =(choice==lag(choice))*1,
                   stay_frc_unch          =(choice==lag(unchosen))*1,
                   reward_oneback         =factor(lag(reward,default=0),labels=c('unrewarded','rewarded'))
)


#get maximum likelihood estimates for the reward_oneback effect of the simulated data

library(brms)

myprior=c(set_prior(prior = "normal(0,0.2)",class = "b", coef = "Intercept"),
          set_prior(prior = "normal(0,0.2)",class = "b", coef = "reward_onebackrewarded"))


model= brm(stay_frc_unch ~ 0 + Intercept+reward_oneback+(1+reward_oneback| subject), 
             data = df.sim%>%filter(reoffer_ch==F,reoffer_unch==T), 
             family = bernoulli(link = "logit"),
             warmup = 1000,
             iter = 2000,    
             cores =4,
             chains=4,
             prior=myprior)

plot(conditional_effects(model), plot = FALSE)[[1]] + ggtitle("(A) model simulation") + xlab("previous-outcome") +
  ylab("pStay(unchosen)") + theme_bw()+
  scale_x_discrete(labels=c("Unrewarded","Rewarded"))

