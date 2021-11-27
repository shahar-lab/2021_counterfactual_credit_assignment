rm(list=ls())

#load data --------------------------------
load(paste('./data/modeling_data/null_100subjects_4blocks_50trials_4arms_simdata.Rdata',sep=""))

df$action        =df$choice
df$unchosen      =df$offer1
df$unchosen[df$choice==df$offer1]=df$offer2[df$choice==df$offer1]
df$selected_offer=(df$choice==df$offer2)*1+1

#fit with optim ------------------------------------------------
library(gtools)
pars    <-  lapply(1:max(df$subject), 
                    function(s) {if (s%%10==T) {print(paste('subj=',s,sep=''))}
      
                    optim(par= c(runif(1,-5,5),runif(1,-3,3)),
                    fn = fit.block, df= df[df$subject==s,],Nalt=4,
                    lower = c(-5,-3), upper = c(5,3),
                    method="L-BFGS-B")$par})
pars=do.call(rbind,pars)

#compare true and recovered ------------------------------------------------
load('data/modeling_data/null_100subjects_4blocks_50trials_4arms_parameters.Rdata')

#alpha
plot(true.parameters[,'alpha'],inv.logit(pars[,1]))
cor(true.parameters[,'alpha'],inv.logit(pars[,1]))
hist(true.parameters[,'alpha']-inv.logit(pars[,1]))

plot(true.parameters[,'beta'],exp(pars[,2]))
cor(true.parameters[,'beta'],exp(pars[,2]))
hist(true.parameters[,'beta']-exp(pars[,2]))
               
fit.block = function(par, df,Nalt){ 
  Q              = rep(0,Nalt)
  trials         = dim(df)[1]
  action         = df$action
  reward         = df$reward
  offer1         = df$offer1
  offer2         = df$offer2
  selected_offer = df$selected_offer
  pAction        = rep(NA, trials)
  
  alpha          = gtools::inv.logit(par[1])
  beta           = exp(par[2])
  
  for (t in 1:trials){
    Qoffer       =c(Q[offer1[t]],Q[offer2[t]])
    pAction[t]   = exp(beta*Qoffer[selected_offer[t]]) / sum(exp(beta*Qoffer))
    Q[action[t]] = Q[action[t]] + alpha*(reward[t] - Q[action[t]])
  }
  return (-sum(log(pAction)))
}
