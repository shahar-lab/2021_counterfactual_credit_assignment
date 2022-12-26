rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')

load('./data/empirical_data/brms_unchosen_weakly_informative_priors_delta_abs_exp.rdata')

p1=
plot(conditional_effects(model, 
                      effects = "delta_abs_exp_value_oneback:reward_oneback"),
                      plot = FALSE)[[1]]+
       xlab("previous ΔEV")+
       ylab('P(take previously unchosen)')+
      theme_classic()

p2=
my_posteriorplot(model = model,
                 variable_name = 'reward_onebackrewarded:delta_abs_exp_value_oneback',
                 xlabel  = expression(beta[interaction]),
                 mycolor = "blue",
                 0.95)

load('./data/empirical_data/brms_unchosen_weakly_informative_priors_rt_oneback.rdata')
p3=
  plot(conditional_effects(model, 
                           effects = "reward_oneback:rt_oneback",
                           categorical=F),
       plot = FALSE)[[1]]+
  xlab("previous RT")+
  ylab('P(take previously unchosen)')+
  theme_classic()

p4=
  my_posteriorplot(model = model,
                   variable_name = 'reward_onebackrewarded:delta_abs_exp_value_oneback',
                   xlabel  = expression(beta[interaction]),
                   mycolor = "blue",
                   0.95)
