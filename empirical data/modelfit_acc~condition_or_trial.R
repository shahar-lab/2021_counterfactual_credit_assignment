library(brms)
rm(list=ls())
load('./data/empirical_data/df.rdata')


####accuracy per condition ---------------
df_summ <- 
  df |> 
  dplyr::count(subject, condition, acc,
               name = "k")

contrasts(df_summ$condition) <- c(-1,1)

myprior= c(prior(normal(0,0.2), b,  Intercept),
           prior(normal(0,0.2), b,  condition1)) 

model <- brm(acc | weights(k) ~ 0 + Intercept + condition + (condition | subject),
             data    = df_summ,
             family  = bernoulli(link = "logit"),
             iter    = 10,
             warmup  = 1,
             backend = 'cmdstan',
  prior = myprior,
  chain = 1

)

model <- update(model,
                sample_prior = FALSE, 
                cores = 4, 
                chains = 4,
                iter = 2000,
                warmup = 1000)
em=emmeans(model,~condition,type='response')
emmeans::regrid(em)


####accuracy per trial ---------------
df_summ <- 
  df |> 
  dplyr::count(subject, trl, acc,
               name = "k")

df_summ$trl = (df_summ$trl-2)

myprior= c(prior(normal(0,0.2), b,  Intercept),
           prior(normal(0,0.2/50), b,  trl)) 

model <- brm(acc | weights(k) ~ 0 + Intercept + trl + (trl | subject),
             data    = df_summ,
             family  = bernoulli(link = "logit"),
             iter    = 10,
             warmup  = 1,
             backend = 'cmdstan',
             prior = myprior,
             chain = 1
             
)

model <- update(model,
                sample_prior = FALSE, 
                cores = 4, 
                chains = 4,
                iter = 2000,
                warmup = 1000)

save(model,file='./data/empirical_data/brms_acc~trial.rdata')

