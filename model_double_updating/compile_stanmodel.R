#aim: compile and save stan model (so you wont have to redo this everytime you re-run the model with different parameters)

library(rstan) 

# compile stan model----------------------------------------------

my_compiledmodel=stan_model('./models/null.stan')
save(my_compiledmodel,file='./data/my_compiledmodel.rdata')
