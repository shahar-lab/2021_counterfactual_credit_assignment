rm(list=ls())
library(brms)
load('./data/empirical_data/brms_unchosen~reward_oneback_withrwXcond_randeff.rdata')
model1=model
load('./data/empirical_data/brms_unchosen~reward_onebackXcondition.rdata')
model2=model



#run loo
model1 <- loo(model1)
model2 <- loo(model2)


#compare with loo
loo_compare(model1,model2)

#stacking weights
stack_models <- cbind(model1$pointwise[,"elpd_loo"],
                      model2$pointwise[,"elpd_loo"])
loo::stacking_weights(stack_models)

