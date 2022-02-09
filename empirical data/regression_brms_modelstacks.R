#this code compare two model where reward_oneback predicts selection
#of an unchosen option. A model with reward_oneback as an only predictor
#and a model with reward_oneback x condition (0=negative, 1=positive blocks)

rm(list=ls())
source('./functions/my_packages.r')

#----------------------------------------------------------------------------------

#load models
load('./data/empirical_data/brms_unchosen_reward_oneback_x_cond_weakly_informative_priors_with_reward_onebackXcond_random.rdata')
model2=model
load('./data/empirical_data/brms_unchosen_reward_oneback_weakly_informative_priors_with_reward_onebackXcond_random.rdata')
model1=model
load('./data/empirical_data/brms_unchosen_empty_model_weakly_informative_priors_with_reward_onebackXcond_random.rdata')
model0=model


#run loo
model2 <- loo(model2)
model1 <- loo(model1)
model0 <- loo(model0)

#compare with loo
loo_compare(model2,model1,model0)

#stacking weights
stack_models <- cbind(model2$pointwise[,"elpd_loo"],
                      model1$pointwise[,"elpd_loo"],
                      model0$pointwise[,"elpd_loo"])
stacking_weights(stack_models)


