plot_posterior=function(model,effect_string,xlabel){
  #Bayesian visualisation -------------------------------------------------
  library(bayestestR)
  library(bayesplot)
  library(tidyverse) # for data manipulation and plots
  library(ggplot2)
  library(dplyr)
  # Plot the distribution and add the limits of the two CIs
  posteriors = insight::get_parameters(model)
  #HDI
  ci_hdi <- ci(posteriors[,effect_string], method = "HDI")
  #prior
  prior = distribution_normal(4000, mean = 0, sd = 0.2)
  #support interval
  # si_1 = si(posteriors$b_rw_oneback, prior, BF = 1)
  if (grepl(':',effect_string)){
    gg_string=paste('`',effect_string,"`",sep="")
  }
  else{
    gg_string=effect_string
  }
  ggplot(posteriors, aes_string(x = gg_string)) +
    theme_classic()+
    geom_density(fill = "orange") +
    # The median in red
    geom_vline(
      xintercept = median(posteriors[,effect_string]),
      color = "red",
      size = 1
    ) +
    geom_vline(xintercept = ci_hdi$CI_low,
               color = "royalblue",
               size = 3) +
    geom_vline(xintercept = ci_hdi$CI_high,
               color = "royalblue",
               size = 3)+
    xlab(label=xlabel)+
    ylab(label="Density")
}