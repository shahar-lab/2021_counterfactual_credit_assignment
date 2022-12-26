my_posteriorplot<-function(model,variable_name,xlabel,mycolor="gray",myCI){
  library(ggplot2)
  library(insight)
  library(bayestestR)
  params=insight::get_parameters(model)
  variable=params[,paste0("b_",variable_name)]
  ggplot(data.frame(x=variable),aes(x=variable))+geom_density(alpha = .5,fill=mycolor)+
    geom_vline(xintercept = median(variable), linetype="dotted",color = "blue", size=1.5)+
    geom_segment(aes(x = bayestestR::hdi(variable, ci = myCI)$CI_low, y = 0, xend = bayestestR::hdi(variable, ci = myCI)$CI_high, yend = 0),color="darkgray",size=2,show.legend = F)+
    xlab(xlabel)+ ylab("Density") + theme_classic()
}