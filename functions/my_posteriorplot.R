my_posteriorplot<-function(x,myxlabel,mycolor,myxlim,my_vline){
  
  
  ggplot(data.frame(x=x),aes(x=x))+geom_density(alpha = .5,fill=mycolor)+ 
    geom_vline(xintercept = my_vline, linetype="dotted",color = "blue", size=1.5)+
    geom_segment(aes(x = hdi(x, ci = 0.95)$CI_low, y = 0, xend = x, yend = 0),color="darkgray",size=2,show.legend = F)+
    xlim(myxlim[1],myxlim[2])+ xlab(myxlabel)+ theme_classic()
  
}