load(file='./data/empirical_data/brms_acc~trial.rdata')
library(ggpubr)
mytheme=
  theme_pubclean()+
  theme(panel.border   = element_blank(), 
        axis.line      = element_line(color='gray'),
        text           = element_text(size=14,  family="serif"),
        axis.title     = element_text(size=14),
        legend.position= "right",
        plot.title     = element_text(hjust = 0.5))

library(ggplot2)
df |>
  group_by(condition, trl) |>
  summarise(acc=mean(acc)) |>
  ggplot(aes(x=trl,y=acc,color=condition))+geom_point()+mytheme+
  xlab('trial')+ylab('accuracy')
