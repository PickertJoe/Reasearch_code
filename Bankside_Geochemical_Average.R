#This script plots the average values for each constituent group by which
#Bank of the stream the wells where located

DOAB<-ggplot(master, aes(x=Month, y=DO, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",size=1)+
  stat_summary(fun.data = mean_se, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  labs(y="Dissolved OxygMonth")+
  ggtitle("Average DO Values")
