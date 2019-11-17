#This script plots the plots the saturation index data in three formats:
# 1) Averaged across the entire system
# 2) Averaged by bankside
# 3) Individually for each well
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd("~/R_Scripts/Data")
master <- read.csv("MasterData.csv")
master$Month = factor(master$Month, levels=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"))


########################################
####BEGIN BANKSIDE CODE BLOCK###########
########################################

AragBA<-ggplot(master, aes(x=Month, y=Aragonite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",size=1)+
  stat_summary(fun.data = mean_se, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Aragonite Saturation Index")+
  ggtitle("Bankside Aragonite Saturation Indices")

CalciteBA<-ggplot(master, aes(x=Month, y=Calcite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",size=1)+
  stat_summary(fun.data = mean_se, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Calcite Saturation Index")+
  ggtitle("Bankside Calcite Saturation Indices")

Co2BA<-ggplot(master, aes(x=Month, y=CO2, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",size=1)+
  stat_summary(fun.data = mean_se, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="log-CO2 Saturation Index")+
  ggtitle("Bankside CO2 Saturation Indices")

DolBA<-ggplot(master, aes(x=Month, y=Dolomite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",size=1)+
  stat_summary(fun.data = mean_se, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Dolomite Saturation Index")+
  ggtitle("Bankside Dolomite Saturation Indices")

si_bankside<- ggarrange(AragBA, Co2BA, CalciteBA, DolBA, 
                           common.legend = TRUE, legend="bottom",
                           ncol=2, nrow=2)

########################################
####END BANKSIDE CODE BLOCK#############
########################################


########################################
####BEGIN SYSTEM-WIDE CODE BLOCK########
########################################

AragA<-ggplot(master, aes(x=Month, y=Aragonite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="pointrange", color='red', aes(group=1))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Aragonite Saturation Index")+
  ggtitle("A")

CalciteA<-ggplot(master, aes(x=Month, y=Calcite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="pointrange", color='red', aes(group=1))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Calcite Saturation Index")+
  ggtitle("C")

Co2A<-ggplot(master, aes(x=Month, y=CO2, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="pointrange", color='red', aes(group=1))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="log-CO2 Saturation Index")+
  ggtitle("D")

DolA<-ggplot(master, aes(x=Month, y=Dolomite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="pointrange", color='red', aes(group=1))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Dolomite Saturation Index")+
  ggtitle("B")

si_average<- ggarrange(AragA, DolA, CalciteA, Co2A, 
                       common.legend = TRUE, legend="bottom",
                       ncol=2, nrow=2)

########################################
####END SYSTEM-WIDE CODE BLOCK##########
########################################

########################################
####BEGIN INDIVIDUAL CODE BLOCK#########
########################################

Arag<-ggplot(master, aes(x=Month, y=Aragonite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  geom_point(aes(shape=Well))+
  geom_line(size=.5)+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Aragonite Saturation Index", shape="Well Number")+
  scale_shape_manual(values=c(0,1,2,5,0,1,2,5))+
  ggtitle("Aragonite Saturation Indices")

Calcite<-ggplot(master, aes(x=Month, y=Calcite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  geom_point(aes(shape=Well))+
  geom_line(size=.5)+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Calcite Saturation Index", shape="Well Number")+
  scale_shape_manual(values=c(0,1,2,5,0,1,2,5))+
  ggtitle("Calcite Saturation Indices")

Co2<-ggplot(master, aes(x=Month, y=CO2, color=Bank, group=Bank 
))+ 
  theme_bw()+
  geom_point(aes(shape=Well))+
  geom_line(size=.5)+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="log-CO2 Saturation Index", shape="Well Number")+
  scale_shape_manual(values=c(0,1,2,5,0,1,2,5))+
  ggtitle("CO2 Saturation Indices")

Dol<-ggplot(master, aes(x=Month, y=Dolomite, color=Bank, group=Bank 
))+ 
  theme_bw()+
  geom_point(aes(shape=Well))+
  geom_line(size=.5)+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="Dolomite Saturation Index", shape="Well Number")+
  scale_shape_manual(values=c(0,1,2,5,0,1,2,5))+
  ggtitle("Dolomite Saturation Indices")

si_summary<- ggarrange(Arag, Co2, Calcite, Dol, 
                       common.legend = TRUE, legend="bottom",
                       ncol=2, nrow=2)

########################################
####END INDIVIDUAL CODE BLOCK###########
########################################

# This section of code prints each of the figure arrays to pdf files
setwd("~/R_Scripts/Figures/")

pdf("SI_Bankside.pdf")
print(si_bankside)
dev.off()

pdf("SI_Summary.pdf")
print(si_summary)
dev.off()

pdf("SI_Average.pdf")
print(si_average)
dev.off()