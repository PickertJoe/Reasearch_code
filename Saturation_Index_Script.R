#This script plots the plots the saturation index data in three formats:
# 1) Averaged across the entire system
# 2) Averaged by bankside
# 3) Individually for each well
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd("~/Desktop/R_Scripts/Data")
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

########################################
####END SYSTEM-WIDE CODE BLOCK##########
########################################

########################################
####BEGIN INDIVIDUAL CODE BLOCK#########
########################################

########################################
####END INDIVIDUAL CODE BLOCK###########
########################################

# This section of code prints each of the figure arrays to pdf files
setwd("~/Desktop/R_Scripts/Figures")
pdf("SI_Bankside.pdf")
print(si_bankside)
dev.off()