#This script plots the average values for each constituent group by which
#Bank of the stream the wells where located
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd("~/R_Scripts/Data")
master <- read.csv("MasterData.csv")
master$Month = factor(master$Month, levels=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"))

#This custom function is used to calculate the mean & se log values for pH
f1 <- function(x){
  log10(mean(10^x))
}

f2 <- function(x){
  log10(mean_se(10^x))
}

#Plots the mean monthly dissolved oxygen values by bankside
DOAB<-ggplot(master, aes(x=Month, y=DO, color=Bank, group=Bank 
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
  labs(y="Dissolved Oxygen (ppm)")+
  ggtitle("Mean Bankside DO Values")

#Plots the mean monthly specific conductivity values by bankside
SPCAB<-ggplot(master, aes(x=Month, y=Conductivity, color=Bank, group=Bank 
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
  labs(y="Specific Conductivity(us/cm)")+
  ggtitle("Mean Bankside SPC Values")

#Plots the mean monthly water temperature values by bankside
TempAB<-ggplot(master, aes(x=Month, y=Temperature, color=Bank, group=Bank 
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
  labs(y="Water Temperature (C)")+
  ggtitle("Mean Bankside Water Temperature")

#Plots the mean monthly pH values by bankside
pHAB<-ggplot(master, aes(x=Month, y=pH, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = f1, geom="line",size=1)+
  stat_summary(fun.data = f2, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="pH Values")+
  ggtitle("Mean Bankside pH Values")

#Plots the mean monthly alkalinity values by bankside
AlkAB<-ggplot(master, aes(x=Month, y=Alkalinity, color=Bank, group=Bank 
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
  labs(y="[CaCO3] (mg/L)")+
  ggtitle("Mean Bankside Alkalinity")

#Plots the mean monthly nitrate values by bankside
NO3AB<-ggplot(master, aes(x=Month, y=NO3, color=Bank, group=Bank 
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
  labs(y="[NO3-N] (mg/L)")+
  annotate("text", x=3, y=0.2, label="Detection= 0.02")+
  annotate("segment", x=1, y=0, xend=12, yend=0)+
  ggtitle("Mean Bankside NO3 Values")

#Plots the mean monthly sulfate values by bankside
SO4AB<-ggplot(master, aes(x=Month, y=SO4, color=Bank, group=Bank 
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
  labs(y="[SO4] (mg/L)")+
  ggtitle("Mean Bankside SO4 Values")

#Plots the mean monthly chloride values by bankside
ClAB<-ggplot(master, aes(x=Month, y=Cl, color=Bank, group=Bank 
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
  labs(y="[Cl] (mg/L)")+
  ggtitle("Mean Bankside Cl Values")

#Plots the mean monthly calcium values by bankside
CaAB<-ggplot(master, aes(x=Month, y=Ca, color=Bank, group=Bank 
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
  labs(y="[Ca] (mg/L)")+
  ggtitle("Mean Bankside Ca Values")

#Plots the mean monthly sodium values by bankside
NaAB<-ggplot(master, aes(x=Month, y=Na, color=Bank, group=Bank 
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
  labs(y="[Na] (mg/L)")+
  ggtitle("Mean Bankside Na Values")

#Plots the mean monthly magnesium values by bankside
MgAB<-ggplot(master, aes(x=Month, y=Mg, color=Bank, group=Bank 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",size=1, na.rm = FALSE)+
  stat_summary(fun.data = mean_se, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  labs(y="[Mg] (mg/L)")+
  ggtitle("Mean Bankside Mg Values")

#Plots the mean monthly potassium values by bankside
KAB<-ggplot(master, aes(x=Month, y=K, color=Bank, group=Bank 
))+ 
  theme_bw()+
  ggtitle("Mean Bankside K Values")+
  theme(plot.title = element_text(hjust=0.5))+
  stat_summary(fun.y = mean, geom="line",size=1, na.rm=FALSE)+
  stat_summary(fun.data = mean_se, geom="pointrange")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_x_discrete(breaks=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"),
                   labels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Jan"))
  theme(legend.position = 'bottom')+
  theme(axis.title.x = element_blank())+
  labs(y="[K] (mg/L)")
  

#Creating figure arrays from the above figures
anion_bankside<- ggarrange(NO3AB, SO4AB, ClAB, AlkAB, 
                          common.legend = TRUE, legend="bottom",
                          ncol=2, nrow=2)

cation_bankside<- ggarrange(CaAB, KAB, MgAB, NaAB, 
                           common.legend = TRUE, legend="bottom",
                           ncol=2, nrow=2)

fieldpar_bankside<- ggarrange(DOAB, pHAB, SPCAB, TempAB,
                             common.legend = TRUE, legend="bottom",
                             ncol=2, nrow=2)

microbe_bankside <- ggarrange(NO3AB, pHAB, DOAB, KAB,
                              common.legend=TRUE, legend='bottom',
                              ncol=1, nrow=4)

#These lines will save pdfs of the figure arrays
setwd("~/R_Scripts/Figures")
pdf("Anion_Bankside.pdf")
print(anion_bankside)
dev.off()

pdf("Cation_Bankside.pdf")
print(cation_bankside)
dev.off()

pdf("Parameter_Bankside.pdf")
print(fieldpar_bankside)
dev.off()

pdf("Microbial_Activity.pdf")
print(microbe_bankside)
dev.off()
