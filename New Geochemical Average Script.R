#This script graphs all geochemical data from master data file
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd("~/Desktop/R_Scripts/Data")
master <- read.csv("MasterData.csv")
master$Month = factor(master$Month, levels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
wellname <- c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8")

#Plots the monthly mean(+sd) concentrations of Dissolved Oxygen
DOA<-ggplot(master, aes(x=master$Month, y=master$DO 
  ))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="Dissolved Oxygen(%)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average DO Values")

#Plots the monthly mean(+sd) concentrations of Nitrate
NO3A<-ggplot(master, aes(x=master$Month, y=master$NO3 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[NO3-N] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average NO3 Concentrations")

#Plots the monthly mean(+sd) concentrations of Sulfate
SO4A<-ggplot(master, aes(x=master$Month, y=master$SO4 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[SO4] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average SO4 Concentrations")


#Plots the monthly mean(+sd) concentrations of Chloride
ClA<-ggplot(master, aes(x=master$Month, y=master$Cl 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[Cl] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average Cl Concentrations")

#Plots the monthly mean(+sd) water temperatures
TempA<-ggplot(master, aes(x=master$Month, y=master$Temperature
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="Water Temperature (C)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average Water Temperatures")

#Plots the monthly mean(+sd) pH values
pHA<-ggplot(master, aes(x=master$Month, y=master$pH 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="Well Water pH")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average pH Values")

#Plots the monthly mean(+sd) specific conductivity values
SPCA<-ggplot(master, aes(x=master$Month, y=master$Conductivity 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="Specific Conductivity (us/cm))")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average SPC Values")

#Plots the monthly mean(+sd) alkalinity concentrations
AlkA<-ggplot(master, aes(x=master$Month, y=master$Alkalinity 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[HCO3] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average Water Alkalinity")

#Plots the monthly mean(+sd) concentrations of Calcium
CaA<-ggplot(master, aes(x=master$Month, y=master$Ca 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[Ca] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average Ca Concentrations")

#Plots the monthly mean(+sd) concentrations of Potassium
KA<-ggplot(master, aes(x=master$Month, y=master$K 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[K] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average K Concentrations")

#Plots the monthly mean(+sd) concentrations of Magnesium
MgA<-ggplot(master, aes(x=master$Month, y=master$Mg 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[Ng] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average Mg Concentrations")

#Plots the monthly mean(+sd) concentrations of Sodium
NaA<-ggplot(master, aes(x=master$Month, y=master$Na 
))+ 
  theme_bw()+
  stat_summary(fun.y = mean, geom="line",color="red",size=1, aes(group=1))+
  stat_summary(fun.data = mean_se, geom="ribbon",fill="black", color="black")+
  labs(y="[Na] (mg/L)")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Average Na Concentrations")

anion_average<- ggarrange(NO3A, SO4A, ClA, AlkA, 
          common.legend = TRUE, legend="bottom",
          ncol=2, nrow=2)

cation_average<- ggarrange(CaA, KA, MgA, NaA, 
          common.legend = TRUE, legend="bottom",
          ncol=2, nrow=2)

fieldpar_average<- ggarrange(DOA, pHA, SPCA, TempA,
          common.legend = TRUE, legend="bottom",
          ncol=2, nrow=2)

#These lines will save pdfs of the figure arrays
setwd("~/Desktop/R_Scripts/Figures")
pdf("Anion_Average.pdf")
print(anion_average)
dev.off()

pdf("Cation_Average.pdf")
print(cation_average)
dev.off()

pdf("Parameter_Average.pdf")
print(fieldpar_average)
dev.off()