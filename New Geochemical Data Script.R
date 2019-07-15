#This script graphs all geochemical data from master data file
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
master <- read.csv("MasterData.csv")
master$Month = factor(master$Month, levels=c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
##Below is the original color scheme
##c(Well1="black",
  #Well2="dodgerblue4",
 # Well3="cyan1",
  #Well4="forestgreen",
  #Well5="darkmagenta",
  #Well6="red1",
  #Well7="deeppink1",
  #Well8="darkgoldenrod4"))

DO<-ggplot(master, aes(x=master$Month, y=master$DO, 
  group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="Dissolved Oxygen (%)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well DO Values")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
                                ))+
  scale_shape_manual(values=c(0,1,2,5,0,1,2,5))

NO3<-ggplot(master, aes(x=master$Month, y=master$NO3, 
                       group=master$Well, color=master$Bank))+ 
  geom_point(aes( shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="[NO3-N] (mg/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Nitrate Concentrations")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

SO4<-ggplot(master, aes(x=master$Month, y=master$SO4, 
                       group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="[SO4] (mg/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well SO4 Concentrations")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

Cl<-ggplot(master, aes(x=master$Month, y=master$Cl, 
                       group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="[Cl] (mg/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Chloride Concentrations")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

Temp<-ggplot(master, aes(x=master$Month, y=master$Temperature, 
                       group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="Water Temperature (C)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Water Temperatures")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

pH<-ggplot(master, aes(x=master$Month, y=master$pH, 
                       group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="Water pH", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well pH Values")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

SPC<-ggplot(master, aes(x=master$Month, y=master$Conductivity, 
                       group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="Specific Conductance (us/cm)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well SPC Values")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

Alk<-ggplot(master, aes(x=master$Month, y=master$Alkalinity, 
                       group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="Alkalinity (mgCO3/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Alkalinity")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

Ca<-ggplot(master, aes(x=master$Month, y=master$Ca, 
                        group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="[Ca] (mg/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Calcium Concentrations")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

K<-ggplot(master, aes(x=master$Month, y=master$K, 
                                                                         group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="[K] (mg/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Potassium Concentrations")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))


Mg<-ggplot(master, aes(x=master$Month, y=master$Mg, 
                        group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="[Mg] (mg/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Magensium Concentrations")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

Na<-ggplot(master, aes(x=master$Month, y=master$Na, 
                        group=master$Well, color=master$Bank))+ 
  geom_point(aes(shape=master$Well))+
  theme_bw()+
  geom_line(size=.5)+
  labs(y="[Na] (mg/L)", x="Month", color="Bank Side", shape="Well Number")+
  ggtitle("NEON Well Sodium Concentrations")+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"
  ))+
  scale_shape_manual(values=c(0,1,2,5,6,8,11,14))

#This portion of code creates figure arrays for all pertinent anions, 
#cations, and field parameters

anions<- ggarrange(NO3, SO4, Cl, Alk, 
          labels= c("A", "B", "C", "D"),
          common.legend = TRUE, legend="bottom",
          ncol=2, nrow=2)

cations<- ggarrange(Ca, K, Mg, Na, 
                   labels= c("A", "B", "C", "D"),
                   common.legend = TRUE, legend="bottom",
                   ncol=2, nrow=2)

fieldpar<- ggarrange(DO, pH, SPC,
          labels= c("A", "B", "C"),
          common.legend = TRUE, legend="bottom",
          ncol=2, nrow=2)
