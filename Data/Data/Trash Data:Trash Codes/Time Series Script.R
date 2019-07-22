library(data.table)
library(ggplot2)
setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
NO3 <- read.csv("NO3.csv")
setDT(NO3)
NO3$Month <- factor(NO3$Month, levels= c("Nov","Dec","Jan","Feb","Mar"))
NO3.long <- melt(NO3, id="Month", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
n<- ggplot(NO3.long, aes(x=Month, y=value, group=variable, color=variable))+
  geom_line(aes(linetype=variable), size=1.5)+
  scale_linetype_manual(values=c("solid", "longdash", "longdash", "solid", "longdash", "solid", "solid", "longdash"), guide=FALSE)+
  geom_point(size=3)+
  theme_bw()+
  labs(y="Nitrate Concentration(mgNO3/L)", color="Well Number")+
  ggtitle("NEON Well Nitrate Concentrations")+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

Cl <- read.csv("Cl.csv")
setDT(Cl)
Cl$Month <- factor(Cl$Month, levels= c("Nov","Dec","Jan","Feb","Mar"))
Cl.long <- melt(Cl, id="Month", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
c<- ggplot(Cl.long, aes(x=Month, y=value, group=variable, color=variable))+
  geom_line(aes(linetype=variable), size=1.5)+
  scale_linetype_manual(values=c("solid", "longdash", "longdash", "solid", "longdash", "solid", "solid", "longdash"), guide=FALSE)+
  geom_point(size=3)+
  theme_bw()+
  labs(y="Chloride Concentration(mgCl/L)", color="Well Number")+
  ggtitle("NEON Well Chloride Concentrations")+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

SO4 <- read.csv("SO4.csv")
setDT(SO4)
SO4$Month <- factor(SO4$Month, levels= c("Nov","Dec","Jan","Feb","Mar"))
SO4.long <- melt(SO4, id="Month", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
s<- ggplot(SO4.long, aes(x=Month, y=value, group=variable, color=variable))+
  geom_line(aes(linetype=variable), size=1.5)+
  scale_linetype_manual(values=c("solid", "longdash", "longdash", "solid", "longdash", "solid", "solid", "longdash"), guide=FALSE)+
  geom_point(size=3)+
  theme_bw()+
  labs(y="Sulfate Concentration(mgSO4/L)", color="Well Number")+
  ggtitle("NEON Well Sulfate Concentrations")+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

SPC <- read.csv("SPC.csv")
setDT(SPC)
SPC$Month <- factor(SPC$Month, levels= c("Nov","Dec","Jan","Feb","Mar", "Apr", "May"))
SPC.long <- melt(SPC, id="Month", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
SPC<- ggplot(SPC.long, aes(x=Month, y=value, group=variable, color=variable))+
  geom_line(aes(linetype=variable), size=1.5)+
  scale_linetype_manual(values=c("solid", "longdash", "longdash", "solid", "longdash", "solid", "solid", "longdash"), guide=FALSE)+
  geom_point(size=3)+
  theme_bw()+
  labs(y="Specific Conductance (us/cm)", color="Well Number")+
  ggtitle("NEON Well Specific Conductance Values")+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

DO <- read.csv("DO.csv")
setDT(DO)
DO$Month <- factor(DO$Month, levels= c("Nov","Dec","Jan","Feb","Mar", "Apr", "May"))
DO.long <- melt(DO, id="Month", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
DO<- ggplot(DO.long, aes(x=Month, y=value, group=variable, color=variable))+
  geom_line(aes(linetype=variable), size=1.5)+
  scale_linetype_manual(values=c("solid", "longdash", "longdash", "solid", "longdash", "solid", "solid", "longdash"), guide=FALSE)+
  geom_point(size=3)+
  theme_bw()+
  labs(y="Dissolved Oxygen (%)", color="Well Number")+
  ggtitle("NEON Well Dissolved Oxygen Values")+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

pH <- read.csv("pH.csv")
setDT(pH)
pH$Month <- factor(pH$Month, levels= c("Nov","Dec","Jan","Feb","Mar", "Apr", "May"))
pH.long <- melt(pH, id="Month", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
pHp<- ggplot(pH.long, aes(x=Month, y=value, group=variable, color=variable))+
  geom_line(aes(linetype=variable), size=1.5)+
  scale_linetype_manual(values=c("solid", "longdash", "longdash", "solid", "longdash", "solid", "solid", "longdash"), guide=FALSE)+
  geom_point(size=3)+
  theme_bw()+
  labs(y="pH Readings", color="Well Number")+
  ggtitle("NEON Well pH Values")+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

Alkalinity <- read.csv("Alkalinity.csv")
setDT(Alkalinity)
Alkalinity$Month <- factor(Alkalinity$Month, levels= c("Nov","Dec","Jan","Feb","Mar", "Apr", "May"))
Alkalinity.long <- melt(Alkalinity, id="Month", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
a<- ggplot(Alkalinity.long, aes(x=Month, y=value, group=variable, color=variable))+
  geom_line(size=1.5, na.rm = TRUE)+
  theme_bw()+
  labs(y="Alkalinity (mgCaCO3/L)", color="Well Number")+
  ggtitle("Monthly Alkalinity Readings")+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

ggarrange(n, c, s, SPC, DO, pHp, a,
          labels= c("A", "B", "C", "D", "E", "F"),
          common.legend = TRUE, legend="bottom",
          ncol=2, nrow=4)