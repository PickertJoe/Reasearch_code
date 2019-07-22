library(data.table)
library(ggplot2)
setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
NO3 <- read.csv("NO3.csv")
setDT(NO3)
NO3$Month <- factor(NO3$Month, levels= c("Nov","Dec","Jan","Feb","Mar"))
n<- ggplot(NO3, aes(x=Month, y=Conc,  color=well))+
  labs(y="Nitrate Concentration(mgNO3/L)")+
  geom_line(aes(y=Well1),size=1.5, color="black")+
  geom_line(aes(y=Well2),size=1.5, color="dodgerblue4")+
  geom_line(aes(y=Well3),size=1.5, color="cyan1")+
  geom_line(aes(y=Well4),size=1.5, color="forestgreen")+
  geom_line(aes(y=Well5),size=1.5, color="darkmagenta")+
  geom_line(aes(y=Well6),size=1.5, color="red1")+
  geom_line(aes(y=Well7),size=1.5, color="deeppink1")+
  geom_line(aes(y=Well8),size=1.5, color="darkgoldenrod4")+
  ggtitle("Monthly Nitrate Concentrations")+
  guide_legend()+

  theme_bw()
print(n)