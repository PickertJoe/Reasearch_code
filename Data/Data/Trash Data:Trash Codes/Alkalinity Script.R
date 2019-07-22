library(data.table)
library(ggplot2)
setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
Alkalinity <- read.csv("Alkalinity.csv")
setDT(Alkalinity)
Alkalinity$Month <- factor(Alkalinity$Month, levels= c("Nov","Dec","Jan","Feb","Mar", "Apr"))
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
print(a)