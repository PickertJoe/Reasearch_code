library(data.table)
library(ggplot2)
setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
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
#pdf("pH.pdf")
print(pHp)
#dev.off()