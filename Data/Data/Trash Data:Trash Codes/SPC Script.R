library(data.table)
library(ggplot2)
setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
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
#pdf("SPC.pdf")
print(SPC)
#dev.off()