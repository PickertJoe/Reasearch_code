library(ggplot2)
setwd("~/R_Scripts/Data/")
SPrecip <- read.csv("Precipagg.csv")
SPrecip$Month <- factor(SPrecip$Month, levels=c("Oct '16", "Nov '16","Dec '16","Jan '17","Feb '17","Mar '17", "Apr '17", "May '17", "Jun '17", "Jul '17", "Aug '17", "Sep '17","Oct '17", "Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18"))

Lower <- SPrecip$MPrecip - SPrecip$SE
Upper <- SPrecip$MPrecip + SPrecip$SE

PrecipG <- ggplot(data=SPrecip, aes(x=SPrecip$Month, y=SPrecip$MPrecip, group=1)) + 

  labs(y="Precipitation (mm)", x="Month")+
  geom_ribbon(aes(x=SPrecip$Month, ymin=Lower, ymax=Upper),fill="forestgreen", alpha=0.9) +
  geom_line(aes( y =SPrecip$MPrecip), color="black")+
  geom_point(stat="identity", aes(y=SPrecip$APrecip), color="blue", size=3)+
  ggtitle("Mean vs Observed Precipitation (WYs 2017 & 2018)")+
  theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("forest green", "navy"),
                    labels=c("Mean Precipitation", "Observed Precipitation"))+
  theme_bw()

PrecipG

setwd("~/R_Scripts/Figures")
pdf("Study_Precip.pdf")
print(PrecipG)
dev.off()