library(ggplot2)
setwd("~/R_Scripts/Data/")
SPrecip <- read.csv("Precipagg.csv")
SPrecip$Month <- factor(SPrecip$Month, levels=c("Oct '16", "Nov '16","Dec '16","Jan '17","Feb '17","Mar '17", "Apr '17", "May '17", "Jun '17", "Jul '17", "Aug '17", "Sep '17","Oct '17", "Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18"))

Lower <- SPrecip$MPrecip - SPrecip$SE
Upper <- SPrecip$MPrecip + SPrecip$SE

PrecipG <- ggplot(data=SPrecip, aes(x=SPrecip$Month, y=SPrecip$MPrecip, group=1)) + 
  geom_errorbar(aes(x=SPrecip$Month, ymin=Lower, ymax=Upper,color="red"), alpha=0.9) +
  geom_line(aes(y =SPrecip$MPrecip, color="red"), size = 1)+
  geom_point(stat="identity", aes(y=SPrecip$APrecip, color="blue"), size=3)+
  ggtitle("Mean vs Observed Precipitation (WYs 2017 & 2018)")+
  scale_x_discrete(breaks=c("Oct '16","Jan '17","Apr '17","Jul '17","Oct '17", "Jan '18", "Apr '18", "Jul '18", "Sep '18"),
                   labels=c("Oct","Jan '17","Apr","Jul","Oct", "Jan '18", "Apr", "Jul", "Sep"))+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Observed Precpitation", "Mean Precipitation ± SD"))+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(y="Precipitation (mm)", x="Month", colour="Series")+
  theme(legend.position = 'bottom')

PrecipG

setwd("~/R_Scripts/Figures/")
pdf("Study_Precip.pdf")
print(PrecipG)
dev.off()