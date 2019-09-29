library(ggplot2)
setwd("~/Desktop/R_Scripts/Data/")
SPrecip <- read.csv("Precipagg.csv")
SPrecip$Month <- factor(SPrecip$Month, levels=c("Oct","Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))

Lower <- SPrecip$MPrecip - SPrecip$SE
Upper <- SPrecip$MPrecip + SPrecip$SE

PrecipG <- ggplot(data=SPrecip, aes(x=SPrecip$Month, y=SPrecip$MPrecip, group=1)) + 

  labs(y="Precipitation (mm)", x="Month")+
  geom_ribbon(aes(x=SPrecip$Month, ymin=Lower, ymax=Upper),fill="forestgreen", alpha=0.9) +
  geom_line(aes( y =SPrecip$MPrecip), color="black")+
  geom_point(stat="identity", aes(y=SPrecip$APrecip), color="blue", size=3)+
  ggtitle("Study Precip vs Climatic Average")+
  scale_fill_manual(values=c("forest green", "navy"),
                    labels=c("Mean Precipitation", "Observed Precipitation"))+
  theme_bw()

PrecipG

setwd("~/Desktop/R_Scripts/Figures")
pdf("Study_Precip.pdf")
print(PrecipG)
dev.off()