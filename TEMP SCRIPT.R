library(ggplot2)
library(dplyr)
library(reshape)
library(chron)
setwd("~/Desktop/R_Scripts/Data/")


#This first part of this code isolates the mean monthly temperatures
#And their respective standard deviations for the KONZA climatic record
Tempav <- read.csv("KonzaMet.csv")
Tempav$TAIR <- as.numeric(as.character(Tempav$TAIR))
MeanT <- summarise(group_by(Tempav, RECMONTH), mean(TAIR, na.rm=TRUE), sd(TAIR, na.rm = TRUE))
colnames(MeanT) <- c("Month", "MeanT", "SeT")

#Creating a subset of just the study data
StudyTemp = subset(Tempav, RECYEAR==2018, select=c(RECMONTH, RECDAY, TAIR))
MeanStudy = summarise(group_by(StudyTemp, RECMONTH), mean(TAIR, na.rm=TRUE), sd(TAIR, na.rm = TRUE))
colnames(MeanStudy) = c("Month", "Mean", "SE")

TempG <- ggplot() + 
  
  labs(y="Mean Daily temperature (C)", x="Month")+
  geom_line(data=MeanT, aes(x=factor(Month), y=MeanT, color='Climatic Mean'), size=1.3, group=1)+
  geom_ribbon(data=MeanT, aes(x=factor(Month), ymin=MeanT-SeT, ymax=MeanT+SeT, fill='Climatic Standard Error'), alpha=0.3, group=1)+
  geom_point(data=MeanStudy, aes(x=factor(Month), y=Mean, color='2018 Mean'), size=3)+
  geom_errorbar(data=MeanStudy, aes(x=factor(Month), ymin=Mean-SE, ymax=Mean+SE, color='2018 Standard Error'))+
  scale_color_manual(values = c("Climatic Mean" = "dodgerblue4", 
                                "2018 Mean" = "red", "2018 Standard Error" = "red"))+
  scale_fill_manual(values = c("Climatic Standard Error" = "cyan"))+
  scale_x_discrete(breaks=c('1','2','3','4','5','6','7','8','9','10','11','12'),
                   labels=c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  #scale_color_discrete()+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position ="bottom")+
  ggtitle("Mean Temperatures for Study Period vs Climatic Average")+
  theme_bw()

print(TempG)


setwd("~/Desktop/R_Scripts/Figures")
pdf("Study_Temperature.pdf")
print(TempG)
dev.off()

