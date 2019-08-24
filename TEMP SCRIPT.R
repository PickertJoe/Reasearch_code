library(ggplot2)
library(dplyr)
library(reshape)
library(chron)
setwd("~/Desktop/R_Scripts/Data/")


#This first part of this code was only needed once and should not be run again
Tempav <- read.csv("KonzaMet.csv")
Tempav$TAIR <- as.numeric(as.character(Tempav$TAIR))
MeanT <- summarise(group_by(Tempav, RECMONTH),
                   +                    mean(TAIR, na.rm=TRUE))

MeanT <- MeanT[-c(148), ]
colnames(MeanT) <- c("month", "year", "temperature")
AggT <-aggregate(MeanT$temperature, 
                 by=list(month=MeanT$month), mean, na.rm=TRUE)
st.err <- function(x) {sd(x)/sqrt(length(x))}
TSE <- aggregate(MeanT$temperature, 
                     by=list(month=MeanT$month), st.err)
AggT <- cbind(AggT, TSE[,2])
colnames(AggT) <- c("Month", "Climatic Mean", "SE")
write.csv(AggT, file="Tempagg.csv")

#This will run the identical code for the study period temperature
#Only rune this section of code if the temperature data has been updated
STempav <- read.csv("StudyTemp.csv")
STempav$date <- as.Date(with(STempav, 
                            paste(RECYEAR, RECMONTH, RECDAY, sep="-")),
                       "%Y-%m-%d")
STempav[,1:3]<- NULL
SNewTemp <- data.frame(date = STempav$date,
                      year = as.numeric(format(STempav$date, format = "%Y")),
                      month = as.numeric(format(STempav$date, format = "%m")),
                      day = as.numeric(format(STempav$date, format = "%d")),
                      Temperature= as.numeric(format(STempav$TAIR)),
                      na.rm=TRUE)
SMeanT <- summarise(group_by(SNewTemp, month, year), 
                   mean(Temperature, na.rm = TRUE))
SMeanT <- SMeanT[-c(148), ]
colnames(SMeanT) <- c("month", "year", "temperature")
SAggT <-aggregate(SMeanT$temperature, 
                 by=list(month=SMeanT$month), mean, na.rm=TRUE)
st.err <- function(x) {sd(x)/sqrt(length(x))}
STSE <- aggregate(SMeanT$temperature, 
                 by=list(month=SMeanT$month), st.err)
SAggT <- cbind(SAggT, TSE[,2])
colnames(SAggT) <- c("Month", "Study Period Mean", "StudySE")
write.csv(SAggT, file="STempagg.csv")

#Start here if temperature data was added
AggT <- read.csv("Tempagg.csv")
SAggT <-read.csv("STempagg.csv")
AggT <-cbind(AggT,SAggT)
AggT[,1]<-NULL
AggT[,4:5] <- NULL
AggT$M <- (c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
AggT$M <- factor(AggT$M, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
LowerT <- AggT$Climatic.Mean - AggT$SE
UpperT <- AggT$Climatic.Mean + AggT$SE
UpperS<- AggT$Study.Period.Mean + AggT$StudySE
LowerS<- AggT$Study.Period.Mean - AggT$StudySE
AggT[,1] <- NULL
AggT[,2]<- NULL
AggT[,3] <- NULL

AggT.long <- melt(AggT, id.vars="M")
TempG <- ggplot(data=AggT.long, aes(x=AggT.long$M, y=AggT.long$value, fill=variable)) + 
  
  labs(y="Mean Daily temperature (C)", x="Month")+
  geom_ribbon(aes(x=AggT$Month, ymin=LowerT, ymax=UpperT),fill="forestgreen", group=1) +
  #geom_line(aes( y =AggT$StudyTemp), color="black", size=2)+
  geom_bar(stat="identity", position="dodge")+
  #geom_errorbar(aes(ymin=AggT.long$value-LowerT, ymax=AggT.long$value+UpperT)
                #,width=.2)+
  scale_fill_manual(name="Time Period",
                      labels=c("1982-2016", "2016-2018"),
                    values=c("dodgerblue4", "red1"))+
  ggtitle("Mean Daily Temperatures for Study Period")+
  theme_bw()

print(TempG)
