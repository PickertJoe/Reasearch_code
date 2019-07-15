library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
Precip <- read.csv("KONZA.csv")
setDT(Precip)
Precip$Precipitation <-as.numeric(Precip$Precipitation)
Precip$Date <- as.Date(Precip$Date, format = "%m/%d/%Y")
NewPrecip <- data.frame(date = Precip$Date,
                        year = as.numeric(format(Precip$Date, format = "%Y")),
                        month = as.numeric(format(Precip$Date, format = "%m")),
                        day = as.numeric(format(Precip$Date, format = "%d")),
                        Precipitation= as.numeric(format(Precip$Precipitation)))
##Lines for contextualizing Gwen's data
NewKonza<-read.csv("NewKonza.csv", stringsAsFactors = FALSE, na.strings = ".")
NewKonza<- subset(NewKonza, watershed=="HQ")
NewKonza[2]<-NULL
NewKonza$RecDate<-as.POSIXct(NewKonza$RecDate,format="%m/%d/%Y")
NewKonza$ppt<-as.numeric(NewKonza$ppt)
NewKonzaAgg<- aggregate(NewKonza["ppt"], format(NewKonza["RecDate"], "%Y"), sum, na.rm=TRUE)
write.csv(NewKonzaAgg, file="YearlyPrecip.csv")

Et <- read.csv("KONZA_Et.csv")
setDT(Et)
Et$Et <-as.numeric(Et$Et)
Et$Date <- as.Date(Et$Date, format = "%m/%d/%Y")
NewEt <- data.frame(date = Et$Date,
                    year = as.numeric(format(Et$Date, format = "%Y")),
                    month = as.numeric(format(Et$Date, format = "%m")),
                    day = as.numeric(format(Et$Date, format = "%d")),
                    Evapotranspiration= as.numeric(format(Et$Et)))
LastPrecip <-read.csv("2016precip.csv")
LastPrecip$Precip <-as.numeric(LastPrecip$Precip)
LastPrecip$Date <- as.Date(LastPrecip$Date, format = "%m/%d/%Y")
NewLastPrecip <- data.frame(date = LastPrecip$Date,
                            year = as.numeric(format(LastPrecip$Date, format = "%Y")),
                            month = as.numeric(format(LastPrecip$Date, format = "%m")),
                            day = as.numeric(format(LastPrecip$Date, format = "%d")),
                            Precipitation= as.numeric(format(LastPrecip$Precip)))
PrecipSort <- summarise(group_by(NewPrecip, month, year), sum(Precipitation, na.rm = TRUE))
colnames(PrecipSort) <- c("Month", "Year", "Precipitation")
Precipagg <-aggregate(PrecipSort$Precipitation, by=list(month=PrecipSort$Month), mean)
st.err <- function(x) {sd(x)/sqrt(length(x))}
SE <- aggregate(PrecipSort$Precipitation, by=list(month=PrecipSort$Month), st.err)
EtSort <- summarise(group_by(NewEt, month, year), sum(Evapotranspiration, na.rm = TRUE))
colnames(EtSort) <- c("Month", "Year", "Evapotranspiration")
Etagg <-aggregate(EtSort$Evapotranspiration, by=list(month=EtSort$Month), mean)
EtSE <- aggregate(EtSort$Evapotranspiration, by=list(month=EtSort$Month), st.err)
Precipagg <- cbind(Precipagg, SE[,2], Etagg[,2], EtSE[,2])
setDT(Precipagg)
colnames(Precipagg) <- c("Month", "Precipitation", "SE", "Evapotranspiration", "EtSE")
#Precipagg$Month <- as.character(Precipagg$Month)
#Precipagg$Month <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
p <- ggplot(data=Precipagg, aes(x=Month, y=Precipitation), legend=TRUE) + 

  labs(y="Mean Precipitation/Evapotranspiration (mm)")+
  geom_ribbon(aes(ymin=Precipitation-SE, ymax=Precipitation+SE),fill="steelblue2") +
  geom_ribbon(aes(ymin=Evapotranspiration-EtSE, ymax=Evapotranspiration+EtSE), fill="firebrick3")+
  geom_line(aes( y = Precipitation), color="black", legend=TRUE)+
  geom_line(aes(y=Evapotranspiration), color="white")+
  ggtitle("Precipitation and Et at the KONZA LTER (2016)")+
  theme_bw()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_fill_identity(name="The color", guide='legend', values=c('firebrick3'='firebrick3','steelblue2'='steeblue2'), labels=c("Et","Precipitation"))
  #geom_errorbar(aes(ymin=Precipitation-SE, ymax=Precipitation+SE), width=.2, color="blue")
print(p)