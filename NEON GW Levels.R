library(data.table)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)

setwd("~/Dropbox/EVRN 624 Files/Data/GWData/Raw/Neon_elev-groundwater/5_min_data")

##**** SUPER IMPORTANT the headers in all the fils need to be the same for this to work 
files1 = list.files(pattern=c(".301.","*.csv")) # hear what we are doing is make a list that contains all CSV files 
files1<-Filter(function(x) grepl("_5_min", x), files1) #this code will let you then select only the files with 5 min data 
well1 = do.call(rbind, lapply(files1, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))# if this fails it measn that the first three columns don't have the same column names 

files2 = list.files(pattern=c(".302.","*.csv")) 
files2<-Filter(function(x) grepl("_5_min", x), files2) 
well2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))

files3 = list.files(pattern=c(".303.","*.csv")) 
files3<-Filter(function(x) grepl("_5_min", x), files3) 
well3 = do.call(rbind, lapply(files3, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))

files4 = list.files(pattern=c(".304.","*.csv")) 
files4<-Filter(function(x) grepl("_5_min", x), files4) 
well4 = do.call(rbind, lapply(files4, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))

files5 = list.files(pattern=c(".305.","*.csv")) 
files5<-Filter(function(x) grepl("_5_min", x), files5) 
well5 = do.call(rbind, lapply(files5, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))

files6 = list.files(pattern=c(".306.","*.csv")) 
files6<-Filter(function(x) grepl("_5_min", x), files6) 
well6 = do.call(rbind, lapply(files6, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))

files7 = list.files(pattern=c(".307.","*.csv")) 
files7<-Filter(function(x) grepl("_5_min", x), files7) 
well7 = do.call(rbind, lapply(files7, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))

files8 = list.files(pattern=c(".308.","*.csv")) 
files8<-Filter(function(x) grepl("_5_min", x), files8) 
well8 = do.call(rbind, lapply(files8, function(x) read.csv(x, stringsAsFactors = FALSE)[,1:3]))

well1[2]<-NULL
well2[2]<-NULL
well3[2]<-NULL
well4[2]<-NULL
well5[2]<-NULL
well6[2]<-NULL
well7[2]<-NULL
well8[2]<-NULL

#now you need to add date and time with Posixct
well1$startDateTime<-as.POSIXct(well1$startDateTime,format="%Y-%m-%dT%H:%M")
well2$startDateTime<-as.POSIXct(well2$startDateTime,format="%Y-%m-%dT%H:%M")
well3$startDateTime<-as.POSIXct(well3$startDateTime,format="%Y-%m-%dT%H:%M")
well4$startDateTime<-as.POSIXct(well4$startDateTime,format="%Y-%m-%dT%H:%M")
well5$startDateTime<-as.POSIXct(well5$startDateTime,format="%Y-%m-%dT%H:%M")
well6$startDateTime<-as.POSIXct(well6$startDateTime,format="%Y-%m-%dT%H:%M")
well7$startDateTime<-as.POSIXct(well7$startDateTime,format="%Y-%m-%dT%H:%M")
well8$startDateTime<-as.POSIXct(well8$startDateTime,format="%Y-%m-%dT%H:%M")

#Use these lines for daily averages (can be modified for hourly)
w1.H.ave<-aggregate(well1["groundwaterElev"], format(well1["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w2.H.ave<-aggregate(well2["groundwaterElev"], format(well2["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w3.H.ave<-aggregate(well3["groundwaterElev"], format(well3["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w4.H.ave<-aggregate(well4["groundwaterElev"], format(well4["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w5.H.ave<-aggregate(well5["groundwaterElev"], format(well5["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w6.H.ave<-aggregate(well6["groundwaterElev"], format(well6["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w7.H.ave<-aggregate(well7["groundwaterElev"], format(well7["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE) 
w8.H.ave<-aggregate(well8["groundwaterElev"], format(well8["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)



newTotal<- merge(w1.H.ave,w2.H.ave, by="startDateTime", all=T)
newTotal<- merge(newTotal,w3.H.ave, by="startDateTime", all=T)
newTotal<- merge(newTotal,w4.H.ave, by="startDateTime", all=T)
newTotal<- merge(newTotal,w5.H.ave, by="startDateTime", all=T)
newTotal<- merge(newTotal,w6.H.ave, by="startDateTime", all=T)
newTotal<- merge(newTotal,w7.H.ave, by="startDateTime", all=T)
newTotal<- merge(newTotal,w8.H.ave, by="startDateTime", all=T)

colnames(newTotal) <- c("Date_Time", "Well1","Well2","Well3","Well4",
                        "Well5","Well6","Well7","Well8")

#Removes all values less than 0 from data frame
newTotal[newTotal < 0] <- NA

# Hard-coded corrections for transducer error
# On January 1,2018, the NEON data indicated an instant drop of ~6m
# Given the stability of the data after Jan 1,2018, the data
# Preceeding this date was smoothed to match the data following
# Jan 1, 2018

#Well1
newTotal[1:122,2]<-newTotal[1:122,2]-6.18
#Well2
newTotal[1:122,3]<-newTotal[1:122,3]-6.11
#Well3
newTotal[1:122,4]<-newTotal[1:122,4]-7.35
#Well4
newTotal[1:122,5]<-newTotal[1:122,5]-6.74
#Well5
newTotal[1:122,6]<-newTotal[1:122,6]-6.31
#Well6
newTotal[1:122,7]<-newTotal[1:122,7]-5.93
#Well7
newTotal[1:122,8]<-newTotal[1:122,8]-5.13
#Well8
newTotal[1:122,9]<-newTotal[1:122,9]-5.721


WLA.long <- melt(newTotal, id="Date_Time", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
WLA.long$Date_Time <- as.Date.character(WLA.long$Date_Time, format= "%Y-%m-%d")
WLA.long <- WLA.long[order(WLA.long$Date_Time), ]
#Use these lines to attach bankside info to DF
#The number in length.out will be the row# of WLA.long
bank <- c("Ag","Pr","Pr","Ag","Pr","Ag","Ag","Pr")
list2 <- rep(bank, length.out=nrow(WLA.long))
WLA.long<- cbind(WLA.long, list2)

#Use these lines to plot a section of groundwater data just for sampling period
#newTotal<- newTotal[-c(1:422),]

P<- ggplot(WLA.long, aes(x=Date_Time, y=value, group=variable, color=list2))+
  geom_line(aes(y= value,linetype=WLA.long$variable), size=1)+
  theme_bw()+
  labs(x="Date", y="Water Level Elevations(m)", color="Bank Side", linetype="Well Number", legend.position='bottom')+
  ggtitle("NEON Well Water Elevations")+
  theme(legend.position = 'bottom')+
  
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  scale_linetype_manual(values=c(1,2,3,4,5,6,1,2))
#pdf("NEON Water Levels.pdf")
print(P)


#This portion of code reads in stream flow data and adds to plot of GW

setwd("~/Dropbox/EVRN 624 Files/Data/RCodes/Data")
QP <- read.csv("KonzaQP.csv")
QP$Date <-as.POSIXct(QP$Date,format="%m/%d/%y")
QP$Date <- as.Date(QP$Date)
DP <- ggplot(QP, aes(x=Date, y=QP$Streamflow))+
  geom_line(aes(y=QP$Streamflow), size=1, color="red")+
  theme_bw()+
  labs(x="Date", y="Stream Discharge(cfs)")+
  scale_y_continuous(sec.axis = sec_axis(~.+001, name="Daily Precipitation(mm)"))+
  ggtitle("King's Creek Discharge vs Precipitation")+
  geom_bar(stat="identity", aes(y=QP$Precipitation), fill="navy")

Stream <-ggarrange(DP, P,
                  labels=c("A","B"),
                  ncol=1, nrow=2)
pdf("HydroGraph.pdf")
print(Stream)
dev.off()