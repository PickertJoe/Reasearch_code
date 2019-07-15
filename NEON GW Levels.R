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

# #hard-coded corrections for transducer error
# newTotal<-newTotal[-c(530),]
# newTotal<-newTotal[-c(556),]
# #Well1
# newTotal[530:555,2]<-newTotal[530:555,2]-3.25
# newTotal[556:750,2]<-newTotal[556:750,2]-1.9395
# #Well2
# newTotal[530:555,3]<-newTotal[530:555,3]-.5135
# newTotal[556:750,3]<-newTotal[556:750,3]+.7995
# #Well3
# newTotal[530:555,4]<-newTotal[530:555,4]-.9
# newTotal[556:750,4]<-newTotal[556:750,4]+.4
# newTotal[454:609,4]<-newTotal[454:609,4]+.4201
# newTotal[609:750,4]<-newTotal[609:750,4]+.7415
# newTotal[609,4]<-newTotal[609,4]-.614
# #Well4
# newTotal[530:555,5]<-newTotal[530:555,5]-2.4241
# newTotal[556:750,5]<-newTotal[556:750,5]-1.0741
# #Well5
# newTotal[530:555,6]<-newTotal[530:555,6]-0.84
# newTotal[556:750,6]<-newTotal[556:750,6]+.4674
# #Well6
# newTotal[530:555,7]<-newTotal[530:555,7]-2.97
# newTotal[556:750,7]<-newTotal[556:750,7]-1.5887
# #Well7
# newTotal[530:555,8]<-newTotal[530:555,8]-3.43
# newTotal[556:750,8]<-newTotal[556:750,8]-2.1742
# #Well8
# newTotal[530:555,9]<-newTotal[530:555,9]-2.1134
# newTotal[556:750,9]<-newTotal[556:750,9]-0.7616
# newTotal[515:750,9]<-newTotal[515:750,9]-.4504

WLA.long <- melt(newTotal, id="Date_Time", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
WLA.long$Date_Time <- as.Date.character(WLA.long$Date_Time, format= "%Y-%m-%d")
WLA.long <- WLA.long[order(WLA.long$Date_Time), ]
#Use these lines to attach bankside info to DF
#The number in length.out will be the row# of WLA.long
bank <- c("Ag","Pr","Pr","Ag","Pr","Ag","Ag","Pr")
list2 <- rep(bank, length.out=nrow(WLA.long))
WLA.long<- cbind(WLA.long, list2)

#Removing GW values less than 0
WLA.long.corr <- subset(WLA.long, value >0)
list2 <- rep(bank, length.out=nrow(WLA.long.corr))


P<- ggplot(WLA.long.corr, aes(x=Date_Time, y=value, group=variable, color=list2))+
  geom_line(aes(y= value,linetype=WLA.long.corr$variable), size=1)+
  theme_bw()+
  labs(x="Date", y="Water Level Elevations(m)", color="Bank Side", linetype="Well Number")+
  ggtitle("NEON Well Water Elevations")+

  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  #geom_point(aes(shape=WLA.long$variable))+
  #scale_shape_manual(values=c(0,1,2,5,6,8,11,14))
  scale_linetype_manual(values=c(1,2,3,4,5,6,1,2))
  #scale_color_manual(values = c(Well1="black",
  #                              Well2="dodgerblue4",
  #                              Well3="cyan1",
  #                              Well4="forestgreen",
  #                              Well5="darkmagenta",
  #                              Well6="red1",
  #                              Well7="deeppink1",
  #                              Well8="darkgoldenrod4"))
#pdf("NEON Water Levels.pdf")
print(P)
#dev.off()

#Use these lines to plot a section of groundwater data just for sampling period
#newTotal<- newTotal[-c(1:422),]
WLA.long <- melt(newTotal, id="Date_Time", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
WLA.long$Date_Time <- as.Date.character(WLA.long$Date_Time, format= "%Y-%m-%d")
WLA.long <- WLA.long[order(WLA.long$Date_Time), ]
#Use these lines to attach bankside info to DF
#The number in length.out will be the row# of WLA.long
bank <- c("Ag","Pr","Pr","Ag","Pr","Ag","Ag","Pr")
list2 <- rep(bank, length.out=nrow(WLA.long))
WLA.long<- cbind(WLA.long, list2)
P<- ggplot(WLA.long, aes(x=Date_Time, y=value, group=variable, color=list2))+
  geom_line(aes(y= value,linetype=WLA.long$variable), size=1)+
  theme_bw()+
  labs(x="Date", y="Water Level Elevations(m)", color="Bank Side", linetype="Well Number")+
  ggtitle("NEON Well Water Elevations")+
  
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  #geom_point(aes(shape=WLA.long$variable))+
  #scale_shape_manual(values=c(0,1,2,5,6,8,11,14))
  scale_linetype_manual(values=c(1,2,3,4,5,6,1,2))
#scale_color_manual(values = c(Well1="black",
#                              Well2="dodgerblue4",
#                              Well3="cyan1",
#                              Well4="forestgreen",
#                              Well5="darkmagenta",
#                              Well6="red1",
#                              Well7="deeppink1",
#                              Well8="darkgoldenrod4"))
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
print(Stream)