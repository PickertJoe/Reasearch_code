library(data.table)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)


setwd("~/R_Scripts/Data/GWData/Raw/NEON_elev-groundwater/5_min_data")

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

#Removes an erroneous data point
newTotal[530,] <-NA

# Hard-coded corrections for transducer error
# On January 1,2018, the NEON data indicated an instant drop of ~6m
# Given the stability of the data after Jan 1,2018, the data
# Preceeding this date was smoothed to match the data following
# Jan 1, 2018

#Well1
newTotal[1:483,2]<-newTotal[1:483,2]-6.18
#Well2
newTotal[1:483,3]<-newTotal[1:483,3]-6.11
#Well3
newTotal[1:483,4]<-newTotal[1:483,4]-7.35
#Well4
newTotal[1:483,5]<-newTotal[1:483,5]-6.74
#Well5
newTotal[1:483,6]<-newTotal[1:483,6]-6.31
#Well6
newTotal[1:483,7]<-newTotal[1:483,7]-5.93
#Well7
newTotal[1:483,8]<-newTotal[1:483,8]-5.13
#Well8
newTotal[1:483,9]<-newTotal[1:483,9]-5.721


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

GW<- ggplot(WLA.long, aes(x=factor(Date_Time), y=value, group=variable, color=list2))+
  geom_line(aes(y= value,color = variable), size=1)+
  theme_bw()+
  labs(y="Water Level Elevations(m)", color="Well Number + Bankside", legend.position='bottom')+
  ggtitle("NEON Well Water Elevations")+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  scale_color_manual(values = c("#003300", "#0033FF", "#00FFFF", "#669933", "#0099FF", "#CCFF00", "#99FF99", "#000099"))+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                     labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))+
  #Adding annotation points to designate sampling dates
  annotate("point", x='2018-08-25', y=322.6, color='red', size=3)+
  annotate("point", x='2018-09-15', y=322.6, color='red', size=3)+
  annotate("point", x='2018-04-14', y=322.6, color='red', size=3)+
  annotate("point", x='2018-05-09', y=322.6, color='red', size=3)+
  annotate("point", x='2019-01-06', y=322.6, color='red', size=3)+
  annotate("point", x='2018-02-03', y=322.6, color='red', size=3)+
  annotate("point", x='2017-11-01', y=322.6, color='red', size=3)+
  annotate("point", x='2017-12-02', y=322.6, color='red', size=3)+
  annotate("point", x='2018-03-02', y=322.6, color='red', size=3)+
  annotate("point", x='2018-06-15', y=322.6, color='red', size=3)+
  annotate("point", x='2018-01-08', y=322.6, color='red', size=3)
print(GW)

#Creating GW elevation plots by bankside
AgGW <- ggplot(subset(WLA.long, variable %in% c("Well1", "Well4", "Well6", "Well7")))+
  geom_line(aes(factor(Date_Time), value, group=variable, color=variable), size=1)+
  theme_bw()+
  labs(y="Water Level Elevation(m)", color="Well Number")+
  ggtitle("Agricultural Bank GW Levels")+
  theme(legend.position = 'bottom')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                   labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))

PrGW <- ggplot(subset(WLA.long, variable %in% c("Well2", "Well3", "Well5", "Well8")))+
  geom_line(aes(factor(Date_Time), value, group=variable, color=variable), size=1)+
  theme_bw()+
  labs(y="Water Level Elevation(m)", color="Well Number")+
  ggtitle("Prairie Bank GW Levels")+
  theme(legend.position = 'bottom')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                   labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))


#This portion of code reads in stream flow data and adds to plot of GW

setwd("~/R_Scripts/Data/")
QP <- read.csv("KonzaQP.csv")
#Converting streamflow from cfs to m3/s
QP$Streamflow <- QP$Streamflow * .028316846592
QP$Date <-as.POSIXct(QP$Date,format="%m/%d/%y")
QP$Date <- as.Date(QP$Date)

SW <- ggplot(QP, aes(x=factor(Date), y=Streamflow))+
  geom_line(aes(y=Streamflow, group=1), size=1, color="red")+
  theme_bw()+
  labs(y="Discharge(m3/s)")+
  scale_y_continuous(trans='log10')+
  ggtitle("King's Creek Discharge")+
  theme(axis.title.x = element_blank())+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                   labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))+
  theme(plot.title = element_text(hjust=0.5))
  
print(SW)

P <- ggplot(QP, aes(x=factor(Date), y=Streamflow))+
  geom_bar(stat="identity", aes(y=QP$Precipitation), fill="navy",width=2)+
  theme_bw()+
  labs(y="Precipitation(mm)")+
  ggtitle("King's Creek Precipitation")+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.title.x = element_blank())+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                   labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))
print(P)


Stream <-ggarrange(P, SW, GW,
                  heights = c(.25,.25,.5),
                  ncol=1, nrow=3,
                  legend='bottom')

GW_Bankside <-ggarrange(AgGW, PrGW,
                        ncol=1, nrow=2,
                        legend='bottom')

#Saving the figure array as a pdf
setwd("~/R_Scripts/Figures")
pdf("HydroGraph.pdf")
print(Stream)
dev.off()

pdf("GW_Levels_Bankside.pdf")
print(GW_Bankside)
dev.off()