library(data.table)
library(ggplot2)
library(dplyr)

setwd("~/Dropbox/EVRN 624 Files/Data/GWData/Raw/Neon_cond/5_min_data")

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

well1<-subset(well1, groundwaterSpecCond >550)
well2<-subset(well2, groundwaterSpecCond >550)
well3<-subset(well3, groundwaterSpecCond >550)
well4<-subset(well4, groundwaterSpecCond >550)
well5<-subset(well5, groundwaterSpecCond >550)
well6<-subset(well6, groundwaterSpecCond >550)
well7<-subset(well7, groundwaterSpecCond >550)
well8<-subset(well8, groundwaterSpecCond >550)

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
w1.H.ave<-aggregate(well1["groundwaterSpecCond"], format(well1["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w2.H.ave<-aggregate(well2["groundwaterSpecCond"], format(well2["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w3.H.ave<-aggregate(well3["groundwaterSpecCond"], format(well3["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w4.H.ave<-aggregate(well4["groundwaterSpecCond"], format(well4["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w5.H.ave<-aggregate(well5["groundwaterSpecCond"], format(well5["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w6.H.ave<-aggregate(well6["groundwaterSpecCond"], format(well6["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)  
w7.H.ave<-aggregate(well7["groundwaterSpecCond"], format(well7["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE) 
w8.H.ave<-aggregate(well8["groundwaterSpecCond"], format(well8["startDateTime"],"%Y-%m-%d"), mean, na.rm = TRUE)


newTotalC<- merge(w1.H.ave,w2.H.ave, by="startDateTime", all=T)
newTotalC<- merge(newTotalC,w3.H.ave, by="startDateTime", all=T)
newTotalC<- merge(newTotalC,w4.H.ave, by="startDateTime", all=T)
newTotalC<- merge(newTotalC,w5.H.ave, by="startDateTime", all=T)
newTotalC<- merge(newTotalC,w6.H.ave, by="startDateTime", all=T)
newTotalC<- merge(newTotalC,w7.H.ave, by="startDateTime", all=T)
newTotalC<- merge(newTotalC,w8.H.ave, by="startDateTime", all=T)

colnames(newTotalC) <- c("Date_Time", "Well1","Well2","Well3","Well4",
                        "Well5","Well6","Well7","Well8")

COND.long <- melt(newTotalC, id="Date_Time", measure=c("Well1","Well2","Well3","Well4","Well5","Well6","Well7","Well8"))
COND.long$Date_Time <- as.Date.character(COND.long$Date_Time, format="%Y-%m-%d")
COND.long <- COND.long[order(COND.long$Date_Time), ]
#Use these lines to attach bankside info to DF
#The number in length.out will be the row# of WLA.long
bank <- c("Ag","Pr","Pr","Ag","Pr","Ag","Ag","Pr")
list2 <- rep(bank, length.out=nrow(COND.long))
COND.long<- cbind(COND.long, list2)
C<- ggplot(COND.long, aes(x=Date_Time, y=value, group=variable, color=list2))+
  geom_line(aes(linetype=COND.long$variable), size=1)+
  theme_bw()+
  labs(x="Date", y="Conductivity(us/cm)", color="Bank Side", linetype="Well Number")+
  ggtitle("NEON Well Water Conductivity")+

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
print(C)
#dev.off()
