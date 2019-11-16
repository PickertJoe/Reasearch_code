library(data.table)
library(ggplot2)
library(dplyr)

setwd("~/R_Scripts/Data/GWData/Raw/NEON_cond-groundwater/5_min_data")

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

#Hard-coded values to remove excessively extraneous data points
well1<-subset(well1, groundwaterSpecCond >550)
well2<-subset(well2, groundwaterSpecCond >550)
well3<-subset(well3, groundwaterSpecCond >550)
well4<-subset(well4, groundwaterSpecCond >550)
well5<-subset(well5, groundwaterSpecCond >550)
well6<-subset(well6, groundwaterSpecCond >550)
well7<-subset(well7, groundwaterSpecCond >550)
well8<-subset(well8, groundwaterSpecCond >550)

well1<-subset(well1, groundwaterSpecCond <800)
well2<-subset(well2, groundwaterSpecCond <800)
well3<-subset(well3, groundwaterSpecCond <800)
well4<-subset(well4, groundwaterSpecCond <800)
well5<-subset(well5, groundwaterSpecCond <800)
well6<-subset(well6, groundwaterSpecCond <800)
well7<-subset(well7, groundwaterSpecCond <800)
well8<-subset(well8, groundwaterSpecCond <800)



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

#Plots averaged daily conductivity values for all the wells
C<- ggplot(COND.long, aes(x=Date_Time, y=value, group=variable, color=list2))+
  geom_line(aes(linetype=COND.long$variable), size=1)+
  theme_bw()+
  labs(x="Date", y="Conductivity(us/cm)", color="Bank Side", linetype="Well Number")+
  ggtitle("NEON Well Water Conductivity")+

  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
   scale_linetype_manual(values=c(1,2,3,4,5,6,1,2))

#Plots whole data set averaged by bankside
CBank<- ggplot(COND.long, aes(x=factor(Date_Time), y=value, group=list2, color=list2))+
  stat_summary(fun.y = mean, geom="line",size=1)+
  theme_bw()+
  labs(y="Conductivity(us/cm)", color="Bank Side")+
  ggtitle("NEON Well Water Conductivity")+
  theme(plot.title = element_text(hjust=0.5))+
  ylim(600,750)+
  theme(legend.position = 'bottom')+
  scale_color_manual(values = c(Ag="black",
                                Pr="blue"))+
  theme(axis.title.x = element_blank())+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                   labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))

#Plotting just the agricultural bank
AgC <- ggplot(subset(COND.long, variable %in% c("Well1", "Well4", "Well6", "Well7")))+
  geom_line(aes(factor(Date_Time), value, group=variable, color=variable), size=1)+
  theme_bw()+
  labs(y="Conductivity(us/cm)", color="Well Number")+
  ggtitle("Agricultural Bank Conductivity")+
  ylim(550,800)+
  theme(legend.position = 'bottom')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                   labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))
print(AgC)

#Plotting just the prairie bank
PrC <- ggplot(subset(COND.long, variable %in% c("Well2", "Well3", "Well5", "Well8")))+
  geom_line(aes(factor(Date_Time), value, group=variable, color=variable), size=1)+
  theme_bw()+
  labs(x="Date", y="Conductivity(us/cm)", color="Well Number")+
  ggtitle("Prairie Bank Conductivity")+
  ylim(550,800)+
  theme(legend.position = 'bottom')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks=c('2016-09-01', '2017-01-01', '2017-07-01', '2018-01-01', '2018-07-01', '2019-01-01'),
                   labels=c("2016-09", "2017-01", "2017-07", "2018-01", "2018-07", "2019-01"))

print(PrC)

BankConductivity <-ggarrange(AgC, PrC,
                   ncol=1, nrow=2,
                   legend='bottom')


setwd("~/R_Scripts/Figures/")

#Plotting all conductivity data
pdf("NEON_Total_Conductivity.pdf")
print(C)
dev.off()

#Printing both bankside conductivity graphs
pdf("NEON_Bankside_Well.pdf")
print(BankConductivity)
dev.off()

#Printhing the bankside average plots
pdf("NEON_Bankside_Average.pdf")
print(CBank)
dev.off()

#This section of code will create an array to compare the NEON
#Water elevations and the NEON conductivity values by bankside
#This will require you to run the NEON GW Levels.R script in order to work

GW_COND <- ggarrange(GW, CBank,
                     ncol=1,
                     nrow = 2,
                     legend='bottom')

pdf("Combined_GW_Conductivity.pdf")
print(GW_COND)
dev.off()

#This section of code will create an array to compare the NEON GW elevations
#To the NEON conductivity values by inidividual wells by bankside
#This will require you to run the NEON GW Levels.R script first

Ag_GW_COND <- ggarrange(AgGW, AgC,
                        ncol=1, nrow = 2,
                        legend='bottom')

Pr_GW_COND <- ggarrange(PrGW, PrC,
                        ncol=1, nrow=2,
                        legend='bottom')

pdf("Ag_GW_COND.pdf")
print(Ag_GW_COND)
dev.off()

pdf("Pr_GW_COND.pdf")
print(Pr_GW_COND)
dev.off()