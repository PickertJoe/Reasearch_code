library(data.table)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)

setwd("~/Dropbox/EVRN 624 Files/Data/Alternate_GW_DATA")
AltGW<-read.csv("KONZA_AP.csv", stringsAsFactors = FALSE, na.strings = ".", colClasses = c('character','character','numeric'))
AltGW$NO3<-as.numeric(as.character(AltGW$NO3))

newDF <-data.frame(Date=as.POSIXct(AltGW$Date, format="%m/%d/%y"),
                  well=AltGW$WELLNUMBER,
                     NO3=as.numeric(AltGW$NO3))
newDF$Date<-as.character(newDF$Date)
Alt.long<-melt(newDF, id="Date", measure=c(""))
