# This script plots the observed precipitation at KONZA LTER
# For the water year starting in October 2017 compared to the climatic
# Average precipitation

library(ggplot2)
library(dplyr)
library(lubridate)
library(tibbletime)

setwd("~/Desktop/R_Scripts/Data/")
QP <- read.csv("KonzaQP.csv")
QP$Date <-as.POSIXct(QP$Date,format="%m/%d/%y")
QP$Date <- as.Date(QP$Date)
# Subsetting water year data
WaterYear = QP[396:760, 1:3]

# Removing streamflow data
WaterYear[2] = NULL

WYAgg = WaterYear %>%
    mutate(month = format(Date, "%m")) %>%
    group_by(month) %>%
    summarise(total = sum(Precipitation))
# Reordering months by occurrence in water year
WYAgg$month <- WYAgg[c(10,11,12,1,2,3,4,5,6,7,8,9), ]
WYAgg[2] = NULL



#setwd("~/Dropbox/EVRN 624 Files/Data/Rcodes/Data")
#SPrecip <- read.csv("Precipagg.csv")