#This script generates the necessary summary tables for geochemical
#Data collected during this study

library(ggplot2)
library(dplyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(magrittr)
library(qwraps2)

setwd("~/Desktop/R_Scripts/Data")
master <- read.csv("MasterData.csv")

#Creating a new, separate data frame to hold mean values by month and bank
means <-aggregate(master, by=list(master$Month, master$Bank), 
                  FUN = mean_sd(master), na.rm=TRUE, na.action = NULL)

#For whatever reason, this aggregate creates three unecessary columns; remove them
means[,3:5] <- NULL

#Replace columns 1 and 2 with appropriate names
colnames(means)[1:2] <- c("Month", "Bank")

#Creating a custom vector to reorder table in sampling order
month_order = c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
means <- means %>%
  mutate(Month = factor(Month, levels = month_order)) %>%
  arrange(Month)

#This section contains all code to save data tables as pdfs

setwd("~/Desktop/R_scripts/Figures")

#Total data table
pdf(file = "total.pdf", height = 30, width = 12)
grid.table(master,rows=NULL)
dev.off()




print(test)
master %>%
  +     group_by(.dots=c('Month','Bank')) %>%
  +     summarize(x=mean(DO))