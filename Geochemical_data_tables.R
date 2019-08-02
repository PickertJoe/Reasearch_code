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

options(qwraps2_markup = "markdown")


our_summary1 <-
  list("Dissolved Oxygen(%)" =
         list("Min" = ~ min(.data$DO, na.rm = TRUE),
              "Max" = ~ max(.data$DO, na.rm = TRUE),
              "Mean (sd)" = ~ qwraps2::mean_sd(.data$DO, na_rm = TRUE, denote_sd = "pm")),
      "Specific Conductivity(us/cm)" = 
        list("Min" = ~ min(.data$Conductivity, na.rm = TRUE),
             "Max" = ~ max(.data$Conductivity, na.rm = TRUE),
             "Mean (sd)" = ~ qwraps2::mean_sd(.data$Conductivity, na_rm = TRUE, denote_sd = "pm")))

whole <- summary_table(master, our_summary1)
print(whole)
banktable <- summary_table(dplyr::group_by(master, Bank), our_summary1)
print(banktable)
monthtable <- summary_table(dplyr::group_by(master, Month), our_summary1)
pdf("testprint.pdf")
print(whole)
dev.off()

#This section contains all code to save data tables as pdfs

setwd("~/Desktop/R_scripts/Figures")

#Total data table
pdf(file = "total.pdf", height = 30, width = 12)
grid.table(master,rows=NULL)
dev.off()


test <-aggregate(master, by=list(master$Month, master$Bank), FUN = mean, na.rm=TRUE, na.action = NULL)
#For whatever reason, this aggregate creates three unecessary columns; remove them
test[,3:5] <- NULL
#Replace columns 1 and 2 with appropriate names
colnames(test)[1:2] <- c("Month", "Bank")

#Creating a custom vector to reorder table in sampling order
month_order = c("Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
test >%>
  slice(match(month_order, Month))

print(test)
master %>%
  +     group_by(.dots=c('Month','Bank')) %>%
  +     summarize(x=mean(DO))