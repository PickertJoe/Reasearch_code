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
grid.table(banktable)
dev.off()


#This section contains all code to save data tables as pdfs

setwd("~/Desktop/R_scripts/Figures")

#Total data table
pdf(file = "total.pdf", height = 30, width = 12)
grid.table(master,rows=NULL)
dev.off()

master %>%
  +     group_by(.dots=c('Month','Bank')) %>%
  +     summarize(x=mean(DO))