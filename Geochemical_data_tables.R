#This script generates the necessary summary tables for geochemical
#Data collected during this study

library(ggplot2)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(magrittr)
library(qwraps2)

setwd("~/Desktop/R_Scripts/Data")
master <- read.csv("MasterData.csv")

pdf(file = "total.pdf", height = 22, width = 12)
grid.table(master,rows=NULL)
dev.off()

our_summary1 <-
  list("Dissolved Oxygen(%)" =
         list("min" = ~ min(.data$DO),
              "max" = ~ max(.data$DO),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$DO)))
whole <- summary_table(master, our_summary1)
print(whole)