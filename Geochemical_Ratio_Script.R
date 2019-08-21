#This script utilizes meq/L ratios to further explore geochemical data
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd("~/Desktop/R_Scripts/Data")
masterRatio <- read.csv("MasterData.csv")
masterRatio$Month = factor(masterRatio$Month, levels=c("Nov '17","Dec '17","Jan '18","Feb '18","Mar '18", "Apr '18", "May '18", "Jun '18", "Jul '18", "Aug '18", "Sep '18", "Jan '19"))

#This section of code will calculate the millequivalents of each constituent
#And add them as columns to the dataframe
masterRatio$Alk_meq <- masterRatio[,9] / 100
masterRatio$Cl_meq <- masterRatio[,10] / 35
masterRatio$SO4_meq <- masterRatio[,11] / 48
masterRatio$NO3_meq <- masterRatio[,12] / 62
masterRatio$Ca_meq <- masterRatio[,13] / 20
masterRatio$K_meq <- masterRatio[,14] / 39
masterRatio$NH4_meq <- masterRatio[,15] / 18
masterRatio$Mg_meq <- masterRatio[,16] / 12
masterRatio$Na_meq <- masterRatio[,17] / 23

#This section of code creates new columns to hold the ratio data for various constituents
masterRatio$Ca_Mg <- masterRatio[,26] / masterRatio[,29]
masterRatio$SO4_Cl <- masterRatio[,24] / masterRatio[,23]

Ca_Mg_SO4_Cl <- ggplot(masterRatio, aes(x=Ca_Mg, y=SO4_Cl, group=Month, color=Month))+
  theme_bw()+
  geom_point(aes(size=2))+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Ca/Mg(meq/L)", y="SO4/Cl(meq/L)")+
  ggtitle("SO4/Cl vs Ca/Mg")

setwd("~/Desktop/R_Scripts/Figures")
pdf("SO4_Cl_Ca_Mg.pdf")
print(Ca_Mg_SO4_Cl)
dev.off()
