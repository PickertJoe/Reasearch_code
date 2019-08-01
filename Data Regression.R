#This code requires both packages to produce figures
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gridExtra)
setwd("~/Desktop/R_Scripts/Data")
master <- read.csv("MasterData.csv")

#Below was my first attempt to plot using standard function
#ns <-plot(master$NO3, master$SO4, xlab=names(master)[11], 
#          ylab=names(master)[10], pch=16, 
#          main = "Correlation Between NO3 and SO4",
#          abline(lm(master$SO4 ~ master$NO3)),
#          col=wellname)

#I switched to ggplot so I could save the plots as local variables
#I plan on turning this into a for loop to reduce the lines of code

wellname <- as.factor(master[,2])

NS<-ggplot(master, aes(x=master$NO3, y=master$SO4, color=wellname))+ 
      geom_point(size=2)+
      theme_bw()+
      labs(y="[SO4] (mg/L)", x="[NO3] (mg/L)", color="Well Number")+
      ggtitle("Correlation Between NO3 and SO4")+
      geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
      scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

NC<-ggplot(master, aes(x=master$NO3, y=master$Cl, color=wellname))+ 
  geom_point(size=2)+
  theme_bw()+
  labs(y="[Cl] (mg/L)", x="[NO3] (mg/L)", color="Well Number")+
  ggtitle("Correlation Between NO3 and Cl")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

NAlk<-ggplot(master, aes(x=master$NO3, y=master$Alkalinity, color=wellname))+ 
  geom_point(size=2)+
  theme_bw()+
  labs(y="Alkalinity (mgHCO3/L)", x="[NO3] (mg/L)", color="Well Number")+
  ggtitle("Correlation Between NO3 and Alkalinity")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

NCond<-ggplot(master, aes(x=master$NO3, y=master$Conductivity, color=wellname))+ 
  geom_point(size=2)+
  theme_bw()+
  labs(y="Specific Conductance (us/cm)", x="[NO3] (mg/L)", color="Well Number")+
  ggtitle("Correlation Between NO3 and Conductance")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

NDO<-ggplot(master, aes(x=master$NO3, y=master$DO, color=wellname))+ 
  geom_point(size=2)+
  theme_bw()+
  labs(y="Dissolved Oxygen (%))", x="[NO3] (mg/L)", color="Well Number")+
  ggtitle("Correlation Between NO3 and DO")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

Np<-ggplot(master, aes(x=master$NO3, y=master$pH, color=wellname))+ 
  geom_point(size=2)+
  theme_bw()+
  labs(y="pH", x="[NO3] (mg/L)", color="Well Number")+
  ggtitle("Correlation Between NO3 and pH")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))

#Not sure if we wanted this one
if(FALSE){NTemp<-ggplot(master, aes(x=master$NO3, y=master$Temperature, color=wellname))+ 
  geom_point(size=2)+
  theme_bw()+
  labs(y="Water Temperature (C)", x="[NO3] (mg/L)", color="Well Number")+
  ggtitle("Correlation Between NO3 and Water Temperature")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(method='lm', se=FALSE, fullrange=TRUE)+
  scale_color_manual(values = c(Well1="black",
                                Well2="dodgerblue4",
                                Well3="cyan1",
                                Well4="forestgreen",
                                Well5="darkmagenta",
                                Well6="red1",
                                Well7="deeppink1",
                                Well8="darkgoldenrod4"))}
master$Month <- match(master$Month, month.abb)

Regplot<- ggarrange(NS, NC, NAlk, NCond, NDO, Np,
          labels= c("A", "B", "C", "D", "E", "F"),
          common.legend = TRUE, legend="bottom",
          ncol=2, nrow=3)

Cor_par<-round(cor(master[,c(5:8)], y=NULL,
             use = "pairwise.complete.obs", method = "pearson"), 3)

Cor_ion<-round(cor(master[,c(9:14,16:17)], y=NULL,
              use = "pairwise.complete.obs", method = "pearson"), 3)

#Use this line for exporting PDFs of Pearson correlation matrices
#pdf("filename.pdf", height=11, width=8.5)
  #grid.table(data_frame_name)
  #dev.off()

#This section can be used to print summary statistic tables over time
#And for individual wells
summarymast<- read.csv("Summary.csv")
summaryparyear <- summarymast[1:22,1:6]
summaryionyear <- summarymast[1:22,]
summaryionyear[,3:6] <- NULL
summarywell <-read.csv("SummaryW.csv")
bankpar <- read.csv("SummaryB.csv")

#Run these lines to print graphs to the folder
setwd("~/Desktop/R_Scripts")
pdf("Parameter summary.pdf", height=11, width=8.5)
grid.table(summaryparyear, rows=NULL)
dev.off()

pdf("Ion summary.pdf", height=11, width=8.5)
grid.table(Cor_ion)
dev.off()

pdf("Field Parameter Correlations.pdf", height=11, width = 8.5)
grid.table(Cor_par)
dev.off()

pdf("Well summary.pdf", height=11, width=8.5)
grid.table(summarywell, rows=NULL)
dev.off()

pdf("Bankside summary.pdf", height=11, width=8.5)
grid.table(bankpar, rows=NULL)
dev.off()

