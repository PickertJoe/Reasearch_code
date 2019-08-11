#This code requires both packages to produce figures
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gridExtra)
setwd("~/Desktop/R_Scripts/Data")

#Loading master data file
master <- read.csv("MasterData.csv")

#This equation will 

#The following section of code will create regression plots for each constituent
#In relation to nitrate across all of the wells
#Creating name vector using well numbers
# wellname <- as.factor(master[,2])
# 
# NS<-ggplot(master, aes(x=master$NO3, y=master$SO4, color=wellname))+ 
#       geom_point(size=2)+
#       theme_bw()+
#       labs(y="[SO4] (mg/L)", x="[NO3] (mg/L)", color="Well Number")+
#       ggtitle("Correlation Between NO3 and SO4")+
#       geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
#       scale_color_manual(values = c(Well1="black",
#                                 Well2="dodgerblue4",
#                                 Well3="cyan1",
#                                 Well4="forestgreen",
#                                 Well5="darkmagenta",
#                                 Well6="red1",
#                                 Well7="deeppink1",
#                                 Well8="darkgoldenrod4"))
# 
# NC<-ggplot(master, aes(x=master$NO3, y=master$Cl, color=wellname))+ 
#   geom_point(size=2)+
#   theme_bw()+
#   labs(y="[Cl] (mg/L)", x="[NO3] (mg/L)", color="Well Number")+
#   ggtitle("Correlation Between NO3 and Cl")+
#   #geom_line(color='black', data=NSreg)+
#   geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
#   scale_color_manual(values = c(Well1="black",
#                                 Well2="dodgerblue4",
#                                 Well3="cyan1",
#                                 Well4="forestgreen",
#                                 Well5="darkmagenta",
#                                 Well6="red1",
#                                 Well7="deeppink1",
#                                 Well8="darkgoldenrod4"))
# 
# NAlk<-ggplot(master, aes(x=master$NO3, y=master$Alkalinity, color=wellname))+ 
#   geom_point(size=2)+
#   theme_bw()+
#   labs(y="Alkalinity (mgHCO3/L)", x="[NO3] (mg/L)", color="Well Number")+
#   ggtitle("Correlation Between NO3 and Alkalinity")+
#   #geom_line(color='black', data=NSreg)+
#   geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
#   scale_color_manual(values = c(Well1="black",
#                                 Well2="dodgerblue4",
#                                 Well3="cyan1",
#                                 Well4="forestgreen",
#                                 Well5="darkmagenta",
#                                 Well6="red1",
#                                 Well7="deeppink1",
#                                 Well8="darkgoldenrod4"))
# 
# NCond<-ggplot(master, aes(x=master$NO3, y=master$Conductivity, color=wellname))+ 
#   geom_point(size=2)+
#   theme_bw()+
#   labs(y="Specific Conductance (us/cm)", x="[NO3] (mg/L)", color="Well Number")+
#   ggtitle("Correlation Between NO3 and Conductance")+
#   #geom_line(color='black', data=NSreg)+
#   geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
#   scale_color_manual(values = c(Well1="black",
#                                 Well2="dodgerblue4",
#                                 Well3="cyan1",
#                                 Well4="forestgreen",
#                                 Well5="darkmagenta",
#                                 Well6="red1",
#                                 Well7="deeppink1",
#                                 Well8="darkgoldenrod4"))
# 
# NDO<-ggplot(master, aes(x=master$NO3, y=master$DO, color=wellname))+ 
#   geom_point(size=2)+
#   theme_bw()+
#   labs(y="Dissolved Oxygen (%))", x="[NO3] (mg/L)", color="Well Number")+
#   ggtitle("Correlation Between NO3 and DO")+
#   #geom_line(color='black', data=NSreg)+
#   geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
#   scale_color_manual(values = c(Well1="black",
#                                 Well2="dodgerblue4",
#                                 Well3="cyan1",
#                                 Well4="forestgreen",
#                                 Well5="darkmagenta",
#                                 Well6="red1",
#                                 Well7="deeppink1",
#                                 Well8="darkgoldenrod4"))
# 
# Np<-ggplot(master, aes(x=master$NO3, y=master$pH, color=wellname))+ 
#   geom_point(size=2)+
#   theme_bw()+
#   labs(y="pH", x="[NO3] (mg/L)", color="Well Number")+
#   ggtitle("Correlation Between NO3 and pH")+
#   #geom_line(color='black', data=NSreg)+
#   geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
#   scale_color_manual(values = c(Well1="black",
#                                 Well2="dodgerblue4",
#                                 Well3="cyan1",
#                                 Well4="forestgreen",
#                                 Well5="darkmagenta",
#                                 Well6="red1",
#                                 Well7="deeppink1",
#                                 Well8="darkgoldenrod4"))
# 
# #Not sure if we wanted this one
# if(FALSE){NTemp<-ggplot(master, aes(x=master$NO3, y=master$Temperature, color=wellname))+ 
#   geom_point(size=2)+
#   theme_bw()+
#   labs(y="Water Temperature (C)", x="[NO3] (mg/L)", color="Well Number")+
#   ggtitle("Correlation Between NO3 and Water Temperature")+
#   #geom_line(color='black', data=NSreg)+
#   geom_smooth(method='lm', se=FALSE, fullrange=TRUE)+
#   scale_color_manual(values = c(Well1="black",
#                                 Well2="dodgerblue4",
#                                 Well3="cyan1",
#                                 Well4="forestgreen",
#                                 Well5="darkmagenta",
#                                 Well6="red1",
#                                 Well7="deeppink1",
#                                 Well8="darkgoldenrod4"))}
# master$Month <- match(master$Month, month.abb)

# Regplot<- ggarrange(NS, NC, NAlk, NCond, NDO, Np,
#           labels= c("A", "B", "C", "D", "E", "F"),
#           common.legend = TRUE, legend="bottom",
#           ncol=2, nrow=3)

########################################
####END TOTAL REGRESSION CODE BLOCK#####
########################################

#Here, we create subsets of the data from the prairie and cultivated banks
#To explore if correlations differed between them

Ag_data <- master[which(master$Bank=='Ag'),]
Pr_data <- master[which(master$Bank=='Pr'),]

########################################
####BEGIN AG REGRESSION CODE BLOCK######
########################################

#If you want to swap in colors different from the default, use:
# scale_color_manual(values = c(Well1="cyan1",
#                               Well4="forestgreen",
#                               Well6="dodgerblue4",
#                               Well7="deeppink1"
#                               )
#in the ggplot function

AG_SO4_COND<-ggplot(Ag_data, aes(x=SO4, y=Conductivity, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="Conductivity (us/cm)", x="[SO4] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=40, y=612, label = "paste(italic(R) ^ 2, \" = -0.623\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

AG_NO3_COND<-ggplot(Ag_data, aes(x=NO3, y=Conductivity, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="Conductivity (us/cm)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=2, y=675, label = "paste(italic(R) ^ 2, \" = -0.695\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

AG_NO3_Cl<-ggplot(Ag_data, aes(x=NO3, y=Cl, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="[Cl] (mg/L)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=1.5, y=3, label = "paste(italic(R) ^ 2, \" = 0.571\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

AG_NO3_SO4<-ggplot(Ag_data, aes(x=NO3, y=SO4, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="[SO4] (mg/L)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=2, y=40, label = "paste(italic(R) ^ 2, \" = 0.739\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

AG_NO3_K<-ggplot(Ag_data, aes(x=NO3, y=K, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="[K] (mg/L)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=2, y=1.75, label = "paste(italic(R) ^ 2, \" = -.549\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

AG_CA_MG<-ggplot(Ag_data, aes(x=Ca, y=Mg, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="[Mg] (mg/L)", x="[Ca] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=130, y=20, label = "paste(italic(R) ^ 2, \" = 0.557\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

Ag_Corr_Plots <- ggarrange(AG_NO3_COND,AG_NO3_Cl, AG_NO3_K, AG_NO3_SO4, 
                           AG_SO4_COND, AG_CA_MG,
                           common.legend = TRUE, legend="bottom",
                           ncol=2, nrow=3)
Ag_Corr_Plots <- annotate_figure(Ag_Corr_Plots, top = text_grob("Significant Correlations (Ag)", 
                                    color='black', face= 'bold', size=18))

########################################
####END AG REGRESSION CODE BLOCK########
########################################

########################################
####BEGIN PR REGRESSION CODE BLOCK######
########################################

PR_NO3_TEMP<-ggplot(Pr_data, aes(x=NO3, y=Temperature, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="Water Temperature (C)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=1, y=16, label = "paste(italic(R) ^ 2, \" = 0.835\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

PR_NO3_Cl<-ggplot(Pr_data, aes(x=NO3, y=Cl, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="[Cl] (mg/L)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=1, y=2.75, label = "paste(italic(R) ^ 2, \" = 0.818\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

PR_NO3_COND<-ggplot(Pr_data, aes(x=NO3, y=Conductivity, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="Conductivity (us/cm)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=1, y=625, label = "paste(italic(R) ^ 2, \" = -0.61\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

PR_NO3_SO4<-ggplot(Pr_data, aes(x=NO3, y=SO4, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="[SO4] (mg/L)", x="[NO3-N] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=1.5, y=40, label = "paste(italic(R) ^ 2, \" = 0.84\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

PR_SO4_COND<-ggplot(Pr_data, aes(x=SO4, y=Conductivity, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="Conductivity (us/cm)", x="[SO4] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=40, y=625, label = "paste(italic(R) ^ 2, \" = -0.772\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

PR_K_Na<-ggplot(Pr_data, aes(x=K, y=Na, color=factor(Well)))+
  geom_point(size=2)+
  theme_bw()+
  labs(y="[Na] (mg/L)", x="[K] (mg/L)", color="Well Number")+
  #geom_line(color='black', data=NSreg)+
  geom_smooth(aes(group=1), color="black", method='lm', se=FALSE, fullrange=TRUE)+
  annotate("text", x=1, y=4.75, label = "paste(italic(R) ^ 2, \" = 0.795\")", parse=TRUE)+
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(hjust=0.5))

Pr_Corr_Plots <- ggarrange(PR_NO3_TEMP,PR_NO3_Cl, PR_NO3_COND, PR_NO3_SO4, 
                           PR_SO4_COND, PR_K_Na,
                           common.legend = TRUE, legend="bottom",
                           ncol=2, nrow=3)
Pr_Corr_Plots <- annotate_figure(Pr_Corr_Plots, top = text_grob("Significant Correlations (Pr)", 
                                                                color='black', face= 'bold', size=18))

########################################
####END PR REGRESSION CODE BLOCK########
########################################

Cor_par<-round(cor(master[,c(5:8)], y=NULL,
             use = "pairwise.complete.obs", method = "pearson"), 3)

Cor_ion<-round(cor(master[,c(9:14,16:17)], y=NULL,
              use = "pairwise.complete.obs", method = "pearson"), 3)

Cor_total<-round(cor(master[,c(5:14,16:17)], y=NULL,
              use = "pairwise.complete.obs", method = "pearson"), 3)



#Creating the correlation matrices for both banks

Cor_Ag <- round(cor(Ag_data[,c(5:14,16:17)], y=NULL,
                use = "pairwise.complete.obs", method = "pearson"), 3)

Cor_Pr <- round(cor(Pr_data[,c(5:14,16:17)], y=NULL,
                    use = "pairwise.complete.obs", method = "pearson"), 3)



#This section can be used to print summary statistic tables over time
#And for individual wells
summarymast<- read.csv("Summary.csv")
summaryparyear <- summarymast[1:22,1:6]
summaryionyear <- summarymast[1:22,]
summaryionyear[,3:6] <- NULL
summarywell <-read.csv("SummaryW.csv")
bankpar <- read.csv("SummaryB.csv")

#Run these lines to print graphs to the folder
setwd("~/Desktop/R_Scripts/Figures")

pdf("Parameter summary.pdf", height=11, width=8.5)
grid.table(summaryparyear, rows=NULL)
dev.off()

pdf("Ion_correlations.pdf", height=11, width=8.5)
grid.table(Cor_ion)
dev.off()

pdf("Parameter_Correlations.pdf", height=11, width = 8.5)
grid.table(Cor_par)
dev.off()

pdf("Total_Correlations.pdf", width=11, height = 5)
grid.table(Cor_total)
dev.off()

pdf("Ag_Correlations.pdf", width=11, height = 5)
grid.table(Cor_Ag)
dev.off()

pdf("Pr_Correlations.pdf", width=11, height = 5)
grid.table(Cor_Pr)
dev.off()

pdf("Ag_Correlation_Plots.pdf")
print(Ag_Corr_Plots)
dev.off()

pdf("Pr_Correlation_Plots.pdf")
print(Pr_Corr_Plots)
dev.off()

pdf("Well summary.pdf", height=11, width=8.5)
grid.table(summarywell, rows=NULL)
dev.off()

pdf("Bankside summary.pdf", height=11, width=8.5)
grid.table(bankpar, rows=NULL)
dev.off()

