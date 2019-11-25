#Visualizations to display categorical data
#Specifically, contingency across different classification systems (DSM-V vs TMCQ temperament) 
#and within classification system across time (Y1-Y3)

#Load packages
library(tidyverse)
library(reshape2)
library(ggalluvial)

#Assign palettes for figures
pal = c("#B7FFFD","#52D4EE","#00A5C6")
pal2 = c("#FDFFB7", "#FFDE5B", "#FF9F23")
pal3 = c("#DD3A3A", "#F0785A", "#FFB499")
StablePalette <- c("#067DA1", "#ABD5E2", "#CC3533", "#FFAD99", "#FFCD51", "#FFE6B2")
AdditionPal <- "#43accc"

#Bring in data
###############################################################################################################

#Load data and select Y1 ADHD only
data <- read.csv('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Data/Y1-3 ADHD TMCQ and DSM Long Only.csv', 
                 na.strings = c("NA", -999, ""), fileEncoding="UTF-8-BOM") %>%
  filter(Y1_ADHD_STATUS == 3)%>%
  mutate(Y1_CD_TMCQmdgrpR = recode(Y1_CD_TMCQmdgrpR, "1" = "Mild", "2" = "Surgent", "3" = "Irritable"), 
         Y2_CD_TMCQmdgrpR = recode(Y2_CD_TMCQmdgrpR, "1" = "Mild", "2" = "Surgent", "3" = "Irritable"), 
         Y3_CD_TMCQmdgrpR = recode(Y3_CD_TMCQmdgrpR, "1" = "Mild", "2" = "Surgent", "3" = "Irritable"),
         Y1_ADHDsx_subtype = recode(Y1_ADHDsx_subtype, "1" = "Hyperactive", "2" = "Inattentive", "3" ="Combined", "0" = "Subthreshold" ),
         Y2_ADHDsx_subtype = recode(Y2_ADHDsx_subtype, "1" = "Hyperactive", "2" = "Inattentive", "3" ="Combined", "0" = "Subthreshold" ),
         Y3_ADHDsx_subtype = recode(Y3_ADHDsx_subtype, "1" = "Hyperactive", "2" = "Inattentive", "3" ="Combined", "0" = "Subthreshold" ))

###############################################################################################################

#Get contingency data
###############################################################################################################

#For contingency across classification methods, get frequency values, 
#simplify variable names, remove rows with NA values and DSM subthreshold (DSM = 0), and recode labels as factors
Contdata <- as.data.frame(AggSet) %>%
  dcast(Y1_CD_TMCQmdgrpR + Y1_ADHDsx_subtype ~ "Frequency", fun.aggregate = length) %>%
  rename(TMCQ = Y1_CD_TMCQmdgrpR, DSM = Y1_ADHDsx_subtype)%>%
  filter(!is.na(TMCQ),!is.na(DSM), DSM != "Subthreshold")

###############################################################################################################

## ALLUVIAL PLOT CONTINGENCY
###############################################################################################################
#Plot as alluvial
library(ggalluvial)

#This version of the alluvial plot uses wide-format data
ggplot(data = Contdata,
                aes(axis1 = TMCQ, axis2 = DSM, y = Frequency)) +
  scale_x_discrete(expand = c(.1, .05), limits = c("TMCQ", "DSM")) + #expand is related to the distance of the data from the axes
  xlab("Classification Method") +
  ylab("Frequency")+
  #alluvia are the connection between x-axis points
  geom_alluvium(aes(fill = TMCQ), alpha = .7, width = .1) + #if straight lines are desired add: knot.pos = 0
  #strata are the x-axis points; use geom_text in place of geom_label to remove the text background
  geom_stratum(width = .1, color = "white", fill = "grey23", alpha = .5) + #geom_label(stat = "stratum", label.strata = TRUE, color = "grey23") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  ggtitle("Correspondence between TMCQ and DSM Classification")+
  scale_fill_manual(name = "TCMQ", values = pal, labels = c("Class 1", "Class 2", "Class3"))

#Save plot (this figure type seems to require higher resolution)
ggsave(paste('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Figures & Output','DSM_TMCQ_Sunburst2.jpeg',sep = "/"), dpi = 600, width = 7, height = 7)

###############################################################################################################

## BUBBLE PLOT FOR CONTINGENCY
###############################################################################################################
#Plot as a contingency table bubble plot with frequency determining bubble size and transparency, with different colors for levels on the y axis
ggplot(Contdata, aes(TMCQ, DSM, color = DSM, alpha = Frequency))+
  geom_point(aes(size = Frequency), show.legend = FALSE)+
  geom_text(aes(label = Frequency), color = "grey23", size = 8) +
  scale_size(range = c(5,100))+
  scale_alpha(range = c(0.75,1))+
  theme_bw(base_size = 15)+
  scale_color_manual(values = pal)+
  theme(legend.position = "none")+
  xlab("")+
  ylab("")
###############################################################################################################

## SUNBURST PLOT FOR CONTINGENCY
###############################################################################################################

#Relabel level 2 (DSM class) to be contingent on level 1 (TMCQ class)
Contdata$DSM <- ifelse(Contdata$TMCQ == "Irritable" & Contdata$DSM == "Combined",  "Irrit_Comb",  
                ifelse(Contdata$TMCQ == "Irritable" & Contdata$DSM == "Hyperactive",  "Irrit_Hyp",  
                ifelse(Contdata$TMCQ == "Irritable" & Contdata$DSM == "Inattentive",  "Irrit_Int",  
                ifelse(Contdata$TMCQ == "Mild" & Contdata$DSM == "Combined",  "Mild_Comb",  
                ifelse(Contdata$TMCQ == "Mild" & Contdata$DSM == "Hyperactive",  "Mild_Hyp",  
                ifelse(Contdata$TMCQ == "Mild" & Contdata$DSM == "Inattentive",  "Mild_Int",  
                ifelse(Contdata$TMCQ == "Surgent" & Contdata$DSM == "Combined",  "Surg_Comb",  
                ifelse(Contdata$TMCQ == "Surgent" & Contdata$DSM == "Hyperactive",  "Surg_Hyp",  
                ifelse(Contdata$TMCQ == "Surgent" & Contdata$DSM == "Inattentive","Surg_Int","?????")))))))))

#Change factor level ordering to get the outer segments to appear in the desired order
Contdata$DSM <- factor(Contdata$DSM, levels = c('Irrit_Comb','Irrit_Hyp','Irrit_Int',
                                                   'Mild_Int','Mild_Hyp', 'Mild_Comb',
                                                   'Surg_Int','Surg_Hyp', 'Surg_Comb'))

library(ggnewscale) #ggnewscale provides separate legends for multiple geoms

#Plot sunburst diagram with TMCQ at the center (level 1) and DSM on the edge (level 2)
ggplot(Contdata, aes(y=Frequency)) +
  geom_bar(aes(fill=TMCQ, x=0.5), alpha = 0.8, width=0.5, stat='identity') +
  scale_fill_manual(limits = c('Mild', 'Surgent', 'Irritable'),
                    #Only give breaks and labels to the elements that should appear in the legend
                    breaks = c('Mild','Surgent', 'Irritable'),
                    labels = c('Mild','Surgent','Irritable'),
                    values = c(pal[1:3]), name = "Temperament Class")+
  new_scale_fill() + #provides separate legends for the geoms preceding and following it
  geom_bar(aes(fill=DSM, x=1), alpha = 0.8, width=0.5, stat='identity') +
  #"Limits" designate which levels will be used, "breaks" designate the levels that appear on the legend,  "labels" labels them, and "values" provide colors
  scale_fill_manual(limits = c('Irrit_Comb','Irrit_Hyp','Irrit_Int',
                               'Mild_Int','Mild_Hyp', 'Mild_Comb',
                               'Surg_Int','Surg_Hyp', 'Surg_Comb'),
                    #Only give breaks and labels to the elements that should appear in the legend
                    breaks = c('Surg_Comb','Surg_Hyp','Surg_Int'),
                    labels = c('Combined','Hyperactive', 'Inattentive'),
                    values = c(pal2,pal2[3],pal2[2],pal2[1],pal2[3],pal2[2],pal2[1]), name = "DSM-V Class")+
  coord_polar(theta='y')+ #polar coordinates stretch the y-axis into a circle (e.g., bottom of y axis at center, top of y axis at edge
  theme_minimal()+
  theme(axis.text = element_blank(), panel.grid = element_blank())+
  labs(x = NULL, y = NULL)

ggsave(paste('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Figures & Output','DSM_TMCQ_Sunburst_arranged_imputed_11-22-19.jpeg',sep = "/"), dpi = 600, width = 7, height = 7)
###############################################################################################################

## ALLUVIAL TRAJECTORIES
###############################################################################################################
###############################################################################################################

## TMCQ TRAJECTORIES
###############################################################################################################

#Get TMCQ data in the correct format (get frequency, rename vars, filter NAs, ensure vars are factors)
TMCQdata <- as.data.frame(AggSet) %>%
  #filter(Y1_ADHDsx_subtype != "Subthreshold", Y2_ADHDsx_subtype != "Subthreshold", Y3_ADHDsx_subtype != "Subthreshold")%>%
  dcast(Y1_CD_TMCQmdgrpR + Y2_CD_TMCQmdgrpR + Y3_CD_TMCQmdgrpR ~ "Frequency", fun.aggregate = length) %>%
  rename(Y1 = Y1_CD_TMCQmdgrpR, Y2 = Y2_CD_TMCQmdgrpR, Y3 = Y3_CD_TMCQmdgrpR)%>%
  filter(!is.na(Y1),!is.na(Y2),!is.na(Y3))

#Convert to proportions rather than raw counts
TMCQdata$Prop = prop.table(TMCQdata$Frequency)

#Code for stability (whether consistently labeled) and class to use for color/shading
TMCQdata$Stable <- ifelse(TMCQdata$Y1 == "Irritable" & (TMCQdata$Y1 == TMCQdata$Y2 & TMCQdata$Y2 == TMCQdata$Y3),  "Irritable: Stable",  
                   ifelse(TMCQdata$Y1 == "Irritable" & (TMCQdata$Y1 != TMCQdata$Y2 | TMCQdata$Y2 != TMCQdata$Y3), "Irritable: Unstable",
                   ifelse(TMCQdata$Y1 == "Surgent" & (TMCQdata$Y1 == TMCQdata$Y2 & TMCQdata$Y2 == TMCQdata$Y3),  "Surgent: Stable", 
                   ifelse(TMCQdata$Y1 == "Surgent" & (TMCQdata$Y1 != TMCQdata$Y2 | TMCQdata$Y2 != TMCQdata$Y3), "Surgent: Unstable", 
                   ifelse(TMCQdata$Y1 == "Mild" & (TMCQdata$Y1 == TMCQdata$Y2 & TMCQdata$Y2 == TMCQdata$Y3),  "Mild: Stable", 
                   ifelse(TMCQdata$Y1 == "Mild" & (TMCQdata$Y1 != TMCQdata$Y2 | TMCQdata$Y2 != TMCQdata$Y3),"Mild: Unstable", "?????")))))) 

#Reord strata by changing the factor ordering for Y1 and the label variable, Stable
TMCQdata$Y1 <- factor(TMCQdata$Y1, levels = c("Irritable", "Surgent", "Mild"))
TMCQdata$Stable <- factor(TMCQdata$Stable, levels = c("Irritable: Stable", "Irritable: Unstable", "Surgent: Stable","Surgent: Unstable","Mild: Stable","Mild: Unstable"))


#Code for just stability, ignoring class
TMCQdata$Stable2 <- ifelse(TMCQdata$Y1 == TMCQdata$Y2 & TMCQdata$Y2 == TMCQdata$Y3,  "Stable",  "Unstable") 

#Code for stability using three levels:
#stable = consistent across years, semi-stable = consistent across two consecutive years, unstable = change every year
TMCQdata$Stable3 <-ifelse(TMCQdata$Y1 == TMCQdata$Y2 & TMCQdata$Y2 == TMCQdata$Y3, "Stable", 
       ifelse(TMCQdata$Y1 == TMCQdata$Y2 | TMCQdata$Y2 == TMCQdata$Y3, "Semi-Stable",  "Unstable")) 

#Plot TMCQ class trajaectories as alluvial plots
ggplot(data = TMCQdata,
       aes(axis1 = Y1, axis2 = Y2, axis3 = Y3,
           y = Prop)) +
  scale_x_discrete(expand = c(.1, .05), limits = c("Y1","Y2", "Y3"), labels = c(1,2,3)) +
  xlab("Year") +
  ylab("Proportion")+
  geom_alluvium(aes(fill = Stable),  alpha  = .9, width = .3)+ #if straigt lines add: knot.pos = 0) +
  geom_stratum(size = .75, width = .3,color = "black", fill = c(StablePalette[5],StablePalette[3],StablePalette[1],rep("white", 6)), alpha = c(rep(1,3),rep(0.25, 6))) + geom_text(stat = "stratum", label.strata = TRUE, color = "black") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Change in Classification - Temperament")+
  scale_fill_manual(name = "Y1 Class Stability", values = StablePalette)

#Separating by regulated (mild) vs dysregulated (surgent/irritable)
TMCQdata$Y1_2 <- ifelse(TMCQdata$Y1 == "Surgent" | TMCQdata$Y1 ==  "Irritable",  "Dysregulated",  "Regulated") 
TMCQdata$Y2_2 <- ifelse(TMCQdata$Y2 == "Surgent" | TMCQdata$Y2 ==  "Irritable",  "Dysregulated",  "Regulated") 
TMCQdata$Y3_2 <- ifelse(TMCQdata$Y3 == "Surgent" | TMCQdata$Y3 ==  "Irritable",  "Dysregulated",  "Regulated")  

#Separating by regulated (mild) vs dysregulated (surgent/irritable) and stability
TMCQdata$class2Stable <- ifelse(TMCQdata$Y1_2 == "Dysregulated" & (TMCQdata$Y1_2 == TMCQdata$Y2_2 & TMCQdata$Y2_2 == TMCQdata$Y3_2),  "Dysregulated: Stable",  
                         ifelse(TMCQdata$Y1_2 == "Dysregulated" & (TMCQdata$Y1_2 != TMCQdata$Y2_2 | TMCQdata$Y2_2 != TMCQdata$Y3_2), "Dysregulated: Unstable",
                         ifelse(TMCQdata$Y1_2 == "Regulated" & (TMCQdata$Y1_2 != TMCQdata$Y2_2 | TMCQdata$Y2_2 != TMCQdata$Y3_2), "Regulated: Unstable",  "Regulated: Stable"))) 

#Plot TMCQ class trajaectories as alluvial plots
ggplot(data = TMCQdata,
       aes(axis1 = Y1_2, axis2 = Y2_2, axis3 = Y3_2,
           y = Prop)) +
  scale_x_discrete(expand = c(.1, .05), limits = c("Y1_2","Y2_2", "Y3_2"), labels = c("Y1","Y2", "Y3")) +
  xlab("Year") +
  ylab("Proportion")+
  geom_alluvium(aes(fill = class2Stable), alpha  = .9, width = .3)+ #if straigt lines add: knot.pos = 0); aes.bind = TRUE groups the lodes within strata
  geom_stratum(width = .3, color = "black", fill = c(StablePalette[3],StablePalette[1],rep("white", 4)), alpha = c(rep(1,2),rep(0.25, 4))) + geom_text(stat = "stratum", label.strata = TRUE, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Change in Classification - Temperament")+
  scale_fill_manual(name = "Y1 Class Stability", values = StablePalette, labels = c("Dysregulated: Stable","Dysregulated: Unstable","Regulated: Stable", "Regulated: Unstable"))

#Save plot (this figure type seems to require higher resolution)
ggsave(paste('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Figures & Output','TMCQ_Alluvial_class_stability_impute_11-25-19.jpeg',sep = "/"), dpi = 600, width = 11, height = 8)

###############################################################################################################

## DSM TRAJECTORIES
###############################################################################################################

#Get DSM data in the correct format (get frequency, rename vars, filter NAs and subthreshold class, ensure vars are factors)
DSMdata <- as.data.frame(AggSet) %>%
  dcast(Y1_ADHDsx_subtype + Y2_ADHDsx_subtype + Y3_ADHDsx_subtype ~ "Frequency", fun.aggregate = length) %>%
  rename(Y1 = Y1_ADHDsx_subtype, Y2 = Y2_ADHDsx_subtype, Y3 = Y3_ADHDsx_subtype) %>%
  filter(!is.na(Y1),!is.na(Y2),!is.na(Y3), Y1  != "Subthreshold", Y2  != "Subthreshold", Y3  != "Subthreshold")%>%
  mutate(Y1 = as.factor(Y1), Y2 = as.factor(Y2), Y3 = as.factor(Y3))

#Code for whether paths are clinically logical
DSMdata$Logic = c("CombLog","CombLog","CombLog","CombIll","CombIll", "CombIll", "HypLog", "HypLog","IntIll","IntIll", "IntIll", "IntLog")

#Code for stability (whether consistently labeled) and class to use for color/shading
DSMdata$Stable <- ifelse(DSMdata$Y1 == "Combined" & (DSMdata$Y1 == DSMdata$Y2 & DSMdata$Y2 == DSMdata$Y3),  "Combined: Stable",  
                  ifelse(DSMdata$Y1 == "Combined" & (DSMdata$Y1 != DSMdata$Y2 | DSMdata$Y2 != DSMdata$Y3), "Combined: Unstable",
                  ifelse(DSMdata$Y1 == "Inattentive" & (DSMdata$Y1 == DSMdata$Y2 & DSMdata$Y2 == DSMdata$Y3),"Inattentive: Stable",
                  ifelse(DSMdata$Y1 == "Inattentive" & (DSMdata$Y1 != DSMdata$Y2 | DSMdata$Y2 != DSMdata$Y3), "Inattentive: Unstable", 
                  ifelse(DSMdata$Y1 == "Inattentive" & (DSMdata$Y1 == DSMdata$Y2 & DSMdata$Y2 == DSMdata$Y3),"Inattentive: Stable",
                  ifelse(DSMdata$Y1 == "Hyperactive" & (DSMdata$Y1 != DSMdata$Y2 | DSMdata$Y2 != DSMdata$Y3), "Hyperactive: Unstable",
                  ifelse(DSMdata$Y1 == "Hyperactive" & (DSMdata$Y1 == DSMdata$Y2 & DSMdata$Y2 == DSMdata$Y3), "Hyperactive: Stable", "?????")))))))

#Reord strata by changing the factor ordering for Y1 and the label variable, Stable
DSMdata$Y1 <- factor(DSMdata$Y1, levels = c("Combined", "Inattentive", "Hyperactive"))
DSMdata$Stable <- factor(DSMdata$Stable, levels = c("Combined: Stable", "Combined: Unstable", "Inattentive: Stable","Inattentive: Unstable","Hyperactive: Stable","Hyperactive: Unstable"))


#Code for just stability, ignoring class
DSMdata$Stable2 <- ifelse(DSMdata$Y1 == DSMdata$Y2 & DSMdata$Y2 == DSMdata$Y3, "Stable","Unstable") 

#Code for stability using three levels:
#stable = consistent across years, semi-stable = consistent across two consecutive years, unstable = change every year
DSMdata$Stable3 <- ifelse(DSMdata$Y1 == DSMdata$Y2 & DSMdata$Y2 == DSMdata$Y3, "Stable", 
       ifelse(DSMdata$Y1 == DSMdata$Y2 | DSMdata$Y2 == DSMdata$Y3, "Semi-Stable", "Unstable")) 

#Convert to proportions rather than raw counts
DSMdata$Prop = prop.table(DSMdata$Frequency)

#Plot DSM class trajaectories as alluvial plots using stability and class differentiation
ggplot(data = DSMdata,
                aes(axis1 = Y1, axis2 = Y2, axis3 = Y3,
                y = Prop)) +
  scale_x_discrete(expand = c(.1, .05), limits = c("Y1","Y2", "Y3"), labels = c(1,2,3)) +
  xlab("Year") +
  ylab("Proportion")+
  geom_alluvium(aes(fill = Stable), alpha  = .9, width = .3)+ #if straigt lines add: knot.pos = 0); aes.bind = TRUE groups the lodes within strata
  geom_stratum(size = 0.75, width = .3, color = "black", fill = c(StablePalette[5],StablePalette[3],StablePalette[1],rep("white", 6)), alpha = c(rep(1,3),rep(0.25, 6))) + geom_text(stat = "stratum", label.strata = TRUE, color = "black") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Change in Classification - DSM-V")+
  scale_fill_manual(name = "Y1 Class Stability", values = StablePalette)

#Plot DSM class trajaectories as alluvial plots using stability only
ggplot(data = DSMdata,
       aes(axis1 = Y1, axis2 = Y2, axis3 = Y3,
           y = Prop)) +
  scale_x_discrete(expand = c(.1, .05), limits = c("Y1","Y2", "Y3")) +
  xlab("Year") +
  ylab("Proportion")+
  geom_alluvium(aes(fill = Stable3), alpha  = .7, width = .3)+ #if straigt lines add: knot.pos = 0) +
  geom_stratum(width = .3, color = "grey23", fill = "white", alpha = .25) + geom_text(stat = "stratum", label.strata = TRUE, color = "grey23") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Change in Classification - DSM-V")+
  scale_fill_manual(name = "Class Stability", values = c( AdditionPal,StablePalette[1],StablePalette[2]), labels = c( "Semi-Stable","Stable","Unstable","Mild: Stable", "Mild: Unstable", "Surgent: Stable", "Surgent: Unstable"))

#Save plot (this figure type seems to require higher resolution)
ggsave(paste('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Figures & Output',"DSM_Alluvial_class_stability_impute_11-25-19.jpeg",sep = "/"), dpi = 600, width = 11, height = 8)

#Code for dropping small hyperactive group
DSMdata2 <- DSMdata %>%
  filter(Y1  != "Hyperactive", Y2  != "Hyperactive", Y3  != "Hyperactive")

#Convert to proportions rather than raw counts
DSMdata2$Prop = prop.table(DSMdata2$Frequency)

#Separating by regulated (mild) vs dysregulated (surgent/irritable) and stability
DSMdata2$Stable <- ifelse(DSMdata2$Y1 == "Combined" & (DSMdata2$Y1 == DSMdata2$Y2 & DSMdata2$Y2 == DSMdata2$Y3),  "Combined: Stable",  
                                ifelse(DSMdata2$Y1 == "Combined" & (DSMdata2$Y1 != DSMdata2$Y2 | DSMdata2$Y2 != DSMdata2$Y3), "Combined: Unstable",
                                       ifelse(DSMdata2$Y1 == "Inattentive" & (DSMdata2$Y1 != DSMdata2$Y2 | DSMdata2$Y2 != DSMdata2$Y3), "Inattentive: Unstable",  "Inattentive: Stable"))) 

#Plot DSM class trajaectories as alluvial plots using stability without hyperactive
ggplot(data = DSMdata2,
       aes(axis1 = Y1, axis2 = Y2, axis3 = Y3,
           y = Prop)) +
  scale_x_discrete(expand = c(.1, .05), limits = c("Y1","Y2", "Y3")) +
  xlab("Year") +
  ylab("Proportion")+
  geom_alluvium(aes(fill = Stable), alpha  = .9, width = .3)+ #if straigt lines add: knot.pos = 0) +
  geom_stratum(width = .3, color = "black", fill = c(StablePalette[3],StablePalette[1],rep("white", 4)), alpha = c(rep(1,2),rep(0.25, 4))) + geom_text(stat = "stratum", label.strata = TRUE, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Change in Classification - DSM-V")+
  scale_fill_manual(name = "Y1 Class Stability", values = StablePalette, labels = c("Combined: Stable","Combined: Unstable", "Inattentive: Stable", "Inattentive: Unstable"))

ggsave(paste('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Figures & Output',"DSM_Alluvial_noHyp_stability_impute_11-22-19.jpeg",sep = "/"), dpi = 600, width = 10, height = 7)

#Format alluvial plot using wide format to change strata and alluvia color by level of those vars 
DSMdataLodes <- to_lodes_form(DSMdata, axes = 1:3)

#Plot DSM as alluvial with strata color defined by DSM level using fill= straum, and alluvia color defined by year using geom_flow instead of geom_alluvia
ggplot(data = DSMdataLodes,
       aes(x = x, stratum = stratum, alluvium = alluvium,
           y = Frequency, fill = stratum, label = stratum)) +
  scale_x_discrete(expand = c(.1, .05)) +
  xlab("Year") +
  ylab("Frequency")+
  geom_flow(width = .1, alpha = .6)+ #if straigt lines are desired add: knot.pos = 0
  geom_stratum(width = .1, color = "grey23") + geom_text(stat = "stratum", label.strata = TRUE, color = "white") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Change in Classification - DSM-V")+
  scale_fill_manual(name = "Y1 Class", values = pal, labels = c("Class 0", "Class 1", "Class 2", "Class 3"))

#Save plot (this figure type seems to require higher resolution)
ggsave(paste('H:/Projects/Y1-Y3 Tempt vs DSM Class Vis/Figures & Output',"DSM_Alluvial_colorYear.jpeg",sep = "/"), dpi = 600, width = 7, height = 7)

###############################################################################################################

## LASAGNA PLOT TRAJECTORIES
###############################################################################################################
LongCatData <- data %>%
  rename(Y1_DSM = Y1_ADHDsx_subtype, Y2_DSM = Y2_ADHDsx_subtype, Y3_DSM = Y3_ADHDsx_subtype,
         Y1_TMCQ = Y1_CD_TMCQmdgrpR, Y2_TMCQ = Y2_CD_TMCQmdgrpR, Y3_TMCQ = Y3_CD_TMCQmdgrpR) %>%
  filter(!is.na(Y1_DSM),!is.na(Y2_DSM),!is.na(Y3_DSM),!is.na(Y1_TMCQ),!is.na(Y2_TMCQ),!is.na(Y3_TMCQ))%>%
  mutate(Y4 = NA)

library(longCatEDA)
LongObj <- longCat(dplyr::select(LongCatData, Y1_DSM:Y3_DSM, Y4), Labels = c("DSM0","DSM 1","DSM 2","DSM 3"), tLabels = c("Y1","Y2","Y3",""))
LongObjSort <- sorter(LongObj, group = MplusData$C, groupLabels = c("Low \nIncreasing", "Moderate"))
longCatPlot(LongObj, xlab = "Year", main = "DSM Class by Year", cols = pal, sort = TRUE, lwd=4)
###############################################################################################################

## REGRESSION TABLE PLOT
###############################################################################################################
RegData <- read.csv('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Data/Regression Model.csv', 
                 na.strings = c("NA", -999, ""), fileEncoding="UTF-8-BOM")%>%
  mutate(Sign = ifelse(RegData$p > .05, 1, ifelse(RegData$p < .01, 3, 2)))


ggplot(RegData, aes(Variable, Beta))+
  geom_linerange(aes(ymin = Beta-SE, ymax = Beta+SE, color = factor(Sign)), size = 1)+
  geom_hline(yintercept = 0)+
  geom_point(aes(color = factor(Sign)), size = 4)+
  labs(title = "Beta Coefficients")+
  coord_flip()+
  scale_x_discrete(limits = c("Sex","Age","Baseline ODD Diagnosis","Baseline ADHD Presentation","Temperament Type"))+
  scale_color_manual(limits = c(1, 2, 3), name = "Significance", values = c("#a33e3c","#317840", "#29cc4b"), labels = c("ns", expression(italic("p")~'< .05'), expression(italic("p")~'< .01')))+
  theme_bw()+
  theme(plot.title = element_text(hjust = .5), legend.text = element_text(hjust = 0))

confint(RegData)
###############################################################################################################

## PROPORTION NEW ONSET BY CLASSIFCATION
###############################################################################################################
NewDiag <- read.csv('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Data/ProportionNewDiag.csv', 
                    na.strings = c("NA", -999, ""), fileEncoding="UTF-8-BOM")

#Get proproportions for TMCQ, melt into format for ggplot, and create labels based on class and type 
#Formatted proportions to surround zero, with new onsets being positive and no new onsets being negative to line up edges in figure
NewDiagProp <- NewDiag%>%
  mutate(PropAny = CountNewDx/(CountNewDx + CountNoNewDx), PropNone = -1*CountNoNewDx/(CountNewDx + CountNoNewDx))%>%
  dplyr::select(Type, Class, PropAny,PropNone)%>%
  melt(id.vars = c("Type", "Class"))%>%
  mutate(Label = paste(Type, Class, sep = ": "))

#Plot as proportions
ggplot(filter(NewDiagProp, Type != "EverIrrit"), aes(x = Label, y = value, fill = variable)) +
  geom_bar(position = "identity", stat = "identity")+
  coord_flip()+
  scale_x_discrete(limits = c("DSM: Inattentive", "DSM: Hyperactive", "DSM: Combined","TMCQ: Mild","TMCQ: Surgent", "TMCQ: Irritable"))+
  ylab("Proportion of Individuals With or Without New Diagnosis Onset")+
  xlab("")+  
  theme_bw()+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  scale_fill_manual(values = c(pal3[1], pal[3]), name = "Legend", labels = c("New Onset", "No New Onset"))

ggsave(paste('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Figures & Output','DSM_TMCQ_NewDxOnset_11-20-19.jpeg',sep = "/"), dpi = 600, width = 10, height = 5)


#Create alternative with regulated and dysregulated TMCQ classes
NewDiag2Class <- rbindlist(list(NewDiag,list("TMCQ", "Dysregulated", NewDiag$CountNewDx[NewDiag$Class == "Surgent"]+NewDiag$CountNewDx[NewDiag$Class == "Irritable"],
                                             NewDiag$CountNoNewDx[NewDiag$Class == "Surgent"]+NewDiag$CountNoNewDx[NewDiag$Class == "Irritable"],
                                             NewDiag$CountNewAnx[NewDiag$Class == "Surgent"]+NewDiag$CountNewAnx[NewDiag$Class == "Irritable"],
                                             NewDiag$CountNoNewAnx[NewDiag$Class == "Surgent"]+NewDiag$CountNoNewAnx[NewDiag$Class == "Irritable"])))%>%
  mutate(Class = recode(Class, "Mild" = "Regulated"))

#Get proproportions for regulated/dysregulated, melt into format for ggplot, and create labels based on class and type 
NewDiag2Class2 <- NewDiag2Class[-c(which(NewDiag2Class$Class == "Irritable"),which(NewDiag2Class$Class == "Surgent")),]%>%
  mutate(PropAny = CountNewDx/(CountNewDx + CountNoNewDx), PropNone = -1*CountNoNewDx/(CountNewDx + CountNoNewDx))%>%
  dplyr::select(Type, Class, PropAny,PropNone)%>%
  melt(id.vars = c("Type", "Class"))%>%
  mutate(Label = paste(Type, Class, sep = ": "))

#Get raw counts for regulated/dysregulated, melt into format for ggplot, and create labels based on class and type 
NewDiag2Class3 <- NewDiag2Class[-c(which(NewDiag2Class$Class == "Irritable"),which(NewDiag2Class$Class == "Surgent")),]%>%
  mutate(CountNoNewDx = -1*CountNoNewDx)%>%
  dplyr::select(Type, Class, CountNewDx,CountNoNewDx)%>%
  melt(id.vars = c("Type", "Class"))%>%
  mutate(Label = paste(Type, Class, sep = ": "))

#Plot as proportions or raw counts based on regulated/dysregulated
ggplot(filter(NewDiag2Class3, Type != "EverIrrit"), aes(x = Label, y = value, fill = variable)) +
  geom_bar(position = "identity", stat = "identity")+
  coord_flip()+
  scale_x_discrete(limits = c("DSM: Inattentive", "DSM: Hyperactive", "DSM: Combined","TMCQ: Regulated","TMCQ: Dysregulated"))+
  ylab("Count of Individuals With or Without New Diagnosis Onset:\n
        Larger area indicates more individuals")+
  xlab("")+  
  theme_bw()+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  scale_fill_manual(values = c(pal3[1], pal[3]), name = "Legend", labels = c("New Onset", "No New Onset"))

###############################################################################################################

## SUNBURSTPLOT FOR NEW DIAGNOSIS
###############################################################################################################

#Data collected from cross-tabs file sent by Sarah
NewDiagAny <- read.csv('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Data/ProportionNewDiag.csv', 
                    na.strings = c("NA", -999, ""), fileEncoding="UTF-8-BOM") %>%
  mutate(PropAny = CountNewDx/(CountNewDx + CountNoNewDx))%>%
  dplyr::select(Type, Class, CountNewDx, CountNoNewDx, PropAny)%>%
  

ggplot(filter(NewDiagAny, Type == "TMCQ")) +
  geom_bar(aes(fill=Class, x=0.5, y=filter(NewDiagAny,variable == "CountNewDx"| variable == "CountNoNewDx")), alpha = 0.8, width=0.5, stat='identity') +
  scale_fill_manual(limits = c('Mild',  'Irritable','Surgent'),
                    #Only give breaks and labels to the elements that should appear in the legend
                    breaks = c('Mild','Irritable','Surgent'),
                    labels = c('Mild','Irritable','Surgent'),
                    values = c(pal[1:3]), name = "Temperament Class")+
  new_scale_fill() +
  geom_bar(aes(fill=DSM2, x=1), alpha = 0.8, width=0.5, stat='identity') +
  #"Limits" designate which levels will be used, "breaks" designate the levels that appear on the legend,  "labels" labels them, and "values" provide colors
  scale_fill_manual(limits = c('Irrit_Comb','Irrit_Hyp','Irrit_Int',
                               'Mild_Int','Mild_Hyp', 'Mild_Comb',
                               'Surg_Int','Surg_Hyp', 'Surg_Comb'),
                    #Only give breaks and labels to the elements that should appear in the legend
                    breaks = c('Surg_Comb','Surg_Hyp','Surg_Int'),
                    labels = c('Combined','Hyperactive', 'Inattentive'),
                    values = c(pal2,pal2[3],pal2[2],pal2[1],pal2[3],pal2[2],pal2[1]), name = "DSM-V Class")+
  coord_polar(theta='y')+ #polar coordinates stretch the y-axis into a circle (e.g., bottom of y axis at center, top of y axis at edge
  theme_minimal()+
  theme(axis.text = element_blank(), panel.grid = element_blank())+
  labs(x = NULL, y = NULL)
###############################################################################################################

## Using MI to get more data
###############################################################################################################

#Load imputation package
library(mice)

#Number of imputations

NVisImp <- 50

#Get table 1 data, select and rename relevant variables, joing with TMCQ/DSM dataset, and make sure classifications are factors
Miss_TMCQ_DSM <- read.csv('H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c("",-999,-888, NA),fileEncoding="UTF-8-BOM")%>%
  dplyr::select(MERGEID, ACTIVCONT = Y1_TMCQ_ACTIVCONT, ACTIVIT = Y1_TMCQ_ACTIVITY, AFFIL = Y1_TMCQ_AFFIL, ANGER = Y1_TMCQ_ANGER, 
         FEAR = Y1_TMCQ_FEAR, HIP = Y1_TMCQ_HIP, IMPULS = Y1_TMCQ_IMPULS, INHIBIT = Y1_TMCQ_INHIBIT, SAD = Y1_TMCQ_SAD, 
         SHY = Y1_TMCQ_SHY, SOOTHE = Y1_TMCQ_SOOTHE, ASSERT = Y1_TMCQ_ASSERT, ATTFOC = Y1_TMCQ_ATTFOCUS, LIP = Y1_TMCQ_LIP,            
         PERCEPT = Y1_TMCQ_PERCEPT, DISCOMF = Y1_TMCQ_DISCOMF, OPEN = Y1_TMCQ_OPENNESS,
         ARSHYP = Y1_P_ADHDRS_HYP_TS,	ARSINT = Y1_P_ADHDRS_INT_TS, KSADINT = Y1_KSAD_INTSX,	KSADHYP = Y1_KSAD_HYPSX, IQ = WISCWIAT_FSIQ,
         CONEF = Y1_P_CON3_EF_TS,	CONHYP = Y1_P_CON3_HYP_TS,	CONINT = Y1_P_CON3_INT_TS, SEX) %>%
  right_join(dplyr::select(data,-Y1_ADHD_STATUS), by = "MERGEID")%>%
  mutate(Y1_CD_TMCQmdgrpR = as.factor(Y1_CD_TMCQmdgrpR),Y2_CD_TMCQmdgrpR = as.factor(Y2_CD_TMCQmdgrpR),Y3_CD_TMCQmdgrpR = as.factor(Y3_CD_TMCQmdgrpR),
         Y1_ADHDsx_subtype = as.factor(Y1_ADHDsx_subtype),Y2_ADHDsx_subtype = as.factor(Y2_ADHDsx_subtype),Y3_ADHDsx_subtype = as.factor(Y3_ADHDsx_subtype))

## Extract all variable names in dataset
allVars <- names(Miss_TMCQ_DSM)

## names of variables with missingness
missVars <- names(Miss_TMCQ_DSM)[colSums(is.na(Miss_TMCQ_DSM)) > 0]

## mice predictorMatrix
## A square matrix of size 'ncol(data)' containing 0/1 data specifying the set of predictors to be used for each
## target column. Rows correspond to target variables (i.e.variables to be imputed), in the sequence as they appear in
## data. A value of '1' means that the column variable is used as a predictor for the target variable (in the rows). The
## diagonal of 'predictorMatrix' must be zero. The default for'predictorMatrix' is that all other columns are used as
## predictors (sometimes called massive imputation). Note: For two-level imputation codes '2' and '-2' are also allowed.

predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predictorMatrix) <- allVars
colnames(predictorMatrix) <- allVars

#Variables to inform imputation
imputerVars <- c("ACTIVCONT", "ACTIVIT", "AFFIL", "ANGER", "FEAR", "HIP", "IMPULS", "INHIBIT", "SAD", 
                 "SHY", "SOOTHE", "ASSERT", "ATTFOC", "LIP", "PERCEPT", "DISCOMF", "OPEN",
                 "ARSHYP",	"ARSINT", "KSADINT",	"KSADHYP", "IQ", "CONEF",	"CONHYP",	"CONINT", "SEX", 
                 "Y1_CD_TMCQmdgrpR", "Y2_CD_TMCQmdgrpR", "Y3_CD_TMCQmdgrpR",
                 "Y1_ADHDsx_subtype","Y2_ADHDsx_subtype","Y3_ADHDsx_subtype")
imputerMatrix <- predictorMatrix
imputerMatrix[,imputerVars] <- 1

#Variables to be imputed (must include all missing variables that will be used in imputation as predictors)
imputedVars <- intersect(unique(c(imputerVars)), missVars)
imputedMatrix <- predictorMatrix
imputedMatrix[imputedVars,] <- 1

predictorMatrix <- imputerMatrix * imputedMatrix

#Set diag of matrix to zero
diag(predictorMatrix) <- 0

#Impute data
VisImp <- mice(Miss_TMCQ_DSM, m = NVisImp, predictorMatrix = predictorMatrix, method = "pmm", seed = 12345)

#Get list of issues related to imputation
VisImp$loggedEvents

#Gets list of dataframes for each imputed dataset
ImputationSet <- list()
Test <- rep(NA, NVisImp)
for(i in 1:NVisImp) {
  ImputationSet[[i]] <- complete(VisImp,i)
}

#Function to calculate mode - gets counts of all unique elements of x, then finds the most frequent element(s)
#Randomly selects one of the most frequent elements if there are multiple modes returned
ModeFun <- function(x) sample(names(table(x))[table(x)==max(table(x))], 1)


#Use data.table syntax to bind the imputation dataframes, running the mode function defined above as 
AggSet <- data.table::rbindlist(ImputationSet)[,lapply(.SD, ModeFun), 
                         by = MERGEID, 
                         .SDcols = c("Y1_CD_TMCQmdgrpR", "Y2_CD_TMCQmdgrpR", "Y3_CD_TMCQmdgrpR",
                                     "Y1_ADHDsx_subtype","Y2_ADHDsx_subtype","Y3_ADHDsx_subtype")]

#Comparing missing and non-missing data after imputation

#Pre imputation
ggplot(drop_na(Miss_TMCQ_DSM, Y1_CD_TMCQmdgrpR), aes(x = Y1_CD_TMCQmdgrpR, y = ARSHYP, alpha = Y1_CD_TMCQmdgrpR)) +
  geom_dotplot(binaxis='y', stackdir='center', 
               stackratio = 1, dotsize = 0.5, fill = pal[3], color = pal[3], na.rm = TRUE, method = )+
  theme_bw()+
  scale_alpha_manual(name = "Temperament Class", limits = c("Irritable", "Mild", "Surgent"), values = c(0.25,0.5, 1))+
  ylab("ADHD Rating Scale: Hyperactive t-score")+
  xlab("Y1 Temperament Class")+
  ggtitle("Missing Dataset")+
  theme(plot.title = element_text(hjust = 0.5))

#Post imputation
ggplot(complete(VisImp, 1), aes(x = Y1_CD_TMCQmdgrpR, y = ARSHYP, alpha = Y1_CD_TMCQmdgrpR)) +
  geom_dotplot(binaxis='y', stackdir='center', 
               stackratio = 1, dotsize = 0.5, fill = pal[3], color = pal[3])+
  theme_bw()+
  scale_alpha_manual(name = "Temperament Class", limits = c("Irritable", "Mild", "Surgent"), values = c(0.25,0.5, 1))+
  ylab("ADHD Rating Scale: Hyperactive t-score")+
  xlab("Y1 Temperament Class")+
  ggtitle("Imputed Dataset")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="red")


#Plotting distribution as a ridgeline plot
#Ridgeline plot: Partially overlapping density plots to show change across distributions
library(ggridges)
ggplot(complete(VisImp, 1),  aes(x = ARSHYP, y = factor(Y1_CD_TMCQmdgrpR, levels = c("Mild", "Surgent", "Irritable")))) + #Note that the x/y axis are flipped
  geom_density_ridges(aes(fill = factor(Y1_CD_TMCQmdgrpR, levels = c("Mild", "Surgent", "Irritable"))), size = 1) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Temperament Class")+
  theme_bw()+
  xlab("ADHD Rating Scale: Hyperactive t-score")+
  ylab("Y1 Temperament Class")+
  ggtitle("Imputed Dataset")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

#Save plot
ggsave(paste('H:/Projects/Y1-Y3 TMCQ vs DSM Class Vis/Figures & Output',"Imputed Dataset ADHD RS Hyp by TMCQ Class.jpeg",sep = "/"), dpi = 600, width = 10, height = 7)

###############################################################################################################