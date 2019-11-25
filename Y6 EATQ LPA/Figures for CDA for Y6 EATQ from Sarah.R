#Set working drive
setwd('H:/Projects/Y6 EATQ LPA for Sarah n400')

#Load data manipulation and LPA packages
library(tidyverse)
library(tidyLPA)
library(reshape2)

#Set list for variable order and labels in figure
PlotOrd = c("AttFoc","Inhib","ActCont","HIP","Affil","Shy","Aggress","Frust","Depress","Fear")
PlotLab = c("Attention","Inhibitory Control","Activation Control","High Intensity Pleasure",
            "Affiliation","Shyness","Aggression","Frustration","Depressive Mood","Fear")

#Read in data as tidyverse tibble data frame
EATQ <- read_csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Y6 EATQ and Demo data 8-8-19.csv', na = c("", "NA", "-999"))

Y1Data <- read_csv('H:/Projects/Y1 Factor Scores for Mike n1400/Data/all_data_adhd_ML_project_persistent_11-Sep-2018.csv', 
                   na = c("-999","NA",""))

EATQ_CDA <- read_csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Data/ADHDonly community detection group assignments.csv', na = c("", "NA", "-999"))
 #THIS IS THE CORRECTED FILE - ORIGINAL FILE HAD TO SWITCH YEAR COLUMNS BECAUSE IT APPEARS Y1 CLASSES BELONG TO Y6 AND VICE VERSA, UPDATED MERGEID, AND RECODED CLASS "4" to "3"

EATQ <- left_join(EATQ, select(Y1Data, MERGEID, DX), by = "MERGEID") %>%
  rename("Y1_ADHD_STATUS" = DX)

#Selecting controls only (status == 1), only EATQ variables, performing listwise deletion for missing data (all primary data is missing for these anyway)
EATQ_Cont_Y1 <-EATQ %>%
  filter(Y1_ADHD_STATUS == 1) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

EATQ_Cont_Y6 <-EATQ %>%
  filter(Y6_ADHD_STATUS == 1) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Selecting ADHD only (status == 3), only EATQ scales, performing listwise deletion for missing data
EATQ_ADHD_Y1 <-EATQ %>%
  filter(Y1_ADHD_STATUS == 3) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

EATQ_ADHD_Y6 <-EATQ %>%
  filter(Y6_ADHD_STATUS == 3) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Get list of control subject means and SDs by EATQ scale
ContMeans_Y1 = apply(EATQ_Cont_Y1[,2:ncol(EATQ_Cont_Y1)],2,mean)
ContSD_Y1 = apply(EATQ_Cont_Y1[,2:ncol(EATQ_Cont_Y1)],2,sd)

ContMeans_Y6 = apply(EATQ_Cont_Y6[,2:ncol(EATQ_Cont_Y6)],2,mean)
ContSD_Y6 = apply(EATQ_Cont_Y6[,2:ncol(EATQ_Cont_Y6)],2,sd)

#Convert ADHD scale scores to z scores based on control means
EATQ_ADHD_z_Cont_Y1 = cbind(EATQ_ADHD_Y1[,1],sweep(EATQ_ADHD_Y1[,2:ncol(EATQ_ADHD_Y1)],2,ContMeans_Y1,"-")  %>%
                           sweep(2,ContSD_Y1,"/"))

EATQ_ADHD_z_Cont_Y6 = cbind(EATQ_ADHD_Y6[,1],sweep(EATQ_ADHD_Y6[,2:ncol(EATQ_ADHD_Y6)],2,ContMeans_Y6,"-")  %>%
                           sweep(2,ContSD_Y6,"/"))

#For readability and compatibility with Mplus, shorten variable names
names(EATQ_ADHD_z_Cont_Y1) <- c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","Shy","HIP")
names(EATQ_ADHD_z_Cont_Y6) <- c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","Shy","HIP")

#Get dateframe with raw data and classes
ADHD_Classes_Y1 <- left_join(EATQ_ADHD_z_Cont_Y1, select(EATQ_CDA, MERGEID, Class_Y1), by = "MERGEID")
ADHD_Classes_Y6 <- left_join(EATQ_ADHD_z_Cont_Y6, select(EATQ_CDA, MERGEID, Class_Y6), by = "MERGEID")

#Frequency tables for the new class variable
ADHDclassCountY1 = count(ADHD_Classes_Y1, Class_Y1)
ADHDclassCountY1 = cbind(ADHDclassCountY1, percent = prop.table(ADHDclassCountY1$n))

ADHDclassCountY6 = count(ADHD_Classes_Y6, Class_Y6)
ADHDclassCountY6 = cbind(ADHDclassCountY6,percent = prop.table(ADHDclassCountY6$n))


#Melt dataframe into long format (scales as rows)
ADHD_Melt_Y1 <- melt(ADHD_Classes_Y1, id.vars = "Class_Y1", measure.vars = 2:11, variable.name = "Features")
ADHD_Melt_Y6 <- melt(ADHD_Classes_Y6, id.vars = "Class_Y6", measure.vars = 2:11, variable.name = "Features")


#Cast as averages within feature by class
ADHD_Cast_Y1 <- dcast(ADHD_Melt_Y1, Features + Class_Y1~"Mean", mean)
ADHD_Cast_Y1$Class <- as.factor(ADHD_Cast_Y1$Class_Y1)

ADHD_Cast_Y6 <- dcast(ADHD_Melt_Y6, Features + Class_Y6~"Mean", mean)
ADHD_Cast_Y6$Class <- as.factor(ADHD_Cast_Y6$Class_Y6)

# Plot scale averages by profile
adhdPlot_Y1 <- ADHD_Cast_Y1 %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Class, color = Class)) +
  scale_color_hue(name="Profile", labels=c(
    paste("1 (", round(ADHDclassCountY1$percent[1]*100),"%)", sep = ""),
    paste("2 (", round(ADHDclassCountY1$percent[2]*100),"%)", sep = ""),
    paste("3 (", round(ADHDclassCountY1$percent[3]*100),"%)", sep = ""),
    paste("4 (", round(ADHDclassCountY1$percent[4]*100),"%)", sep = ""))) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  ylim(-3,4) +
  #Order of features for figure
  scale_x_discrete(limits = PlotOrd,labels = PlotLab) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = "z-score (control referenced)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5))+
  ggtitle("ADHD Y1 Status Parent EATQ CDA")

# Plot scale averages by profile
adhdPlot_Y6 <- ADHD_Cast_Y6 %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Class, color = Class)) +
  scale_color_hue(name="Profile", labels=c(
    paste("1 (", round(ADHDclassCountY6$percent[1]*100),"%)", sep = ""),
    paste("2 (", round(ADHDclassCountY6$percent[2]*100),"%)", sep = ""),
    paste("3 (", round(ADHDclassCountY6$percent[3]*100),"%)", sep = ""),
    paste("4 (", round(ADHDclassCountY6$percent[4]*100),"%)", sep = ""))) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  ylim(-3,4) +
  #Order of features for figure
  scale_x_discrete(limits = PlotOrd, labels = PlotLab) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = "z-score (control referenced)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle("ADHD Y6 Status Parent EATQ CDA")
