#Script to take age-restructured data and run LGCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportion

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)

#Where data file (created below) will go

MplusLocation <- 'H:/Projects/Y1-Y5 LCGA for TMCQ/Mplus Scripts'
MplusDataFile <- 'MplusDataByYear.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Number of classes to run:
FirstClass = 1
LastClass = 5

#Current model number (from Mplus output)
ModelNumber <- 3

#Prepare data and input files for Mplus
#####################################################################################################

DataByYear <- read.csv( 'H:/Projects/Y1-Y5 LCGA for TMCQ/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c(-999, -888, "NA", ""),fileEncoding="UTF-8-BOM")

#Take age restructured data created in the "Restructure Data by Age.r" script and modify it for Mplus
#Select subset of variables, rename variables, and order by new variable names
MplusDataByYear <- DataByYear %>%
  select(MERGEID,Y1_TMCQ_ACTIVCONT:Y1_TMCQ_OPENNESS, Y2_TMCQ_ACTIVCONT:Y2_TMCQ_OPENNESS,Y3_TMCQ_ACTIVCONT:Y3_TMCQ_OPENNESS,
         Y4_TMCQ_ACTIVCONT:Y4_TMCQ_OPENNESS,Y5_TMCQ_ACTIVCONT:Y5_TMCQ_OPENNESS) %>%
  rename( ActCont1 = Y1_TMCQ_ACTIVCONT, 
          Activ1 = Y1_TMCQ_ACTIVITY, 
          Affil1 = Y1_TMCQ_AFFIL,
          Anger1 = Y1_TMCQ_ANGER,
          Fear1 = Y1_TMCQ_FEAR,
          HIP1 = Y1_TMCQ_HIP,
          Impuls1 = Y1_TMCQ_IMPULS,
          Inhib1 = Y1_TMCQ_INHIBIT,
          Sad1 = Y1_TMCQ_SAD,
          Shy1 = Y1_TMCQ_SHY,
          Sooth1 = Y1_TMCQ_SOOTHE,
          Assert1 = Y1_TMCQ_ASSERT,
          Focus1 = Y1_TMCQ_ATTFOCUS,
          LIP1 = Y1_TMCQ_LIP,
          Percep1 = Y1_TMCQ_PERCEPT,
          Discomf1 = Y1_TMCQ_DISCOMF,
          Open1 = Y1_TMCQ_OPENNESS,
          ActCont2 = Y2_TMCQ_ACTIVCONT,
          Activ2 = Y2_TMCQ_ACTIVITY,
          Affil2 = Y2_TMCQ_AFFIL,
          Anger2 = Y2_TMCQ_ANGER,
          Fear2 = Y2_TMCQ_FEAR,
          HIP2 = Y2_TMCQ_HIP,
          Impuls2 = Y2_TMCQ_IMPULS,
          Inhib2 = Y2_TMCQ_INHIBIT,
          Sad2 = Y2_TMCQ_SAD,
          Shy = Y2_TMCQ_SHY,
          Sooth2 = Y2_TMCQ_SOOTHE,
          Assert2 = Y2_TMCQ_ASSERT,
          Focus2 = Y2_TMCQ_ATTFOCUS,
          LIP2 = Y2_TMCQ_LIP,
          Percep2 = Y2_TMCQ_PERCEPT,
          Discomf2 = Y2_TMCQ_DISCOMF,
          Open2 = Y2_TMCQ_OPENNESS,
          ActCont3 = Y3_TMCQ_ACTIVCONT,
          Activ3 = Y3_TMCQ_ACTIVITY,
          Affil3 = Y3_TMCQ_AFFIL,
          Anger3 = Y3_TMCQ_ANGER,
          Fear3 = Y3_TMCQ_FEAR,
          HIP3 = Y3_TMCQ_HIP,
          Impuls3 = Y3_TMCQ_IMPULS, 
          Inhib3 = Y3_TMCQ_INHIBIT,
          Sad3 = Y3_TMCQ_SAD,
          Shy3 = Y3_TMCQ_SHY,
          Sooth3 = Y3_TMCQ_SOOTHE,
          Assert3 = Y3_TMCQ_ASSERT,
          Focus3 = Y3_TMCQ_ATTFOCUS,
          LIP3 = Y3_TMCQ_LIP,
          Percep3 = Y3_TMCQ_PERCEPT,
          Discomf3 = Y3_TMCQ_DISCOMF,
          Open3 = Y3_TMCQ_OPENNESS,
          ActCont4 = Y4_TMCQ_ACTIVCONT,
          Activ4 = Y4_TMCQ_ACTIVITY,
          Affil4 = Y4_TMCQ_AFFIL,
          Anger4 = Y4_TMCQ_ANGER,
          Fear4 = Y4_TMCQ_FEAR,
          HIP4 = Y4_TMCQ_HIP,
          Impuls4 = Y4_TMCQ_IMPULS,
          Inhib4 = Y4_TMCQ_INHIBIT,
          Sad4 = Y4_TMCQ_SAD,
          Shy4 = Y4_TMCQ_SHY,
          Sooth4 = Y4_TMCQ_SOOTHE,
          Assert4 = Y4_TMCQ_ASSERT,
          Focus4 = Y4_TMCQ_ATTFOCUS,
          LIP4 = Y4_TMCQ_LIP,
          Percep4 = Y4_TMCQ_PERCEPT,
          Discomf4 = Y4_TMCQ_DISCOMF,
          Open4 = Y4_TMCQ_OPENNESS,
          ActCont5 = Y5_TMCQ_ACTIVCONT,
          Activ5 = Y5_TMCQ_ACTIVITY,
          Affil5 = Y5_TMCQ_AFFIL,
          Anger5 = Y5_TMCQ_ANGER,
          Fear5 = Y5_TMCQ_FEAR,
          HIP5 = Y5_TMCQ_HIP,
          Impuls5 = Y5_TMCQ_IMPULS,
          Inhib5 = Y5_TMCQ_INHIBIT,
          Sad5 = Y5_TMCQ_SAD,
          Shy5 = Y5_TMCQ_SHY,
          Sooth5 = Y5_TMCQ_SOOTHE,
          Assert5 = Y5_TMCQ_ASSERT,
          Focus5 = Y5_TMCQ_ATTFOCUS,
          LIP5 = Y5_TMCQ_LIP,
          Percep5 = Y5_TMCQ_PERCEPT,
          Discomf5 = Y5_TMCQ_DISCOMF,
          Open5 = Y5_TMCQ_OPENNESS) %>%
  select(MERGEID, order(colnames(.)))

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(MplusDataByYear, paste(MplusLocation, MplusDataFile, sep =""))

#Create txt file designating iterative Mplus models

writeLines(paste(
'[[init]]
iterators = classes;
classes = ',FirstClass,':',LastClass,';
filename = "[[classes]]-class LGCA.inp";
outputDirectory = "', MplusLocation, '";
[[/init]]

TITLE: LCGA Comparison
DATA: FILE = "', MplusLocation, MplusDataFile,'";

VARIABLE: 
NAMES ARE
MERGEID	
ActCon1-ActCon5
Activ1-Activ5
Affil1-Affil5
Anger1-Anger5
Assert1-Assert5
Discomf1-Discomf5
Fear1-Fear5
Focus1-Focus5
HIP1-HIP5
Impuls1-Impuls5
Inhib1-Inhib5
LIP1-LIP5
Open1-Open5
Percep1-Percep5
Sad1-Sad5
Shy1-Shy5
Sooth1-Sooth5;

IDVARIABLE IS MERGEID;

USEVARIABLES ARE 
ActCon1-ActCon5
Activ1-Activ5
Affil1-Affil5
Anger1-Anger5
Assert1-Assert5
Discomf1-Discomf5
Fear1-Fear5
Focus1-Focus5
HIP1-HIP5
Impuls1-Impuls5
Inhib1-Inhib5
LIP1-LIP5
Open1-Open5
Percep1-Percep5
Sad1-Sad5
Shy1-Shy5
Sooth1-Sooth5;

!DEFINE # OF CLASSES
CLASSES are c([[classes]]);

!DEFINE MISSING DATA CODE
MISSING = .;

ANALYSIS:
!ESTIMATE A MIXTURE MODEL
TYPE = MIXTURE;

!DEFINE MULTIPLE RANDOM STARTS
STARTS = 100 50;

MODEL:
%OVERALL%

i s | ActCon1@0 ActCon2@1 ActCon3@2 ActCon4@3 ActCon5@4 
Activ1@0 Activ2@1 Activ3@2 Activ4@3 Activ5@4 
Affil1@0 Affil2@1 Affil3@2 Affil4@3 Affil5@4 
Anger1@0 Anger2@1 Anger3@2 Anger4@3 Anger5@4  
Assert1@0 Assert2@1 Assert3@2 Assert4@3 Assert5@4 
Discomf1@0 Discomf2@1 Discomf3@2 Discomf4@3 Discomf5@4 
Fear1@0 Fear2@1 Fear3@2 Fear4@3 Fear5@4 
Focus1@0 Focus2@1 Focus3@2 Focus4@3 Focus5@4 
HIP1@0 HIP2@1 HIP3@2 HIP4@3 HIP5@4 
Impuls1@0 Impuls2@1 Impuls3@2 Impuls4@3 Impuls5@4 
Inhib1@0 Inhib2@1 Inhib3@2 Inhib4@3 Inhib5@4 
LIP1@0 LIP2@1 LIP3@2 LIP4@3 LIP5@4 
Open1@0 Open2@1 Open3@2 Open4@3 Open5@4 
Percep1@0 Percep2@1 Percep3@2 Percep4@3 Percep5@4 
Sad1@0 Sad2@1 Sad3@2 Sad4@3 Sad5@4 
Shy1@0 Shy2@1 Shy3@2 Shy4@3 Shy5@4 
Sooth1@0 Sooth2@1 Sooth3@2 Sooth4@3 Sooth5@4; 

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT
TECH1 TECH4 TECH11 TECH14;

SAVEDATA:
FILE IS "[[classes]]-class LGCA Ouput.csv";
SAVE = CPROB;'
, sep = ""), paste(MplusLocation, MplusLoopFile, sep =""))

#Uses the script above to create individual input files for Mplus
createModels(paste(MplusLocation, MplusLoopFile, sep =""))
#####################################################################################################

#Run input files in Mplus and extract the model data
#####################################################################################################

#Runs all the input files in the directory
runModels(MplusLocation, replaceOutfile = "modifiedDate")

#Get output from Mplus output files
MplusOutput <- readModels(str_replace(MplusLocation, "LGCA/", "LGCA"))
#####################################################################################################

#Get fit indices for all models in the directory
#####################################################################################################

Summaries <- NULL
  
for (i in seq(FirstClass,LastClass)) {
  Summaries <- bind_rows(Summaries, MplusOutput[[i]]$summaries)
} 

Summaries <- Summaries %>%
  tidyr::extract(Filename, into = c("Classes"), regex = "([0-9]+)") %>%
  select(Classes, AIC, BIC, aBIC, Entropy, VLMR_pVal = T11_VLMR_PValue, BLRT_pVal = BLRT_PValue) %>%
  melt(id.vars = "Classes")%>%
  mutate(Classes = as.numeric(Classes))

FitPlots <- ggplot(Summaries,aes(Classes,value))+
  geom_point(size =2, color = "#00AFBB")+
  geom_line(size = 1.25, color = "#00AFBB")+
  theme_bw(base_size = 10, base_family = "sans") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size = 8))+
  facet_grid(variable ~ ., scales= "free_y") +
  scale_x_continuous(name = paste("Number of Classes"), breaks = seq(FirstClass,LastClass)) +
  ylab("Indice Value") +
  ggtitle("LGCA")


#####################################################################################################

#Examine relevant Mplus output for the current model:
#####################################################################################################
MplusOutput[[ModelNumber]]$covariance_coverage
MplusOutput[[ModelNumber]]$errors
MplusOutput[[ModelNumber]]$warnings
MplusOutput[[ModelNumber]]$summaries

#####################################################################################################

#Plot class output from Mplus for the current model
#####################################################################################################

#Get class output from Mplus file
MplusData <- MplusOutput[[ModelNumber]]$savedata

#Grab number of variables from data loading above **FIXED FOR CD4****
VarNum = length(MplusDataByAge)-2

#Recode back to original category values
MplusData[1:VarNum][MplusData[1:VarNum] == 2] <-3
MplusData[1:VarNum][MplusData[1:VarNum] == 1] <-2
MplusData[1:VarNum][MplusData[1:VarNum] == 0] <-1

ClassMeanData <- MplusData %>%
  select(ANX1,ANX2,ANX3,ANX4,ANX5,ANX6,ANX7,  
         CD1,CD2,CD3,CD5,CD6,CD7,   
         MOOD1,MOOD2,MOOD3,MOOD4,MOOD5,MOOD6,MOOD7,
         ODD1,ODD2,ODD3,ODD4,ODD5,ODD6,ODD7,C)%>%
  melt(id.vars ="C") %>%
  dcast(C + variable ~ ., fun.aggregate = mean, na.rm = TRUE) %>%
  rename("value" = ".")

ClassProp = prop.table(table(MplusData$C))

ClassMeanPlot <- ggplot(ClassMeanData, aes(x = variable, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_point(size = 2)+       
  geom_line(size = 0.75)+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  xlab("Variable and Year")+
  ylab("Mean")+
  scale_x_discrete(labels = c(NewLabs))+
  scale_color_hue(name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                             paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                             paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                             paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                             paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))


ClassPropData <- MplusData %>%
  select(ANX1,ANX2,ANX3,ANX4,ANX5,ANX6,ANX7,  
         CD1,CD2,CD3,CD5,CD6,CD7,   
         MOOD1,MOOD2,MOOD3,MOOD4,MOOD5,MOOD6,MOOD7,
         ODD1,ODD2,ODD3,ODD4,ODD5,ODD6,ODD7,C)%>%
  melt(id.vars ="C") %>%
  tidyr::extract(variable, into = c("Variable", "AgeSequence"), regex = "([A-Z]+)([0-9]+)")

plot_list <- list()
a <- 1
Var <- NA
for(k in 1:length(unique(ClassPropData$C))){
  for (v in c("MOOD", "ANX", "ODD", "CD")) {
    ifelse(v == "MOOD", Lab <- "Mood", ifelse(v == "ANX", Lab <- "Anxiety", Lab <- v))
    p <- ggplot(filter(ClassPropData, C == sort(unique(ClassPropData$C))[k], Variable == v, !is.na(value)), aes(x = AgeSequence, fill = as.factor(value))) +
            geom_bar(position = "fill", width = 0.8) +
            theme_bw()+
            theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))+
            ggtitle(paste("Class ", k, " (", round(ClassProp[k]*100),"%) : ", Lab, sep = ""))+
            xlab("Age")+
            ylab("Percent")+
            scale_fill_manual(name = "Category", labels = c("Absent", "Subthreshold", "Present"), values = c("#00AFBB", "#E7B800", "#FC4E07") )+
            scale_y_continuous(labels = scales::percent_format())+
            scale_x_discrete(labels = 8:14)
    plot_list[[a]] = p
    a = a+1
  }
}

library(cowplot)

plot_grid(plotlist = plot_list, nrow = length(unique(ClassPropData$C)))

#####################################################################################################