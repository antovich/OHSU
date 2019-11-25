#Script to take age-restructured data and run LGCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportion

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)

#Where data file (created below) will go

MplusLocation <- 'H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Mplus Scripts/LGCA/'
MplusDataFile <- 'MplusDataByAge.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Number of classes to run:
FirstClass = 1
LastClass = 5

#Current model number (from Mplus output)
ModelNumber <- 3

#Labels for plot below (make sure order aligns with Mplus input) *** NOTE REMOVED CD4 FOR THIS MODEL
NewLabs <- c("Anxiety Age 8","Anxiety Age 9","Anxiety Age 10","Anxiety Age 11","Anxiety Age 12","Anxiety Age 13","Anxiety Age 14",
             "CD Age 8","CD Age 9","CD Age 10","CD Age 12","CD Age 13","CD Age 14",
             "Mood Age 8","Mood Age 9","Mood Age 10","Mood Age 11","Mood Age 12","Mood Age 13","Mood Age 14",
             "ODD Age 8","ODD Age 9","ODD Age 10","ODD Age 11","ODD Age 12","ODD Age 13","ODD Age 14")

#Prepare data and input files for Mplus
#####################################################################################################

#Take age restructured data created in the "Restructure Data by Age.r" script and modify it for Mplus
#Select subset of variables, rename variables, and order by new variable names
MplusDataByAge <- DataByAge %>%
  select(MERGEID,Age8_DTEAM_MOOD:Age14_DTEAM_CD) %>%
  rename(MOOD1 = Age8_DTEAM_MOOD, ANX1 = Age8_DTEAM_ANX, ODD1 = Age8_DTEAM_ODD, CD1 = Age8_DTEAM_CD,
         MOOD2 = Age9_DTEAM_MOOD, ANX2 = Age9_DTEAM_ANX, ODD2 = Age9_DTEAM_ODD, CD2 = Age9_DTEAM_CD, 
         MOOD3 = Age10_DTEAM_MOOD, ANX3 = Age10_DTEAM_ANX, ODD3 = Age10_DTEAM_ODD, CD3 = Age10_DTEAM_CD, 
         MOOD4 = Age11_DTEAM_MOOD, ANX4 = Age11_DTEAM_ANX, ODD4 = Age11_DTEAM_ODD, CD4 = Age11_DTEAM_CD, 
         MOOD5 = Age12_DTEAM_MOOD, ANX5 = Age12_DTEAM_ANX, ODD5 = Age12_DTEAM_ODD, CD5 = Age12_DTEAM_CD, 
         MOOD6 = Age13_DTEAM_MOOD, ANX6 = Age13_DTEAM_ANX, ODD6 = Age13_DTEAM_ODD, CD6 = Age13_DTEAM_CD,
         MOOD7 = Age14_DTEAM_MOOD, ANX7 = Age14_DTEAM_ANX, ODD7 = Age14_DTEAM_ODD, CD7 = Age14_DTEAM_CD) %>%
  select(MERGEID, order(colnames(.)))

# Designate 5 as missing
MplusDataByAge[MplusDataByAge == 5] <- NA

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(MplusDataByAge, paste(MplusLocation, MplusDataFile, sep =""))

#Create txt file designating iterative Mplus models
# *******NOTE******* Had to drop CD4 as the number of categories were different
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
ANX1-ANX7
CD1-CD7
MOOD1-MOOD7	
ODD1-ODD7;

IDVARIABLE IS MERGEID;

USEVARIABLES ARE 
ANX1-ANX7
CD1-CD7
MOOD1-MOOD7
ODD1-ODD7;

!DEFINE CATEGORICAL INDICATORS 
!(Must specify categories if not identical)

CATEGORICAL ARE 
ANX1-ANX7(*)
MOOD1-MOOD7(*)
ODD1-ODD7(*)
CD1(*) CD2(*) CD3(*) CD4(*) CD5(*) CD6(*) CD7(*);

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

i s | ANX1@0 ANX2@1 ANX3@2 ANX4@3 ANX5@4 ANX6@5 ANX7@6
CD1@0 CD2@1 CD3@2 CD4@3 CD5@4 CD6@5 CD7@6;
MOOD1@0 MOOD2@1 MOOD3@2 MOOD4@3 MOOD5@4 MOOD6@5 MOOD7@6
ODD1@0 ODD2@1 ODD3@2 ODD4@3 ODD5@4 ODD6@5 ODD7@6

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