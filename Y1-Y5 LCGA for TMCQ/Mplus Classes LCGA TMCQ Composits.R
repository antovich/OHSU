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
ModelNumber <- 2

#Prepare data and input files for Mplus
#####################################################################################################

DataByYear <- read.csv( 'H:/Projects/Y1-Y5 LCGA for TMCQ/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c(-999, -888, "NA", ""),fileEncoding="UTF-8-BOM")

#Take age restructured data created in the "Restructure Data by Age.r" script and modify it for Mplus
#Select subset of variables, rename variables, and order by new variable names
MplusDataByYear <- DataByYear %>%
  select(MERGEID,Y1_TMCQ_SURGENCY:Y1_TMCQ_NEGAFFECT, Y2_TMCQ_SURGENCY:Y2_TMCQ_NEGAFFECT,Y3_TMCQ_SURGENCY:Y3_TMCQ_NEGAFFECT,
         Y4_TMCQ_SURGENCY:Y4_TMCQ_NEGAFFECT,Y5_TMCQ_SURGENCY:Y5_TMCQ_NEGAFFECT) %>%
  rename( SURGE1 = Y1_TMCQ_SURGENCY, 
          EFFCONT1 = Y1_TMCQ_EFFCONT, 
          NEGAFFT1 = Y1_TMCQ_NEGAFFECT,
          SURGE2 = Y2_TMCQ_SURGENCY, 
          EFFCONT2 = Y2_TMCQ_EFFCONT, 
          NEGAFFT2 = Y2_TMCQ_NEGAFFECT,
          SURGE3 = Y3_TMCQ_SURGENCY, 
          EFFCONT3 = Y3_TMCQ_EFFCONT, 
          NEGAFFT3 = Y3_TMCQ_NEGAFFECT,
          SURGE4 = Y4_TMCQ_SURGENCY, 
          EFFCONT4 = Y4_TMCQ_EFFCONT, 
          NEGAFFT4 = Y4_TMCQ_NEGAFFECT,
          SURGE5 = Y5_TMCQ_SURGENCY, 
          EFFCONT5 = Y5_TMCQ_EFFCONT, 
          NEGAFFT5 = Y5_TMCQ_NEGAFFECT) %>%
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
EFFCONT1-EFFCONT5
NEGAFFT1-NEGAFFT5
SURGE1-SURGE5;

IDVARIABLE IS MERGEID;

USEVARIABLES ARE 
EFFCONT1-EFFCONT5;

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

i s | EFFCONT1@0 EFFCONT2@1 EFFCONT3@2 EFFCONT4@3 EFFCONT5@4; 

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
MplusOutput <- readModels(MplusLocation)
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
  ggtitle("LGCA TMCQ Negative Affect Y1-Y5")


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

VarNum = length(MplusDataByYear)-1


ClassMeanData <- MplusData %>%
  select(1:5,C)%>%
  melt(id.vars ="C") %>%
  dcast(C + variable ~ ., fun.aggregate = mean, na.rm = TRUE) %>%
  rename("value" = ".")

ClassProp = prop.table(table(MplusData$C))

ClassMeanPlot <- ggplot(ClassMeanData, aes(x = variable, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_point(size = 2)+       
  geom_line(size = 0.75)+
  theme_bw()+
  xlab("Year")+
  ylab("Mean")+
  scale_x_discrete(labels = c(1:6))+
  scale_color_hue(name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                             paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                             paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                             paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                             paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))+
  ggtitle('TMCQ Effortful Control')


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