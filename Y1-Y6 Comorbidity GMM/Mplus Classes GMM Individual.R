#Script to take age-restructured data and run LGCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportion

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)

#Where data file (created below) will go
MplusLocation <- 'H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Mplus Scripts/LGCA/Cluster'
MplusDataFile <- 'MplusDataByAge.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Number of classes to run:
FirstClass = 2
LastClass = 4

#Number of classes
ModelNumber <- 3

#Select the range of ages for the analysis
AgeRange = 8:14

#Add quadratic to the LCGA model?
Quad = FALSE


#Prepare data and input files for Mplus
#####################################################################################################

#Take age restructured data created in the "Restructure Data by Age.r" script and modify it for Mplus
#Select rename all relevant variables then select a subset. Note, the order in select() must be maintained to match the Mplus headers (below)
MplusDataByAge <- DataByAge %>%
  rename(MOOD7 = Age7_DTEAM_MOOD, ANX7 = Age7_DTEAM_ANX, ODD7 = Age7_DTEAM_ODD, CD7 = Age7_DTEAM_CD,
         MOOD8 = Age8_DTEAM_MOOD, ANX8 = Age8_DTEAM_ANX, ODD8 = Age8_DTEAM_ODD, CD8 = Age8_DTEAM_CD,
         MOOD9 = Age9_DTEAM_MOOD, ANX9 = Age9_DTEAM_ANX, ODD9 = Age9_DTEAM_ODD, CD9 = Age9_DTEAM_CD, 
         MOOD10 = Age10_DTEAM_MOOD, ANX10 = Age10_DTEAM_ANX, ODD10 = Age10_DTEAM_ODD, CD10 = Age10_DTEAM_CD, 
         MOOD11 = Age11_DTEAM_MOOD, ANX11 = Age11_DTEAM_ANX, ODD11 = Age11_DTEAM_ODD, CD11 = Age11_DTEAM_CD, 
         MOOD12 = Age12_DTEAM_MOOD, ANX12 = Age12_DTEAM_ANX, ODD12 = Age12_DTEAM_ODD, CD12 = Age12_DTEAM_CD, 
         MOOD13 = Age13_DTEAM_MOOD, ANX13 = Age13_DTEAM_ANX, ODD13 = Age13_DTEAM_ODD, CD13 = Age13_DTEAM_CD,
         MOOD14 = Age14_DTEAM_MOOD, ANX14 = Age14_DTEAM_ANX, ODD14 = Age14_DTEAM_ODD, CD14 = Age14_DTEAM_CD,
         MOOD15 = Age15_DTEAM_MOOD, ANX15 = Age15_DTEAM_ANX, ODD15 = Age15_DTEAM_ODD, CD15 = Age15_DTEAM_CD,
         MOOD16 = Age16_DTEAM_MOOD, ANX16 = Age16_DTEAM_ANX, ODD16 = Age16_DTEAM_ODD, CD16 = Age16_DTEAM_CD,
         MOOD17 = Age17_DTEAM_MOOD, ANX17 = Age17_DTEAM_ANX, ODD17 = Age17_DTEAM_ODD, CD17 = Age17_DTEAM_CD,
         MOOD18 = Age18_DTEAM_MOOD, ANX18 = Age18_DTEAM_ANX, ODD18 = Age18_DTEAM_ODD, CD18 = Age18_DTEAM_CD,
         MOOD19 = Age19_DTEAM_MOOD, ANX19 = Age19_DTEAM_ANX, ODD19 = Age19_DTEAM_ODD, CD19 = Age19_DTEAM_CD,) %>%
  select(MERGEID, paste0("ANX",AgeRange),paste0("CD",AgeRange),paste0("MOOD",AgeRange),paste0("ODD",AgeRange))


# Designate 5 as missing or as subthreshold
MplusDataByAge[MplusDataByAge == 5] <- 2

MplusDataByAge$FAMID <- as.numeric(substr(MplusDataByAge$MERGEID, start = 1, stop =5 ))

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(MplusDataByAge, paste(MplusLocation, MplusDataFile, sep ="/"))

#Create txt file designating iterative Mplus models
writeLines(paste(
'[[init]]
iterators = classes outcome;
classes = ',FirstClass,':',LastClass,';
outcome = 1:4;
Measure#outcome = ANX CD MOOD ODD;
filename = "[[classes]]-class GMM [[Measure#outcome]].inp";
outputDirectory = "', MplusLocation, '";
[[/init]]

TITLE: LCGA Comparison
DATA: FILE = "', paste(MplusLocation, MplusDataFile,sep = "/"), '";

VARIABLE: 
NAMES ARE
MERGEID ',
paste0('ANX', AgeRange[1],'-ANX', last(AgeRange)),' ',
paste0('CD', AgeRange[1],'-CD', last(AgeRange)),' ', 
paste0("MOOD", AgeRange[1],"-MOOD", last(AgeRange)),' ',
paste0("ODD", AgeRange[1],"-ODD", last(AgeRange)),';

IDVARIABLE IS MERGEID;

USEVARIABLES ARE 
[[Measure#outcome]]', AgeRange[1],'-[[Measure#outcome]]', last(AgeRange),';

!DEFINE CATEGORICAL INDICATORS 
!(Must specify categories if not identical)

CATEGORICAL ARE 
',paste0('[[Measure#outcome]]',AgeRange,'(*) ', collapse = " "),';

!DEFINE # OF CLASSES
CLASSES are c([[classes]]);

!DEFINE MISSING DATA CODE
MISSING = .;

ANALYSIS:
!ESTIMATE A MIXTURE MODEL
TYPE = MIXTURE;
ALGORITHM = INTEGRATION;

!DEFINE MULTIPLE RANDOM STARTS
STARTS = 100 50;

MODEL:
%OVERALL%

i s ',ifelse(Quad, 'q', ''),' | [[Measure#outcome]]8@0 [[Measure#outcome]]9@1 
[[Measure#outcome]]10@2 [[Measure#outcome]]11@3 [[Measure#outcome]]12@4 
[[Measure#outcome]]13@5 [[Measure#outcome]]14@6

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT
TECH1 TECH4 TECH11 TECH12 TECH14;

SAVEDATA:
FILE IS "[[classes]]-class GMM [[Measure#outcome]] Ouput.csv";
SAVE = CPROB;'
, sep = ""), paste(MplusLocation, MplusLoopFile, sep ="/"))

#Uses the script above to create individual input files for Mplus
createModels(paste(MplusLocation, MplusLoopFile, sep ="/"))
#####################################################################################################

#Run input files in Mplus and extract the model data
#####################################################################################################

#Runs all the input files in the directory
runModels(MplusLocation, replaceOutfile = "modifiedDate")

#Get output from Mplus output files
MplusOutput <- readModels(MplusLocation)
#####################################################################################################

#Specify measure (ODD, MOOD, CD, ANX)
Measure = "MOOD"

#Get correctly formatted label for plots
ifelse(Measure == "MOOD", VarLab <- "Mood", ifelse(Measure == "ANX", VarLab <- "Anxiety", VarLab <- Measure))

#Get fit indices for all models related to a specific variable
#####################################################################################################

#Build a dataframe with all the summary/fit indice information for each model using the current measure 
SummariesAll <- NULL
for (i in paste0("X",FirstClass:LastClass,".class.lgca.",tolower(Measure),".out")) {
  SummariesAll <- bind_rows(SummariesAll, MplusOutput[[i]]$summaries)
} 

#Get the create a number of classes variable from the Mplus output filename, 
#select relevant fit indices, melt into long format and set class variable as numeric
Summaries <- SummariesAll %>%
  tidyr::extract(Filename, into = c("Classes"), regex = "([0-9]+)") %>%
  select(Classes, AIC, BIC, aBIC, Entropy, VLMR_pVal = T11_VLMR_PValue, BLRT_pVal = BLRT_PValue) %>%
  melt(id.vars = "Classes")%>%
  mutate(Classes = as.numeric(Classes))

#Plot the fit indices collected above
FitPlots <- ggplot(Summaries,aes(Classes,value))+
  geom_point(size =2, color = "#00AFBB")+
  geom_line(size = 1.25, color = "#00AFBB")+
  theme_bw(base_size = 10, base_family = "sans") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size = 8))+
  facet_grid(variable ~ ., scales= "free_y") +
  scale_x_continuous(name = "Number of Classes", breaks = seq(FirstClass,LastClass)) +
  ylab("Indice Value") +
  ggtitle(paste0("LGCA ", VarLab))


#####################################################################################################

#Examine relevant Mplus output for the current model:
#####################################################################################################
MplusOutput[[paste0("X",ModelNumber,".class.lgca.",tolower(Measure),".out")]]$covariance_coverage
MplusOutput[[paste0("X",ModelNumber,".class.lgca.",tolower(Measure),".out")]]$errors
MplusOutput[[paste0("X",ModelNumber,".class.lgca.",tolower(Measure),".out")]]$warnings
MplusOutput[[paste0("X",ModelNumber,".class.lgca.",tolower(Measure),".out")]]$summaries

#####################################################################################################

#Plot class output from Mplus for the current model
#####################################################################################################

#Get class output from Mplus file
MplusData <- MplusOutput[[paste0("X",ModelNumber,".class.gmm.",tolower(Measure),".out")]]$savedata

VarNum = length(AgeRange)

#Recode back to original category values
MplusData[1:VarNum][MplusData[1:VarNum] == 2] <-3
MplusData[1:VarNum][MplusData[1:VarNum] == 1] <-2
MplusData[1:VarNum][MplusData[1:VarNum] == 0] <-1

ClassMeanData <- MplusData %>%
  select(1:VarNum,C)%>%
  melt(id.vars ="C") %>%
  dcast(C + variable ~ ., fun.aggregate = mean, na.rm = TRUE) %>%
  tidyr::extract(variable, into = c("Variable", "AgeSequence"), regex = "([A-Z]+)([0-9]+)") %>%
  rename("value" = ".") %>%
  mutate(AgeSequence = as.numeric(AgeSequence))

ClassProp = prop.table(table(MplusData$C))

ClassMeanPlot <- ggplot(ClassMeanData, aes(x = AgeSequence, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_point(size = 2)+       
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Age")+
  ylab("Mean")+
  ggtitle(VarLab)+
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)+
  scale_y_continuous(limits = c(0.5, 3.5))+
  scale_color_hue(name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                             paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                             paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                             paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                             paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))

library(longCatEDA)
LongObj <- longCat(select(MplusData, 1:VarNum), Labels = c("Absent","Subthresh","Present"), tLabels = AgeRange)
LongObjSort <- sorter(LongObj, group = MplusData$C, groupLabels = c("Low \nIncreasing", "Moderate"))
longCatPlot(LongObjSort, xlab = "Age", main = VarLab, groupBuffer = 0.5)

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