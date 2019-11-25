#Script to take year-based data and run LGCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportion

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)

#Where data file (created below) will go
MplusLocation <- 'H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Mplus Scripts/LCGA/Year/Restricted variance'
MplusDataFile <- 'MplusDataByAge.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Number of classes to run:
FirstClass = 1
LastClass = 5

#Number of classes
ModelNumber <- 2

#Select the range of testing years for the analysis
YearRange = 1:6

#Add quadratic to the LCGA model?
Quad = FALSE

#Blue palette for categorical data
pal = c("#a8deff","#64bdf5","#2172ed","#0048b5")

#Sequential palette for means data
pal2 = c('#30B8DA','#FF6347','#2FCA72','#FFC34A')

#Read in data
#####################################################################################################

DataByYear <- read.csv('H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c("",-999,-888, NA),fileEncoding="UTF-8-BOM")%>%
  select(MERGEID, AGE1 = V1AGEYEARS, AGE2 = Y2AGEYEARS, AGE3 = Y3AGEYEARS, AGE4 = Y4AGEYEARS, AGE5 = Y5AGEYEARS, AGE6 = Y6AGEYEARS,
         MOOD1 = Y1_DTEAM_MOOD, MOOD2 = Y2_DTEAM_MOOD, MOOD3 = Y3_DTEAM_MOOD, MOOD4 = Y4_DTEAM_MOOD, MOOD5 = Y5_DTEAM_MOOD, MOOD6 = Y6_DTEAM_MOOD,
         ANX1 = Y1_DTEAM_ANX, ANX2 = Y2_DTEAM_ANX, ANX3 = Y3_DTEAM_ANX, ANX4 = Y4_DTEAM_ANX, ANX5 = Y5_DTEAM_ANX, ANX6 = Y6_DTEAM_ANX,
         ODD1 = Y1_DTEAM_ODD, ODD2 = Y2_DTEAM_ODD, ODD3 = Y3_DTEAM_ODD, ODD4 = Y4_DTEAM_ODD,ODD5 = Y5_DTEAM_ODD, ODD6 = Y6_DTEAM_ODD, 
         CD1 = Y1_DTEAM_CD, CD2 = Y2_DTEAM_CD, CD3 = Y3_DTEAM_CD, CD4 = Y4_DTEAM_CD, CD5 = Y5_DTEAM_CD, CD6 = Y6_DTEAM_CD)

#####################################################################################################

#Prepare data and input files for Mplus
#####################################################################################################

# Designate 5 as missing or as subthreshold
DataByYear[DataByYear == 5] <- 2

#Creates FAMID variable for clustering in Mplus
DataByYear$FAMID <- as.numeric(substr(DataByYear$MERGEID, start = 1, stop =5 ))

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(DataByYear, paste(MplusLocation, MplusDataFile, sep ="/"))

#Create txt file designating iterative Mplus models
writeLines(paste(
  '[[init]]
iterators = classes outcome;
classes = ',FirstClass,':',LastClass,';
outcome = 1:4;
Measure#outcome = MOOD ANX ODD CD;
filename = "[[classes]]-class LCGA By Year [[Measure#outcome]].inp";
outputDirectory = "', MplusLocation, '";
[[/init]]

TITLE: LCGA Comparison Year Data
DATA: FILE = "', paste(MplusLocation, MplusDataFile,sep = "/"), '";

VARIABLE: 
NAMES ARE
MERGEID ',
paste0("AGE", YearRange[1],"-AGE", last(YearRange)),' ',
paste0("MOOD", YearRange[1],"-MOOD", last(YearRange)),' ',
paste0('ANX', YearRange[1],'-ANX', last(YearRange)),' ',
paste0("ODD", YearRange[1],"-ODD", last(YearRange)),' ',
paste0('CD', YearRange[1],'-CD', last(YearRange)),' 
FAMID;

IDVARIABLE IS MERGEID;

CLUSTER IS FAMID;

USEVARIABLES ARE 
[[Measure#outcome]]', YearRange[1],'-[[Measure#outcome]]', last(YearRange),paste0(" AGE", YearRange[1],"-AGE", last(YearRange)),';

TSCORES ARE ', paste0("AGE", YearRange[1],"-AGE", last(YearRange)),';

!DEFINE CATEGORICAL INDICATORS 
!(Must specify categories if not identical)

CATEGORICAL ARE 
',paste0('[[Measure#outcome]]',YearRange,'(*)', collapse = " "),';

!DEFINE # OF CLASSES
CLASSES are c([[classes]]);

!DEFINE MISSING DATA CODE
MISSING = .;

ANALYSIS:
!ESTIMATE A MIXTURE MODEL
TYPE = RANDOM MIXTURE COMPLEX;

ALGORITHM = INTEGRATION;

!DEFINE MULTIPLE RANDOM STARTS
STARTS = 200 50;

MODEL:
%OVERALL%

i s ',ifelse(Quad, 'q', ''),' | ', paste0('[[Measure#outcome]]',YearRange, collapse = " "),' AT ', paste0('AGE',YearRange, collapse = " "), '; 

!Restrict variance and covariance for LCGA 
!(necessary with inclusion of ALGORITHM = INTEGRATION; otherwise can be left out)

i@0; 
s@0;
i WITH s @0;

!Provide starting values (otherwise had trouble with some model parameters)
[s*0];
[i*1];

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT (but not available for type = complex)
TECH1 TECH4 TECH11 TECH12;

SAVEDATA:
FILE IS "[[classes]]-class LCGA By Year [[Measure#outcome]] Output.csv";
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

#Specify measure (ODD, MOOD, CD, ANX, SDQTS)
Measure <- "MOOD"

if(Measure == "MOOD") {
  VarLab <- "Mood" 
} else if(Measure == "ANX") {
  VarLab <- "Anxiety"  
} else if(Measure == "SDQTS") {
    VarLab <- "SDQ Impact \nClinical Score"
} else if(Measure == "ODD" | Measure == "CD" ) {
  VarLab <- Measure }


#Get fit indices for all models related to a specific variable
#####################################################################################################

#Build a dataframe with all the summary/fit indice information for each model using the current measure 
SummariesAll <- NULL
for (i in paste0("X",FirstClass:LastClass,".class.lcga.by.year.",tolower(Measure),".out")) {
  SummariesAll <- bind_rows(SummariesAll, MplusOutput[[i]]$summaries)
} 

#Get the create a number of classes variable from the Mplus output filename, 
#select relevant fit indices, melt into long format and set class variable as numeric (cannot get BLRT_PValue, Tech14 if type = cluster)
Summaries <- SummariesAll %>%
  tidyr::extract(Filename, into = c("Classes"), regex = "([0-9]+)") %>%
  select(Classes, AIC, BIC, aBIC, Entropy, VLMR_pVal = T11_VLMR_PValue) %>%
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
  ggtitle(paste0("LCGA ", VarLab))


#####################################################################################################

#Examine relevant Mplus output for the current model:
#####################################################################################################
MplusOutput[[paste0("X",ModelNumber,".class.lcga.by.year.",tolower(Measure),".out")]]$covariance_coverage
MplusOutput[[paste0("X",ModelNumber,".class.lcga.by.year.",tolower(Measure),".out")]]$errors
MplusOutput[[paste0("X",ModelNumber,".class.lcga.by.year.",tolower(Measure),".out")]]$warnings
MplusOutput[[paste0("X",ModelNumber,".class.lcga.by.year.",tolower(Measure),".out")]]$summaries

#####################################################################################################

#Plot class output from Mplus for the current model
#####################################################################################################

#Get class output from Mplus file
MplusData <- MplusOutput[[paste0("X",ModelNumber,".class.lcga.by.year.",tolower(Measure),".out")]]$savedata

VarNum = length(YearRange)

# recode class labels after visual inspection for consistency across plots (format: "old" = new)
ifelse(Measure == "SDQTS" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 3), NA)
ifelse(Measure == "SDQTS" & ModelNumber == 4, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 2, "4" = 3, "2" = 4), NA)
ifelse(Measure == "MOOD" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 3), NA)
ifelse(Measure == "MOOD" & ModelNumber == 2, MplusData$C <- recode(MplusData$C, "2" = 1, "1" = 2), NA)
ifelse(Measure == "ANX" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 2, "2" = 3), NA)
ifelse(Measure == "ANX" & ModelNumber == 4, MplusData$C <- recode(MplusData$C, "1" = 2, "2" = 1), NA)
ifelse(Measure == "ODD" & ModelNumber == 2, MplusData$C <- recode(MplusData$C, "1" = 2, "2" = 1), NA)
ifelse(Measure == "CD" & ModelNumber == 2, MplusData$C <- recode(MplusData$C, "2" = 1, "1" = 2), NA)
ifelse(Measure == "CD" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 3), NA)
ifelse(Measure == "CD" & ModelNumber == 4, MplusData$C <- recode(MplusData$C, "2" = 1, "4" = 2, "1" = 4), NA)

ClassMeanData <- MplusData %>%
  select(1:VarNum,C)%>%
  melt(id.vars ="C") %>%
  dcast(C + variable ~ "value", fun.aggregate = mean, na.rm = TRUE) %>%
  tidyr::extract(variable, into = c("Variable", "YearSequence"), regex = "([A-Z]+)([0-9]+)") %>%
  mutate(YearSequence = as.numeric(YearSequence))

ClassProp = prop.table(table(MplusData$C))

ClassMeanPlot <- ggplot(ClassMeanData, aes(x = YearSequence, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_point(size = 2)+       
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Test Year")+
  ylab("Mean")+
  ggtitle(VarLab)+
  scale_x_continuous(breaks = c(YearRange))+
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)+
  coord_cartesian(ylim = c(-0.25,2.25))+
  scale_color_manual(values = pal2, name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                             paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                             paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                             paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                             paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))


library(longCatEDA)
LongObj <- longCat(select(MplusData, 1:VarNum), Labels = c("Absent","Subthreshold","Present"), tLabels = YearRange)
LongObjSort <- sorter(LongObj, group = MplusData$C, groupLabels = c("Low","Mod.", "High \nDec.", "Mod. \nInc."))
par(mai=c(1,0.5,0.50,0.05))
longCatPlot(LongObjSort, xlab = "Test Year", main = VarLab, legendBuffer = .1)


#Plot within-class proportions by gender, Y1 status, race, Y1 treatment 
#Get frequency of each category by class
PropData<- left_join(select(MplusData, MERGEID, C), 
          select(DataByAge, MERGEID, SEX, COMBINEDRACE, Y1_ADHD_STATUS, Y1_TREAT),
          by = "MERGEID")%>%
  melt(id.var = c("MERGEID", "C"))%>%
  dcast(C + variable + value ~ "count", fun.aggregate = length)

#Preallocate plot list and provide list of labels
plot_list <- list()
CatVarList <- c("SEX", "COMBINEDRACE", "Y1_ADHD_STATUS", "Y1_TREAT")
LabList <- c("Gender", "Race", "Y1 ADHD Status", "Y1 Treatment")
CatList <- list(c("Male", "Female"), c("Non-White","White"), c("Control", "Subthresh", "ADHD", "NC Control"), c("None", "Med", "Psych", "Med+Psych"))

#Loop through variables above to create proportion bar plots by class and variable
for (v in CatVarList) {
p <- ggplot(filter(PropData, variable == v), aes(x = as.factor(C), y = count, fill = as.factor(value))) +
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  theme_bw()+
  theme(panel.grid = element_blank())+
  xlab("Class")+
  ylab("Percent")+
  scale_fill_manual(name = LabList[[match(v, CatVarList)]], labels = CatList[[match(v, CatVarList)]], values = pal, na.value="grey23" )+
  scale_y_continuous(labels = scales::percent_format())
plot_list[[match(v, CatVarList)]] <- p
}

#Plot all together
library(cowplot)
plot_grid(plotlist = plot_list, align = "v", axis = "lr", ncol = 2)

#Plot within-class means for ADHD rating scale, IQ, and parent income category (technically categorical)
MeanData<- left_join(select(MplusData, MERGEID, C), 
                     select(DataByAge, MERGEID, Y1_P_ADHDRS_TOT_TS, WISCWIAT_FSIQ, PRIME_INCOME),
                     by = "MERGEID")%>%
  melt(id.var = c("MERGEID", "C"))%>%
  dcast(C + variable ~ "mean", fun.aggregate = mean, na.rm = TRUE)

plot_list2 <- list()
ContVarList <- c("Y1_P_ADHDRS_TOT_TS", "WISCWIAT_FSIQ", "PRIME_INCOME")
YAxisList <- c("ADHD Rating Scale \nt Score", "WISC-WIAT IQ", "Family Income Category \n(from 1: <25K to 8: >150K")
YAxisScale <- list(c(40,90), c(80, 120), c(0,8))

#Loop through variables above to create mean bar plots by class and variable
for (v in ContVarList) {
  p <- ggplot(filter(MeanData, variable == v), aes(x = as.factor(C), y = mean)) +
    geom_bar(position = "dodge", stat = "identity", fill = pal[3]) +
    theme_bw()+
    theme(panel.grid = element_blank())+
    xlab("Class")+
    ylab(YAxisList[[match(v, ContVarList)]])+
    coord_cartesian(ylim = YAxisScale[[match(v, ContVarList)]])
  plot_list2[[match(v, ContVarList)]] <- p
}

plot_grid(plotlist = plot_list2, align = "v", axis = "lr", ncol = 1)

#####################################################################################################
#Compare model assigment similarity across models with the same number of classes
#####################################################################################################

#Get all the class asignments for single class-number in one dataframe
ClassAssignments <- full_join(
  select(MplusOutput[[paste0("X",ModelNumber,".class.lcga.anx.out")]]$savedata, MERGEID, AnxClass = C),
  select(MplusOutput[[paste0("X",ModelNumber,".class.lcga.cd.out")]]$savedata, MERGEID, CDClass = C), by = "MERGEID") %>%
  full_join(.,select(MplusOutput[[paste0("X",ModelNumber,".class.lcga.mood.out")]]$savedata, MERGEID, MoodClass = C), by = "MERGEID") %>%
  full_join(.,select(MplusOutput[[paste0("X",ModelNumber,".class.lcga.odd.out")]]$savedata, MERGEID, ODDClass = C), by = "MERGEID")%>%
  full_join(.,select(MplusOutput[[paste0("X",ModelNumber,".class.lcga.sdqts.out")]]$savedata, MERGEID, SDQClass = C), by = "MERGEID")

# recode class labels after visual inspection for consistency across plots (format: "old" = new) COPIED FROM ABOVE, SHOULD MATCH
ClassAssignments$CDClass <- recode(ClassAssignments$CDClass, "2" = 1, "1" = 2)

#Recode class labels to pattern for clarity (this was determined after visual inspection of the original plots)
ClassAssignments[ClassAssignments == 1] <- "L"
ClassAssignments[ClassAssignments == 2] <- "H"

#Get the contingency table, convert to factors, rename new frequency variable
FreqData <- dcast(ClassAssignments, AnxClass + ODDClass + MoodClass + CDClass + SDQClass ~ "Frequency", fun.aggregate = length) %>%
  mutate(MoodClass = as.factor(MoodClass), AnxClass = as.factor(AnxClass), CDClass = as.factor(CDClass), ODDClass = as.factor(ODDClass), SDQClass = as.factor(SDQClass))

#Plot as alluvial
library(ggalluvial)
Alluv <- ggplot(data = FreqData,
                aes(axis1 = CDClass, axis2 = MoodClass, axis3 = ODDClass, axis4 = AnxClass, axis5 = SDQClass,
                    y = Frequency)) +
  scale_x_discrete(limits = c( "CD","Mood","ODD","Anxiety", "SDQ"), expand = c(.1, .05)) +
  xlab("Measure") +
  ylab("Frequency")+
  geom_alluvium(aes(fill = SDQClass)) +
  geom_stratum(width = .2) + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("LCGA Class Assignment by Measure")+
  scale_fill_manual(name = "SDQ Class", values = c("#db3939","#0dbf96"), labels = c("High", "Low"))

#Save plot (this figure type seems to require higher resolution)
ggsave(paste(MplusLocation,"alluvial.jpeg",sep = "/"), dpi = 600, width = 7, height = 7)

#Get class asignments for the top class-number within each measure in one dataframe
ClassAssignmentsBest <- full_join(
  select(MplusOutput[[paste0("X2.class.lcga.anx.out")]]$savedata, MERGEID, AnxClass = C),
  select(MplusOutput[[paste0("X2.class.lcga.cd.out")]]$savedata, MERGEID, CDClass = C), by = "MERGEID") %>%
  full_join(.,select(MplusOutput[[paste0("X3.class.lcga.mood.out")]]$savedata, MERGEID, MoodClass = C), by = "MERGEID") %>%
  full_join(.,select(MplusOutput[[paste0("X2.class.lcga.odd.out")]]$savedata, MERGEID, ODDClass = C), by = "MERGEID")%>%
  full_join(.,select(MplusOutput[[paste0("X2.class.lcga.sdqts.out")]]$savedata, MERGEID, SDQClass = C), by = "MERGEID")

# recode class labels after visual inspection for consistency across plots (format: "old" = new) COPIED FROM ABOVE, SHOULD MATCH
ClassAssignmentsBest$CDClass <- recode(ClassAssignmentsBest$CDClass, "2" = 1, "1" = 2)
ClassAssignmentsBest$MoodClass <- recode(ClassAssignmentsBest$MoodClass, "2" = 3, "3" = 2)

ContTable <- table(ClassAssignmentsBest$CDClass, ClassAssignmentsBest$ODDClass)

#Copy table to clipboard
clipr::write_clip(ContTable)

#For each variable pair, create a frequency table and stacked bar graph to show contingency
FreqData <- dcast(ClassAssignmentsBest, ODDClass + CDClass ~ "Frequency", fun.aggregate = length) %>%
  mutate(ODDClass = as.factor(ODDClass), CDClass = as.factor(CDClass))

ggplot(FreqData, aes(x = ODDClass, y = Frequency, fill = CDClass)) +
  geom_bar(position = "stack", stat = "identity", width = 0.8, alpha = 0.5) +
  scale_fill_manual(name = "CD Class", values = c("#0dbf96","#db3939"), labels = c("Low", "Moderate"))+
  scale_x_discrete(labels = c("Low", "Moderate"))+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank())+
  xlab("ODD Class")
