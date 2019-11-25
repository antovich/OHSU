#Script to take age-restructured data and run LGCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportion

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)

#Where data file (created below) will go
MplusLocation <- 'H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Mplus Scripts/LCGA/Cluster'
MplusDataFile <- 'MplusDataByAge.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Number of classes to run:
FirstClass = 1
LastClass = 5

#Number of classes
ModelNumber <- 4

#Select the range of ages for the analysis
AgeRange = 7:14

#Add quadratic to the LCGA model?
Quad = FALSE

# # Orange palette
# pal = c("#ffc100","#ff9a00","#ff7400", "#ff4d00")

#Blue palette for categorical data
pal = c("#a8deff","#64bdf5","#2172ed","#0048b5")

#Sequential palette for means data
pal2 = c('#30B8DA','#FF6347','#2FCA72','#FFC34A')

#Prepare data and input files for Mplus
#####################################################################################################

#Take age restructured data created in the "Restructure Data by Age.r" script and modify it for Mplus
#Select rename all relevant variables then select a subset. Note, the order in select() must be maintained to match the Mplus headers (below)
MplusDataByAge <- DataByAge %>%
  rename(MOOD7 = Age7_DTEAM_MOOD, ANX7 = Age7_DTEAM_ANX, ODD7 = Age7_DTEAM_ODD, CD7 = Age7_DTEAM_CD, SDQTS7 = Age7_P_SDQ_IM_TS,
         MOOD8 = Age8_DTEAM_MOOD, ANX8 = Age8_DTEAM_ANX, ODD8 = Age8_DTEAM_ODD, CD8 = Age8_DTEAM_CD, SDQTS8 = Age8_P_SDQ_IM_TS,
         MOOD9 = Age9_DTEAM_MOOD, ANX9 = Age9_DTEAM_ANX, ODD9 = Age9_DTEAM_ODD, CD9 = Age9_DTEAM_CD, SDQTS9 = Age9_P_SDQ_IM_TS, 
         MOOD10 = Age10_DTEAM_MOOD, ANX10 = Age10_DTEAM_ANX, ODD10 = Age10_DTEAM_ODD, CD10 = Age10_DTEAM_CD, SDQTS10 = Age10_P_SDQ_IM_TS, 
         MOOD11 = Age11_DTEAM_MOOD, ANX11 = Age11_DTEAM_ANX, ODD11 = Age11_DTEAM_ODD, CD11 = Age11_DTEAM_CD, SDQTS11 = Age11_P_SDQ_IM_TS, 
         MOOD12 = Age12_DTEAM_MOOD, ANX12 = Age12_DTEAM_ANX, ODD12 = Age12_DTEAM_ODD, CD12 = Age12_DTEAM_CD, SDQTS12 = Age12_P_SDQ_IM_TS, 
         MOOD13 = Age13_DTEAM_MOOD, ANX13 = Age13_DTEAM_ANX, ODD13 = Age13_DTEAM_ODD, CD13 = Age13_DTEAM_CD, SDQTS13 = Age13_P_SDQ_IM_TS,
         MOOD14 = Age14_DTEAM_MOOD, ANX14 = Age14_DTEAM_ANX, ODD14 = Age14_DTEAM_ODD, CD14 = Age14_DTEAM_CD, SDQTS14 = Age14_P_SDQ_IM_TS,
         MOOD15 = Age15_DTEAM_MOOD, ANX15 = Age15_DTEAM_ANX, ODD15 = Age15_DTEAM_ODD, CD15 = Age15_DTEAM_CD, SDQTS15 = Age15_P_SDQ_IM_TS,
         MOOD16 = Age16_DTEAM_MOOD, ANX16 = Age16_DTEAM_ANX, ODD16 = Age16_DTEAM_ODD, CD16 = Age16_DTEAM_CD, SDQTS16 = Age16_P_SDQ_IM_TS,
         MOOD17 = Age17_DTEAM_MOOD, ANX17 = Age17_DTEAM_ANX, ODD17 = Age17_DTEAM_ODD, CD17 = Age17_DTEAM_CD, SDQTS17 = Age17_P_SDQ_IM_TS,
         MOOD18 = Age18_DTEAM_MOOD, ANX18 = Age18_DTEAM_ANX, ODD18 = Age18_DTEAM_ODD, CD18 = Age18_DTEAM_CD, SDQTS18 = Age18_P_SDQ_IM_TS,
         MOOD19 = Age19_DTEAM_MOOD, ANX19 = Age19_DTEAM_ANX, ODD19 = Age19_DTEAM_ODD, CD19 = Age19_DTEAM_CD, SDQTS19 = Age19_P_SDQ_IM_TS) %>%
  select(MERGEID, paste0("ANX",AgeRange),paste0("CD",AgeRange),paste0("MOOD",AgeRange),paste0("ODD",AgeRange),paste0("SDQTS",AgeRange))


# Designate 5 as missing or as subthreshold
MplusDataByAge[MplusDataByAge == 5] <- 2

#Creates FAMID variable for clustering in Mplus
MplusDataByAge$FAMID <- as.numeric(substr(MplusDataByAge$MERGEID, start = 1, stop =5 ))

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(MplusDataByAge, paste(MplusLocation, MplusDataFile, sep ="/"))

#Create txt file designating iterative Mplus models
writeLines(paste(
  '[[init]]
iterators = classes outcome;
classes = ',FirstClass,':',LastClass,';
outcome = 1:5;
Measure#outcome = ANX CD MOOD ODD SDQTS;
filename = "[[classes]]-class LCGA [[Measure#outcome]].inp";
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
paste0("ODD", AgeRange[1],"-ODD", last(AgeRange)),' ',
paste0("SDQTS", AgeRange[1],"-SDQTS", last(AgeRange)),'
FAMID;

IDVARIABLE IS MERGEID;

CLUSTER IS FAMID;

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
TYPE = MIXTURE COMPLEX;

!DEFINE MULTIPLE RANDOM STARTS
STARTS = 100 50;

MODEL:
%OVERALL%

i s ',ifelse(Quad, 'q', ''),' | ', paste0('[[Measure#outcome]]',AgeRange,'@', (0:length(AgeRange)), collapse = " "),';

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT (but not available for type = complex)
TECH1 TECH4 TECH11 TECH12;

SAVEDATA:
FILE IS "[[classes]]-class LCGA [[Measure#outcome]] Output.csv";
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
Measure <- "ANX"

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
for (i in paste0("X",FirstClass:LastClass,".class.lcga.",tolower(Measure),".out")) {
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
MplusOutput[[paste0("X",ModelNumber,".class.lcga.",tolower(Measure),".out")]]$covariance_coverage
MplusOutput[[paste0("X",ModelNumber,".class.lcga.",tolower(Measure),".out")]]$errors
MplusOutput[[paste0("X",ModelNumber,".class.lcga.",tolower(Measure),".out")]]$warnings
MplusOutput[[paste0("X",ModelNumber,".class.lcga.",tolower(Measure),".out")]]$summaries

#####################################################################################################

#Plot class output from Mplus for the current model
#####################################################################################################

#Get class output from Mplus file
MplusData <- MplusOutput[[paste0("X",ModelNumber,".class.lcga.",tolower(Measure),".out")]]$savedata

VarNum = length(AgeRange)

# recode class labels after visual inspection for consistency across plots (format: "old" = new)
ifelse(Measure == "SDQTS" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 3), NA)
ifelse(Measure == "SDQTS" & ModelNumber == 4, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 2, "4" = 3, "2" = 4), NA)
ifelse(Measure == "MOOD" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "2" = 3, "3" = 2), NA)
ifelse(Measure == "ANX" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "3" = 1, "1" = 2, "2" = 3), NA)
ifelse(Measure == "ANX" & ModelNumber == 4, MplusData$C <- recode(MplusData$C, "3" = 2, "2" = 3), NA)
ifelse(Measure == "ODD" & ModelNumber == 4, MplusData$C <- recode(MplusData$C, "4" = 2, "2" = 4), NA)
ifelse(Measure == "CD" & ModelNumber == 2, MplusData$C <- recode(MplusData$C, "2" = 1, "1" = 2), NA)
ifelse(Measure == "CD" & ModelNumber == 3, MplusData$C <- recode(MplusData$C, "2" = 1, "1" = 2), NA)

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
  coord_cartesian(ylim = c(-0.25,2.25))+
  scale_color_manual(values = pal2, name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                             paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                             paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                             paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                             paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))

library(longCatEDA)
LongObj <- longCat(select(MplusData, 1:VarNum), Labels = c("Absent","Subthreshold","Present"), tLabels = AgeRange)
LongObjSort <- sorter(LongObj, group = MplusData$C, groupLabels = c("Low \nIncreasing","Moderate", "Low", "Sharp \nDec."))
par(mai=c(1,0.5,0.50,0.05))
longCatPlot(LongObjSort, xlab = "Age", main = VarLab, legendBuffer = .1)
dev.off()

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
