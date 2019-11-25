#Script to take age-restructured data and run RMLCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportions

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)

#Where data file (created below) will go, separate folder for each type of analysis must be created
MplusLocation <- 'H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Mplus Scripts/LTA'
MplusDataFile <- 'MplusDataByAge.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Range for number of classes to run:
FirstClass = 1
LastClass = 3

#Select the range of ages for the analysis
AgeRange = 7:14


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

#Dataset for examining rule outs
RuleOut <- select(MplusDataByAge, 2:ncol(MplusDataByAge))

# Designate 5 as missing or as subthreshold
MplusDataByAge[MplusDataByAge == 5] <- 2

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(MplusDataByAge, paste(MplusLocation, MplusDataFile, sep ="/"))

#Create txt file designating iterative Mplus models (see MplusAutomation vignette on createModels for more detail about format)
#THIS SYTAX IS WRONG, I NEED TO CHECK HOW TO SPECIFY THE INDICATORS FOR MULTIPLE CATEGORICAL LATENT VARIABLES (i.e., THE CLASS VARIABLE)
writeLines(paste(
'[[init]]
iterators = classes;
classes = ',FirstClass,':',LastClass,';

filename = "[[classes]]-class LTA.inp";
outputDirectory = "', MplusLocation, '";
[[/init]]

TITLE: LTA Comparison
DATA: FILE = "', paste(MplusLocation, MplusDataFile, sep="/"), '";

VARIABLE: 
NAMES ARE
MERGEID	',
paste0('ANX', AgeRange[1],'-ANX', last(AgeRange)),' ',
paste0('CD', AgeRange[1],'-CD', last(AgeRange)),' ', 
paste0("MOOD", AgeRange[1],"-MOOD", last(AgeRange)),' ',
paste0("ODD", AgeRange[1],"-ODD", last(AgeRange)),';

IDVARIABLE IS MERGEID;

USEVARIABLES ARE 
ANX8 ANX10 ANX12
CD8 CD10 CD12
MOOD8 MOOD10 MOOD12
ODD8 ODD10 ODD12;

!DEFINE CATEGORICAL INDICATORS
CATEGORICAL ARE 
ANX8 ANX10 ANX12
CD8 CD10 CD12
MOOD8 MOOD10 MOOD12
ODD8 ODD10 ODD12;

!DEFINE # OF CLASSES
CLASSES are c1([[classes]]) c2([[classes]]) c3([[classes]]);

!DEFINE MISSING DATA CODE
MISSING = .;

ANALYSIS:
!ESTIMATE A MIXTURE MODEL
TYPE = MIXTURE;

!FOR PROBABILY OUTPUT FOR TRANSITIONS
PARAMETERIZATION = PROBABILITY;

!DEFINE MULTIPLE RANDOM STARTS
STARTS = 500 100;

MODEL:
%OVERALL%


OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT
TECH1 TECH4 TECH8 TECH11 TECH14 TECH15;

SAVEDATA:
FILE IS "[[classes]]-class [[Measure#outcome]] RMLCA Ouput.csv";
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

#Get fit indices for all models related to a specific variable
#####################################################################################################

#Get correctly formatted label for plots
ifelse(Measure == "MOOD", VarLab <- "Mood", ifelse(Measure == "ANX", VarLab <- "Anxiety", VarLab <- Measure))

#Build a dataframe with all he summary/fit indice information for each model using the current measure 
SummariesAll <- NULL
for (i in paste0("X",FirstClass:LastClass,".class.",tolower(paste0(Measure, AgeRange[1],'.',Measure, last(AgeRange))),".rmcla.out")) {
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
  ggtitle(paste0("RMLCA ", VarLab))


#####################################################################################################

#Examine relevant Mplus output for specific class number model using the current variable:
#####################################################################################################

#Model number (i.e., number of classes) to assess (from Mplus output)
ModelNumber <- 2

#Print some information about the model to the screen from Mplus output (much more is available, see second level of MplusOutput list for other options)
MplusOutput[[c(paste0("X",ModelNumber,".class.", tolower(paste0(Measure, AgeRange[1],'.',Measure, last(AgeRange))),".rmcla.out"))]]$covariance_coverage
MplusOutput[[c(paste0("X",ModelNumber,".class.", tolower(paste0(Measure, AgeRange[1],'.',Measure, last(AgeRange))),".rmcla.out"))]]$errors
MplusOutput[[c(paste0("X",ModelNumber,".class.", tolower(paste0(Measure, AgeRange[1],'.',Measure, last(AgeRange))),".rmcla.out"))]]$warnings
MplusOutput[[c(paste0("X",ModelNumber,".class.", tolower(paste0(Measure, AgeRange[1],'.',Measure, last(AgeRange))),".rmcla.out"))]]$summaries

#####################################################################################################

#Compare model assigment similarity across models with the same number of classes
#####################################################################################################

#Get all the class asignments in one dataframe
ClassAssignments <- full_join(
  select(MplusOutput[[paste0("X",ModelNumber,".class.anx",AgeRange[1],".anx",last(AgeRange),".rmcla.out")]]$savedata, MERGEID, AnxClass = C),
  select(MplusOutput[[paste0("X",ModelNumber,".class.cd",AgeRange[1],".cd",last(AgeRange),".rmcla.out")]]$savedata, MERGEID, CDClass = C), by = "MERGEID") %>%
  full_join(.,select(MplusOutput[[paste0("X",ModelNumber,".class.mood",AgeRange[1],".mood",last(AgeRange),".rmcla.out")]]$savedata, MERGEID, MoodClass = C), by = "MERGEID") %>%
  full_join(.,select(MplusOutput[[paste0("X",ModelNumber,".class.odd",AgeRange[1],".odd",last(AgeRange),".rmcla.out")]]$savedata, MERGEID, ODDClass = C), by = "MERGEID")

#Recode class labels for consistency (this was determined after visual inspection of the original plots)
ClassAssignments$MoodClass <- recode(ClassAssignments$MoodClass, '1' = 2, '2' = 1)
ClassAssignments$CDClass <- recode(ClassAssignments$CDClass, '1' = 2, '2' = 1)

#Recode class labels to pattern for clarity (this was determined after visual inspection of the original plots)
ClassAssignments[ClassAssignments == 1] <- "Severe"
ClassAssignments[ClassAssignments == 2] <- "Mild"

#Get the contingency table, convert to factors, rename new frequency variable
FreqData <- dcast(ClassAssignments, AnxClass + ODDClass + MoodClass + CDClass ~ ., fun.aggregate = length) %>%
  mutate(MoodClass = as.factor(MoodClass), AnxClass = as.factor(AnxClass), CDClass = as.factor(CDClass), ODDClass = as.factor(ODDClass)) %>%
  rename("Frequency" = ".") 

#Plot as alluvial
library(ggalluvial)
Alluv <- ggplot(data = FreqData,
       aes(axis1 = CDClass, axis2 = MoodClass, axis3 = ODDClass, axis4 = AnxClass,
           y = Frequency)) +
  scale_x_discrete(limits = c( "CD","Mood","ODD","Anxiety"), expand = c(.1, .05)) +
  xlab("Measure") +
  ylab("Frequency")+
  geom_alluvium(aes(fill = AnxClass)) +
  geom_stratum(width = .2) + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("RMLCA Class Assignment by Measure")+
  scale_fill_manual(name = "Anxiety Class", values = c("#0dbf96","#db3939"))

#Save plot (this figure type seems to require higher resolution)
ggsave("alluvial.jpeg", dpi = 600, width = 7, height = 7)

# #Alternative way to get alluvial plot
# library(ggforce)
# FreqDate <- gather_set_data(FreqData, 1:4)
# ggplot(FreqData, aes(x, id = id, split = y, value = value)) +
#   geom_parallel_sets(aes(fill = AnxClass), alpha = 0.3)+
#   geom_parallel_sets_axes(axis.width = 0.2, fill = "white") +
#   geom_parallel_sets_labels(colour = 'black') +
#   theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5))+
#   scale_x_discrete(limits = c("AnxClass", "ODDClass", "MoodClass", "CDClass"), labels = c("Anxiety", "ODD", "Mood", "CD"))+
#   xlab("Measure")+
#   ylab("Count")+
#   ggtitle("RMLCA Class Assignment by Measure")+
#   scale_fill_manual(name = "Anxiety", values = c("#0dbf96","#db3939"))+
#   coord_flip()


#####################################################################################################

#Plot class output from Mplus for the current model
#####################################################################################################

#Get class output from Mplus file
MplusData <- MplusOutput[[c(paste0("X",ModelNumber,".class.", tolower(paste0(Measure, AgeRange[1],'.',Measure, last(AgeRange))),".rmcla.out"))]]$savedata

#Grab number of variables from data above
VarNum = length(AgeRange)

#Recode back to original category values
MplusData[1:VarNum][MplusData[1:VarNum] == 2] <-3
MplusData[1:VarNum][MplusData[1:VarNum] == 1] <-2
MplusData[1:VarNum][MplusData[1:VarNum] == 0] <-1

#Indicates whether class labels should be switched for consistency (this was determined after visual inspection of the original plots)
ifelse(Measure == "MOOD" | Measure == "CD", ReorderClasses <- TRUE, ReorderClasses <- FALSE)
ifelse(ReorderClasses == TRUE, MplusData$C <- recode(MplusData$C, '1' = 2, '2' = 1), MplusData$C <- MplusData$C)

# #TOO DIFFICULT TO SEE PATTERNS OVER TIME BECAUSE OF THE MISSING DATA; otherwise this would be alluvial plot looking at change over time
# Trends <- MplusData %>%
#   select(1:VarNum, C, MERGEID) %>%
#   dcast(ANX8 + ANX9 + ANX10 + ANX11 + ANX12 + ANX13 + ANX14 ~ C, fun.aggregate = length) %>%
#   rename("Class1" = "1", "Class2" = "2")
# 
# Trends$NACount <- apply(is.na(Trends[1:7]),1, sum)
# 
# AlluvMeasure <- ggplot(data = filter(Trends, NACount < 4),
#       aes(axis1 = as.factor(ANX8), axis2 = as.factor(ANX9), axis3 = as.factor(ANX10), axis4 = as.factor(ANX11),
#       axis5 = as.factor(ANX12), axis6 = as.factor(ANX13), axis7 = as.factor(ANX14), y = Class1)) +
#   scale_x_discrete(labels = c( "8","9","10","11","12","13","14"), expand = c(.1, .05)) +
#   xlab("Age") +
#   ylab("Frequency")+
#   geom_alluvium(aes(fill = as.factor(ANX8))) +
#   geom_stratum(width = .2) + geom_text(stat = "stratum", label.strata = TRUE) +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))+
#   ggtitle("RMLCA Class Assignment by Measure")+
#   scale_fill_manual(name = "Age 7 Anxiety", values = c("#0dbf96","#db3939", "red"))

#Plot each subject's categorical change over time, sorted by RMLCA class (see Tueller, Van  Dorn, & Bobashev, 2016)

library(longCatEDA)
LongObj <- longCat(select(MplusData, 1:VarNum), Labels = c("Absent","Subthresh","Present"), tLabels = AgeRange)
LongObjSort <- sorter(LongObj, group = MplusData$C, groupLabels = c("Severe", "Moderate"))
longCatPlot(LongObjSort, xlab = "Age")



#Get dataframe with just measure variables and class, melt into long format, cast to get the mean by class and measure
ClassMeanData <- MplusData %>%
  select(paste0(Measure,AgeRange), C)%>%
  melt(id.vars ="C") %>%
  dcast(C + variable ~ ., fun.aggregate = mean, na.rm = TRUE) %>%
  rename("value" = ".")

#Get proportion of members in each class
ClassProp = prop.table(table(MplusData$C))

#Plot means by age and class
ClassMeanPlot <- ggplot(ClassMeanData, aes(x = variable, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_point(size = 2)+       
  geom_line(size = 0.75)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Age")+
  ylab("Mean")+
  ggtitle(paste("RMLCA", VarLab))+
  scale_x_discrete(labels = c(AgeRange))+
  scale_y_continuous(limits = c(1,3))+
  scale_color_hue(name = "Class", labels = paste0(seq(FirstClass,LastClass)," (", round(ClassProp[seq(FirstClass,LastClass)]*100), "%)"))

#Get dataframe with all data in long format by class and age
ClassPropData <- MplusData %>%
  select(1:VarNum, C)%>%
  melt(id.vars ="C") %>%
  tidyr::extract(variable, into = c("Variable", "AgeSequence"), regex = "([A-Z]+)([0-9]+)")

#Loop over classes to get plots with proportion of eatch category (absent, subthreshold, present) by age for each class
plot_list <- list()
for(k in 1:length(unique(ClassPropData$C))){
  p <- ggplot(filter(ClassPropData, C == sort(unique(ClassPropData$C))[k], !is.na(value)), aes(x = AgeSequence, fill = as.factor(value))) +
          geom_bar(position = "fill", width = 0.8) +
          theme_bw()+
          theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))+
          ggtitle(paste(VarLab, ": Class ", k, " (", round(ClassProp[k]*100),"%)",  sep = ""))+
          xlab("Age")+
          ylab("Percent")+
          scale_fill_manual(name = "Category", labels = c("Absent", "Subthreshold", "Present"), values = c("#00AFBB", "#E7B800", "#FC4E07") )+
          scale_y_continuous(labels = scales::percent_format())+
          scale_x_discrete(labels = AgeRange)
  plot_list[[k]] = p
}

library(cowplot)

#Plot proportion figures for each class in a grid with each class representing a row
plot_grid(plotlist = plot_list, nrow = length(unique(ClassPropData$C)))

#####################################################################################################


#Examine consistentcy in rule outs across disorders
#####################################################################################################

RuleOut$AnxRuleOut <- apply(RuleOut[1:8] == 5, 1, sum, na.rm = TRUE)
RuleOut$CDRuleOut <- apply(RuleOut[9:16] == 5, 1, sum, na.rm = TRUE)
RuleOut$MoodRuleOut <- apply(RuleOut[17:24] == 5, 1, sum, na.rm = TRUE)
RuleOut$ODDRuleOut <- apply(RuleOut[25:32] == 5, 1, sum, na.rm = TRUE)

RuleOut$AnxRuleOutAny = NULL
RuleOut$CDRuleOutAny = NULL
RuleOut$MoodRuleOutAny = NULL
RuleOut$ODDRuleOutAny = NULL

RuleOut$AnxRuleOutAny[RuleOut$AnxRuleOut > 0] <- 1
RuleOut$CDRuleOutAny[RuleOut$CDRuleOut > 0] <- 1
RuleOut$MoodRuleOutAny[RuleOut$MoodRuleOut > 0] <- 1
RuleOut$ODDRuleOutAny[RuleOut$ODDRuleOut > 0] <- 1

RuleOutFreq <- RuleOut %>%
  dcast(AnxRuleOutAny + CDRuleOutAny + MoodRuleOutAny + ODDRuleOutAny ~ ., fun.aggregate = length) %>%
  rename("Frequency" = ".")

RuleOutFreq[RuleOutFreq == 0] = "None"
RuleOutFreq[RuleOutFreq == 1] = "Rule Out"

AlluvMeasure <- ggplot(data =RuleOutFreq,
                       aes(axis1 = as.factor(CDRuleOutAny), axis2 = as.factor(ODDRuleOutAny), 
                           axis3 = as.factor(MoodRuleOutAny), axis4 = as.factor(AnxRuleOutAny), y = Frequency)) +
  scale_x_discrete(limits = c("CD", "ODD", "Mood", "Anxiety"), expand = c(.1, .05)) +
  xlab("Measure") +
  ylab("Frequency")+
  geom_alluvium(aes(fill = as.factor(AnxRuleOutAny))) +
  geom_stratum(width = .2) + geom_text(stat = "stratum", label.strata = TRUE, size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Any Rule Out by Measure")+
  scale_fill_manual(name = "Age 7 Anxiety", values = c("#0dbf96","#db3939"))+
  coord_flip()
ggsave("H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/RuleOut.jpeg", dpi = 600, width = 7, height = 7)

RuleOutByAge <- select(RuleOut, 1:32)
RuleOutByAge[RuleOutByAge != 5] = NA
RuleOutByAge[RuleOutByAge == 5] = 1

RuleOutByAge$AnxTotal <- apply(select(RuleOutByAge, ANX7:ANX14), 1, sum, na.rm = TRUE)
RuleOutByAge$CDTotal <- apply(select(RuleOutByAge, CD7:CD14), 1, sum, na.rm = TRUE)
RuleOutByAge$MoodTotal <- apply(select(RuleOutByAge, MOOD7:MOOD14), 1, sum, na.rm = TRUE)
RuleOutByAge$ODDTotal <- apply(select(RuleOutByAge, ODD7:ODD14), 1, sum, na.rm = TRUE)

RuleOutByAgePlot <- select(RuleOutByAge, AnxTotal, CDTotal, MoodTotal, ODDTotal) %>%
  melt()%>%
  dcast(variable + value ~ ., fun.aggregate = length) %>%
  rename("freq" = ".")

ggplot(RuleOutByAgePlot, aes(x = variable, y = freq, fill = as.factor(value)))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Total Number \nof Rule Outs")+
  scale_x_discrete(labels = c("Anxiety", "CD", "Mood", "ODD")) +
  xlab("Measure")+
  ylab("Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Frequency of Multiple Rule Outs by Measure")+
  theme_minimal()

#####################################################################################################