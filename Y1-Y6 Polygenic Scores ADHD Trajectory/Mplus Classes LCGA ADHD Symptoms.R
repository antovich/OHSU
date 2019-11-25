#Script to take age-restructured data and run LGCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportion

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)

#Where data file (created below) will go
MplusLocation <- 'H:/Projects/Y1-Y6 Polygenic Scores ADHD Trajectory for Joel/Mplus Scripts/LCGA Symptom'
MplusDataFile <- 'MplusDataByAge.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Number of classes to run:
FirstClass = 1
LastClass = 4

#Select the year range for the analysis
YearRange = 1:6

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

OutcomeData <- read.csv('H:/Projects/Y1-Y6 Polygenic Scores ADHD Trajectory for Joel/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c(NA, -999, -888, ""), fileEncoding = "UTF-8-BOM")
GeneticData <- read.csv('H:/Projects/Y1-Y6 Polygenic Scores ADHD Trajectory for Joel/Data/ADHD_Polygenic_Score_EUR_and_Top4_PCs_List1_Unrelated_WhitesOnly_4-Aug-2017.csv', na.strings = c(NA, -999, -888, ""), fileEncoding = "UTF-8-BOM") %>%
  rename("MERGEID" = "IID")

#Take age restructured data created in the "Restructure Data by Age.r" script and modify it for Mplus
#Select rename all relevant variables then select a subset. Note, the order in select() must be maintained to match the Mplus headers (below)
MplusData <- OutcomeData %>%
  rename(ADHDRS1 = Y1_P_ADHDRS_TOT_TS, KSADHYP1 = Y1_KSAD_HYPSX, KSADINT1 = Y1_KSAD_INTSX, SWAN1 = Y1_SWAN_TOT, CONHYP1 = Y1_P_CON3_HYP_TS, CONINT1 = Y1_P_CON3_INT_TS,
         ADHDRS2 = Y2_P_ADHDRS_TOT_TS, KSADHYP2 = Y2_KSAD_HYPSX, KSADINT2 = Y2_KSAD_INTSX, SWAN2 = Y2_SWAN_TOT, CONHYP2 = Y2_P_CON3_HYP_TS, CONINT2 = Y2_P_CON3_INT_TS,
         ADHDRS3 = Y3_P_ADHDRS_TOT_TS, KSADHYP3 = Y3_KSAD_HYPSX, KSADINT3 = Y3_KSAD_INTSX, SWAN3 = Y3_SWAN_TOT, CONHYP3 = Y3_P_CON3_HYP_TS, CONINT3 = Y3_P_CON3_INT_TS,
         ADHDRS4 = Y4_P_ADHDRS_TOT_TS, KSADHYP4 = Y4_KSAD_HYPSX, KSADINT4 = Y4_KSAD_INTSX, SWAN4 = Y4_SWAN_TOT, CONHYP4 = Y4_P_CON3_HYP_TS, CONINT4 = Y4_P_CON3_INT_TS,
         ADHDRS5 = Y5_P_ADHDRS_TOT_TS, KSADHYP5 = Y5_KSAD_HYPSX, KSADINT5 = Y5_KSAD_INTSX, SWAN5 = Y5_SWAN_TOT, CONHYP5 = Y5_P_CON3_HYP_TS, CONINT5 = Y5_P_CON3_INT_TS,
         ADHDRS6 = Y6_P_ADHDRS_TOT_TS, KSADHYP6 = Y6_KSAD_HYPSX, KSADINT6 = Y6_KSAD_INTSX, SWAN6 = Y6_SWAN_TOT, CONHYP6 = Y6_P_CON3_HYP_TS, CONINT6 = Y6_P_CON3_INT_TS) %>%
  right_join(GeneticData, by = "MERGEID") %>%
  select(MERGEID, paste0("ADHDRS",YearRange),paste0("KSADHYP",YearRange),paste0("KSADINT",YearRange),paste0("SWAN",YearRange),paste0("CONHYP",YearRange),paste0("CONINT",YearRange))


#Creates FAMID variable for clustering in Mplus
MplusData$FAMID <- as.numeric(substr(MplusData$MERGEID, start = 1, stop =5 ))

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(MplusData, paste(MplusLocation, MplusDataFile, sep ="/"))

#Create txt file designating iterative Mplus models
writeLines(paste(
  '[[init]]
iterators = classes outcome;
classes = ',FirstClass,':',LastClass,';
outcome = 1:6;
Measure#outcome = ADHDRS KSADHYP KSADINT SWAN CONHYP CONINT;
filename = "[[classes]]-class LCGA [[Measure#outcome]].inp";
outputDirectory = "', MplusLocation, '";
[[/init]]

TITLE: LCGA ADHD Symptom Comparison
DATA: FILE = "', paste(MplusLocation, MplusDataFile,sep = "/"), '";

VARIABLE: 
NAMES ARE
MERGEID ',
paste0('ADHDRS', YearRange[1],'-ADHDRS', last(YearRange)),' ',
paste0('KSADHYP', YearRange[1],'-KSADHYP', last(YearRange)),' ', 
paste0("KSADINT", YearRange[1],"-KSADINT", last(YearRange)),' ',
paste0("SWAN", YearRange[1],"-SWAN", last(YearRange)),' ',
paste0("CONHYP", YearRange[1],"-CONHYP", last(YearRange)),' ',
paste0("CONINT", YearRange[1],"-CONINT", last(YearRange)),'
FAMID;

IDVARIABLE IS MERGEID;

CLUSTER IS FAMID;

USEVARIABLES ARE 
[[Measure#outcome]]', YearRange[1],'-[[Measure#outcome]]', last(YearRange),';

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

i s ',ifelse(Quad, 'q', ''),' | ', paste0('[[Measure#outcome]]',YearRange,'@', (YearRange-1), collapse = " "), ';

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT (but not available for type = complex)
TECH1 TECH4 TECH11 TECH12 TECH14;

SAVEDATA:
FILE IS "[[classes]]-class Symptom LCGA [[Measure#outcome]] Output.csv";
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

#Specify measure (ADHDRS, KSADHYP, KSADINT, SWAN, CONHYP, CONINT)
Measure <- "KSADINT"

if(Measure == "ADHDRS") {
  VarLab <- "ADHD Rating Scale \n Total t-Scores" 
} else if(Measure == "KSADHYP") {
  VarLab <- "KSAD Hyperactive \nSymptom Count" 
} else if(Measure == "KSADINT") {
  VarLab <- "KSAD Inattentive \nSymptom Count" 
} else if(Measure == "SWAN") {
    VarLab <- "SWAN Overall Score"
} else if(Measure == "CONHYP") {
  VarLab <- "Conners 3 \nHyperactive Scale t-Scores"
} else if(Measure == "CONINT") {
  VarLab <- "Conners 3 \nInattentive Scale t-Scores" }

#Number of classes
ModelNumber <- 3

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
  ggtitle(paste0("Fit Indices: LCGA ", VarLab))


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
MplusDataOut <- MplusOutput[[paste0("X",ModelNumber,".class.lcga.",tolower(Measure),".out")]]$savedata

# recode class labels after visual inspection for consistency across plots (format: "old" = new)
ifelse(Measure == "ADHDRS" & ModelNumber == 3, MplusDataOut$C <- recode(MplusDataOut$C, "1" = 3, "2" = 1, "3" = 2), NA)
ifelse(Measure == "KSADHYP" & ModelNumber == 3, MplusDataOut$C <- recode(MplusDataOut$C, "1" = 2, "2" = 1), NA)
ifelse(Measure == "SWAN" & ModelNumber == 3, MplusDataOut$C <- recode(MplusDataOut$C, "1" = 3, "3" = 1), NA)
ifelse(Measure == "SWAN" & ModelNumber == 2, MplusDataOut$C <- recode(MplusDataOut$C, "1" = 2, "2" = 1), NA)
ifelse(Measure == "CONHYP" & ModelNumber == 2, MplusDataOut$C <- recode(MplusDataOut$C, "1" = 2, "2" = 1), NA)
ifelse(Measure == "CONHYP" & ModelNumber == 4, MplusDataOut$C <- recode(MplusDataOut$C, "1" = 4, "4" = 1), NA)
ifelse(Measure == "CONINT" & ModelNumber == 4, MplusDataOut$C <- recode(MplusDataOut$C, "1" = 3, "3" = 1), NA)

VarNum = length(YearRange)

ClassMeanData <- MplusDataOut %>%
  select(1:VarNum,C)%>%
  melt(id.vars ="C") %>%
  dcast(C + variable ~ "value", fun.aggregate = mean, na.rm = TRUE) %>%
  tidyr::extract(variable, into = c("Variable", "YearSequence"), regex = "([A-Z]+)([0-9]+)") %>%
  mutate(YearSequence = as.numeric(YearSequence))

ClassProp = prop.table(table(MplusDataOut$C))

ClassMeanPlot <- ggplot(ClassMeanData, aes(x = YearSequence, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_point(size = 2)+       
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("Mean")+
  scale_x_continuous(breaks = YearRange)+
  coord_cartesian(ylim = c(0,8))+
  ggtitle(VarLab)+
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)+
  scale_color_manual(values = pal2, name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                             paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                             paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                             paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                             paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))

#Plot within-class proportions by gender, Y1 status, race, Y1 treatment 
#Get frequency of each category by class
PropData<- left_join(select(MplusDataOut, MERGEID, C), 
          select(OutcomeData, MERGEID, SEX, COMBINEDRACE, Y1_ADHD_STATUS, Y1_TREAT),
          by = "MERGEID")%>%
  melt(id.var = c("MERGEID", "C"))%>%
  dcast(C + variable + value ~ "count", fun.aggregate = length)

#Preallocate plot list and provide list of labels
plot_list <- list()
CatVarList <- c("SEX", "Y1_ADHD_STATUS", "Y1_TREAT")
LabList <- c("Gender", "Y1 ADHD Status", "Y1 Treatment")
CatList <- list(c("Male", "Female"), c("Control", "ADHD"), c("None", "Med", "Psych", "Med+Psych"))

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
plot_grid(plotlist = plot_list, align = "v", axis = "lr", ncol = 3)

#Plot within-class means for ADHD rating scale, IQ, and parent income category (technically categorical)
MeanData<- left_join(select(MplusDataOut, MERGEID, C), 
                     select(OutcomeData, MERGEID, Y1_P_ADHDRS_TOT_TS, WISCWIAT_FSIQ, PRIME_INCOME),
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

plot_grid(plotlist = plot_list2, align = "v", axis = "lr", ncol = 3)

#####################################################################################################
# ANOVA comparing polygenic risk across LGCA trajectories
#####################################################################################################

mod1 <- aov(SCALED_SCORE ~ C2, data = RegressData)

ANOVA <- summary.aov(mod1)

PGSMeans <- RegressData %>%
  group_by(C2)%>%
  summarize(Mean = mean(SCALED_SCORE),SE = plotrix::std.error(SCALED_SCORE))

ggplot(data = RegressData)+
  geom_violin(mapping = aes(x = C2, y = SCALED_SCORE, fill = C2), alpha = .5)+
  geom_jitter(mapping = aes(x = C2, y = SCALED_SCORE), width = 0.1, col = 'grey23') +
  geom_errorbar(data = PGSMeans, aes(x = C2, ymin=Mean-SE, ymax=Mean+SE), colour="red", width=.1) +
  geom_point(data = PGSMeans, aes(x = C2, y = Mean), col = 'red', size = 3)+
  scale_fill_manual(name = "Latent Class", limits = c("Low", "Moderate Decrease", "High Decrease"), values = pal2)+
  scale_x_discrete(limits = c("Low", "Moderate Decrease", "High Decrease"))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ylab("Polygenic Risk Score (Scaled)")+
  xlab("Latent Class")

#####################################################################################################
# Multinomial logistic model regressing LCGA trajectories on polygenic risk score
#####################################################################################################

library(nnet)

RegressData <-  left_join(MplusDataOut, GeneticData, by = "MERGEID") %>%
  mutate(C = as.factor(recode(C, "1" = "Low", "2" = "Moderate Decrease", "3" = "High Decrease")))

RegressData$C2 <- relevel(RegressData$C, ref = "Low")
mod1 <- multinom(C2 ~ SCORE, data = RegressData)

summary(mod2)

exp(coef(mod2))

pnorm(abs(summary(mod2)$coefficients / summary(mod2)$standard.errors))

# FOR KSAD AT LEAST IT APPEARS THAT PGS DOES NOT PREDICT CLASS MEMBERSHIP USING LOGISTIC REGRESSION; BUT THESE ARE HARD TO INTERPRET