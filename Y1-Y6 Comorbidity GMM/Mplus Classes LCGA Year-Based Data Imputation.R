#Script to impute missing data and run LGCA models in Mplus using the R/Mplus interface via the package MplusAutomation
#This also uses Mplus output to plots fit indices and class proportion
#In this instance missing data are handled using multiple imputation

library(tidyverse)
library(reshape2)
library(dplyr)
library(MplusAutomation)
library(cowplot)
library(mice)

#Number of imputation sets to create
NumImpute <- 10

#Number of classes to run (for single runs)
ClassNumber <- 2

#Dx variable to run or visualize
VarName <- "MOOD"

#Model Numbers for Fit Plots (for multiple runs):
FirstClass = 1
LastClass = 5

YearRange <- 1:6

#Model number to read
ModelNumber <- 1

#Where data file (created below) will go
MplusLocation <- 'H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Mplus Scripts/LCGA/Imputation/DataByYear/PMM'
MplusDataFile <- 'MplusData.dat'

#Name of file to create imputation sets
MplusInpFile <- 'ImputationScript.inp'

#Name of file to run Mplus LPA type = imputation
MplusMIFile <- paste0('missimp',ClassNumber,'.inp')

#Name of loop file for creating individual analysis files for all sets
MplusLoopFile <- 'MplusModelLoop.txt'

#Ouput file name format for extracting from Mplus file
ModelLabel <- c(".class.lcga.by.year.imputed.",".out")

#List of imputation/aux variables to include
UseList <- "AGE1 AGE2 AGE3 AGE4 AGE5 AGE6 MOOD1 MOOD2 MOOD3 MOOD4 MOOD5 MOOD6
ANX1 ANX2 ANX3 ANX4 ANX5 ANX6 ODD1 ODD2 ODD3 ODD4 ODD5 ODD6 CD1 CD2 CD3 CD4 CD5
CD6 ADHDSTAT SEX ARSHYP ARSINT KSADINT KSADHYP IQ CONEF CONHYP CONINT ACTCONT
ACTIVITY AFFIL ANGER FEAR HIP IMPULS INHIBIT SAD SHY SOOTHE ASSERT ATTFOCUS LIP
PERCEPT DISCOMF OPENNESS"

#List of variables to impute
ImpList <- "MOOD1 MOOD2 MOOD3 MOOD4 MOOD5 MOOD6 
ANX1 ANX2 ANX3 ANX4 ANX5 ANX6 
ODD1 ODD2 ODD3 ODD4 ODD5 ODD6 
CD1 CD2 CD3 CD4 CD5 CD6"

#Plot title
PlotTitle <- "Imputation"

#Plot x-axis tick labels
AxisOrd <- c("")
AxisLabels <- c("")

#Plot y-limits
Ylimits <- c(0,2)

#Add quadratic to the LCGA model?
Quad = FALSE

#Blue palette for categorical data
pal = c("#a8deff","#64bdf5","#2172ed","#0048b5")

#Sequential palette for means data
pal2 = c('#30B8DA','#FF6347','#2FCA72','#FFC34A')

#Read in data
####################################################################################################

MissDataset <- read.csv('H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c("",-999,-888, NA),fileEncoding="UTF-8-BOM")%>%
  select(MERGEID, AGE1 = V1AGEYEARS, AGE2 = Y2AGEYEARS, AGE3 = Y3AGEYEARS, AGE4 = Y4AGEYEARS, AGE5 = Y5AGEYEARS, AGE6 = Y6AGEYEARS,
         MOOD1 = Y1_DTEAM_MOOD, MOOD2 = Y2_DTEAM_MOOD, MOOD3 = Y3_DTEAM_MOOD, MOOD4 = Y4_DTEAM_MOOD, MOOD5 = Y5_DTEAM_MOOD, MOOD6 = Y6_DTEAM_MOOD,
         ANX1 = Y1_DTEAM_ANX, ANX2 = Y2_DTEAM_ANX, ANX3 = Y3_DTEAM_ANX, ANX4 = Y4_DTEAM_ANX, ANX5 = Y5_DTEAM_ANX, ANX6 = Y6_DTEAM_ANX,
         ODD1 = Y1_DTEAM_ODD, ODD2 = Y2_DTEAM_ODD, ODD3 = Y3_DTEAM_ODD, ODD4 = Y4_DTEAM_ODD,ODD5 = Y5_DTEAM_ODD, ODD6 = Y6_DTEAM_ODD, 
         CD1 = Y1_DTEAM_CD, CD2 = Y2_DTEAM_CD, CD3 = Y3_DTEAM_CD, CD4 = Y4_DTEAM_CD, CD5 = Y5_DTEAM_CD, CD6 = Y6_DTEAM_CD,
        ADHDSTAT = Y1_ADHD_STATUS, SEX, ARSHYP = Y1_P_ADHDRS_HYP_TS,	ARSINT = Y1_P_ADHDRS_INT_TS, KSADINT = Y1_KSAD_INTSX,	KSADHYP = Y1_KSAD_HYPSX, IQ = WISCWIAT_FSIQ,
         CONEF = Y1_P_CON3_EF_TS,	CONHYP = Y1_P_CON3_HYP_TS,	CONINT = Y1_P_CON3_INT_TS, ACTCONT = Y1_TMCQ_ACTIVCONT, ACTIVITY = Y1_TMCQ_ACTIVITY, AFFIL = Y1_TMCQ_AFFIL,
        ANGER = Y1_TMCQ_ANGER, FEAR = Y1_TMCQ_FEAR, HIP = Y1_TMCQ_HIP, IMPULS = Y1_TMCQ_IMPULS, INHIBIT = Y1_TMCQ_INHIBIT, SAD = Y1_TMCQ_SAD,             
       SHY = Y1_TMCQ_SHY, SOOTHE = Y1_TMCQ_SOOTHE, ASSERT = Y1_TMCQ_ASSERT, ATTFOCUS = Y1_TMCQ_ATTFOCUS, LIP = Y1_TMCQ_LIP,             
       PERCEPT = Y1_TMCQ_PERCEPT, DISCOMF = Y1_TMCQ_DISCOMF, OPENNESS = Y1_TMCQ_OPENNESS)

#Get column numbers for variables that need coding change
firstcol <- which(colnames(MissDataset) == "MOOD1")
lastcol <- which(colnames(MissDataset) == "CD6")

#Change coding so that "rule outs" (5) are now "subthreshold"
MissDataset[firstcol:lastcol][MissDataset[firstcol:lastcol] == 5] <- 2 

#Create FAMID variable from MERGEID as clustering var
MissDataset$FAMID <- as.numeric(substr(MissDataset$MERGEID, start = 1, stop =5 ))

#Get variable to indicate missingness for later comparison with imputed data
MissKey <- MissDataset %>%
  select(-MERGEID)%>%
  is.na()%>%
  cbind(MissDataset$MERGEID,.)

####################################################################################################

#Impute missing values in mice in R using PMM algorithm (otherwise use Mplus for data augmentation alogrithm below)
####################################################################################################

## Extract all variable names in dataset
allVars <- names(MissDataset)

## names of variables with missingness
missVars <- names(MissDataset)[colSums(is.na(MissDataset)) > 0]

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
imputerVars <- c("AGE1","MOOD1","MOOD2","MOOD3","MOOD4","MOOD5","MOOD6","ANX1","ANX2","ANX3","ANX4","ANX5","ANX6",
    "ODD1","ODD2","ODD3","ODD4","ODD5","ODD6","CD1","CD2","CD3","CD4","CD5","CD6","ADHDSTAT","SEX","ARSHYP","ARSINT",
    "KSADINT","KSADHYP","IQ","CONEF","CONHYP","CONINT","ACTCONT","ACTIVITY","AFFIL","ANGER","FEAR","HIP","IMPULS","INHIBIT",
    "SAD","SHY","SOOTHE","ASSERT","ATTFOCUS","LIP","PERCEPT","DISCOMF","OPENNESS")
imputerMatrix <- predictorMatrix
imputerMatrix[,imputerVars] <- 1

#Variables to be imputed (must include all missing variables that will be used in imputation as predictors)
imputedVars <- intersect(unique(c(imputerVars)), missVars)
imputedMatrix <- predictorMatrix
imputedMatrix[imputedVars,] <- 1

predictorMatrix <- imputerMatrix * imputedMatrix

#Set diag of matrix to zero
diag(predictorMatrix) <- 0

#10 imputation sets
imp <- mice(MissDataset, m = NumImpute, method = "pmm", predictorMatrix = predictorMatrix,  remove_collinear = FALSE)

MiceVarNames <- colnames(complete(imp, 1))

####################################################################################################

#Prepare data and input files for Mplus
####################################################################################################

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)

#Prepare mice-based imputation sets for Mplus
for(impnum in 1:NumImpute) {
  
  prepareMplusData(complete(imp, impnum), paste0(MplusLocation, "/missimp", impnum,".dat"))
  
}

writeLines(paste0("missimp", 1:NumImpute,".dat"), paste0(MplusLocation, "/missimplist.dat"))


#Prepare data for Mplus-based imputation
prepareMplusData(MissDataset, paste(MplusLocation, MplusDataFile, sep ="/"))

####################################################################################################

#Create script to IMPUTE DX values in Mplus using aux variables
####################################################################################################

#Create txt file designating Mplus models
writeLines(paste(
'TITLE: Imputation for Dx Trajectory LCGA Comparison MI
DATA: FILE = 
"', paste0(MplusLocation,"/"), '
',MplusDataFile,'";

VARIABLE: 
NAMES ARE
',paste(colnames(MissDataset), collapse = " \n"),';

!Variables included in the imputation model
USEVARIABLES ARE
'
,UseList,
';

!Variables not included in the imputation model
AUXILIARY ARE 
MERGEID;

!DEFINE MISSING DATA CODE
MISSING = .;

!Variables to be imputed
DATA IMPUTATION:

!Change imputation method (currently COVARIANCE, REGRESSION, SEQUENTIAL)
!This is most relevant if the covariance (the default) does not converge
MODEL = SEQUENTIAL;

!Variables to impute
IMPUTE = 
',ImpList,' ;

!Can specify range for categorical/ordinal imputated values
 VALUES = ',ImpList,'([1-3]);

! Similarly, adjust number of decimals for imputed values 
! with ROUNDING = VAR(3)

NDATASETS = ',NumImpute,' ;

SAVE = missimp*.dat;

ANALYSIS:
TYPE = BASIC;

OUTPUT: 
TECH8'), paste(MplusLocation, MplusInpFile, sep ="/"))


#Uses the script above to run imputation in Mplus
runModels(paste(MplusLocation, MplusInpFile, sep ="/"), replaceOutfile = "modifiedDate")


####################################################################################################

#Create script to RUN LCGA over imputed datasets individually
#Per Mplus to help prevent switching, completes one run to get good start values
#Must run first model, then manually get start values
####################################################################################################

#Creates script to perform LCGA iterating through class numbers and variables for the first imputation set 
#(change NAMES ARE values to ',paste(MiceVarNames, collapse = " "),' ; for MICE imputation)
####################################################################################################

#Create txt file designating iterative Mplus models
writeLines(paste(
  '[[init]]
iterators = classes outcome;
classes = ',FirstClass,':',LastClass,';
outcome = 1:4;
Measure#outcome = MOOD ANX ODD CD;
filename = "[[classes]]-class LCA Imputation Set 1 [[Measure#outcome]].inp";
outputDirectory = "', MplusLocation, '";
[[/init]]

TITLE: Dx LCGA Imputation with Year Data
DATA: FILE = "', paste(MplusLocation, "missimp1.dat",sep = "/"), '";

VARIABLE: 
NAMES ARE
',paste(colnames(MissDataset), collapse = " \n"),' ;

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
TECH1 TECH4 TECH11 TECH12 SVALUES;

SAVEDATA:
FILE IS "[[classes]]-class LCGA By Year [[Measure#outcome]] Output.csv";
SAVE = CPROB;'
  , sep = ""), paste(MplusLocation, MplusLoopFile, sep ="/"))


createModels(paste(MplusLocation, MplusLoopFile, sep ="/"))

runModels(MplusLocation, replaceOutfile = "modifiedDate")

MplusOutput <- readModels(MplusLocation)

#Gets model syntax using final starting values from Mplus run (OUTPUT: SVALUES)
SValues <- str_match(read_file(paste(MplusLocation, '/',ClassNumber,'-class lcga by year imputed ',VarName,'.out', sep ="")), 
          "(?s)MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES(.*)TECHNICAL 1 OUTPUT")[,2]


####################################################################################################

#Script for running the remaining imputation sets (must supply save values from prior run above)
####################################################################################################
#Create txt file designating iterative Mplus models
writeLines(paste(
  '[[init]]
iterators = ImpSets;
ImpSets = ',2,':',NumImpute,';
filename = "',ClassNumber,'-class LCA Imputation Set [[ImpSets]] ',VarName,'.inp";
outputDirectory = "', MplusLocation,'";
[[/init]]

TITLE: Dx LCGA Imputation with Year Data
DATA: FILE = "', paste(MplusLocation, 'missimp[[ImpSets]].dat',sep = "/"), '";

VARIABLE: 
NAMES ARE
',paste(colnames(MissDataset), collapse = " \n"),' ;

IDVARIABLE IS MERGEID;

CLUSTER IS FAMID;

USEVARIABLES ARE 
',VarName, YearRange[1],'-',VarName, last(YearRange),paste0(" AGE", YearRange[1],"-AGE", last(YearRange)),';

TSCORES ARE ', paste0("AGE", YearRange[1],"-AGE", last(YearRange)),';

!DEFINE CATEGORICAL INDICATORS 
!(Must specify categories if not identical)

CATEGORICAL ARE 
',paste0(VarName,YearRange,'(*)', collapse = " "),';

!DEFINE # OF CLASSES
CLASSES are c(',ClassNumber,');

!DEFINE MISSING DATA CODE
MISSING = .;

ANALYSIS:
!ESTIMATE A MIXTURE MODEL
TYPE = RANDOM MIXTURE COMPLEX;

ALGORITHM = INTEGRATION;

!Set STARTS to zero to use starting values determined from the first run, per Mplus suggestion
STARTS = 0;

MODEL:

',SValues,'

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT (but not available for type = complex)
TECH1 TECH4 TECH11 TECH12 SVALUES;

SAVEDATA:
FILE IS "',ClassNumber,'-class LCGA By Year ',VarName,' Output.csv";
SAVE = CPROB;'
  , sep = ""), paste(MplusLocation, MplusLoopFile, sep ="/"))

#Uses loop file to create input files for individual LCA runs
createModels(paste(MplusLocation,  MplusLoopFile, sep ="/"))

#Loop to run individual LCA models
for(m in 2:NumImpute){
  runModels(paste(MplusLocation, '/',ClassNumber,'-Class LCA Imputation Set ', m, ' ',VarName,'.inp', sep =""), replaceOutfile = "modifiedDate")
}

MplusOutput <- readModels(MplusLocation)


####################################################################################################

#Create script to RUN LCGA over all imputed datasets - in Mplus: TYPE = IMPUTATION
####################################################################################################

writeLines(paste(
  'TITLE: Dx LCGA Imputation with Year Data
DATA: FILE = 
"', paste(MplusLocation, 'missimplist.dat', sep ="/"),'";
TYPE = IMPUTATION;

VARIABLE: 
NAMES ARE
',paste(colnames(MissDataset), collapse = " \n"),' ;

IDVARIABLE IS MERGEID;

CLUSTER IS FAMID;

USEVARIABLES ARE 
',VarName, YearRange[1],'-',VarName, last(YearRange),paste0(" AGE", YearRange[1],"-AGE", last(YearRange)),';

TSCORES ARE ', paste0("AGE", YearRange[1],"-AGE", last(YearRange)),';

!DEFINE CATEGORICAL INDICATORS 
!(Must specify categories if not identical)

CATEGORICAL ARE 
',paste0(VarName,YearRange,'(*)', collapse = " "),';

!DEFINE # OF CLASSES
CLASSES are c(',ClassNumber,');

!DEFINE MISSING DATA CODE
MISSING = .;

ANALYSIS:
!ESTIMATE A MIXTURE MODEL
TYPE = RANDOM MIXTURE COMPLEX;

ALGORITHM = INTEGRATION;

!Set STARTS to zero to use starting values determined from the first run, per Mplus suggestion
STARTS = 0;

MODEL:

',SValues,'

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT (but not available for type = complex)
TECH1 TECH4 TECH11 TECH12 SVALUES;

SAVEDATA:
FILE IS "',ClassNumber,'-class LCGA By Year ',VarName,' Output.csv";
SAVE = CPROB;'
  , sep = ""), paste(MplusLocation, MplusMIFile, sep ="/"))

runModels(paste(MplusLocation, MplusMIFile, sep ="/"), replaceOutfile = "modifiedDate")

MplusOutput <- readModels(paste0(MplusLocation, "/mplusmi2.out"))

####################################################################################################



####################################################################################################

# PLOTING AND SECONDARY ANALYSES
####################################################################################################


#Get fit indices for all models
####################################################################################################

#Build a dataframe with all the summary/fit indice information for each model using the current measure 
SummariesAll <- NULL
for (i in paste0("X",FirstClass:LastClass,ModelLabel[1],tolower(VarName),".out")) {
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
  ggtitle(paste("LCGA Mood"))


####################################################################################################

#Examine relevant Mplus output for the current model:
####################################################################################################
MplusOutput[[paste0(ModelLabel[1],ModelNumber,".out")]]$covariance_coverage
MplusOutput[[paste0(ModelLabel[1],ModelNumber,".out")]]$errors
MplusOutput[[paste0(ModelLabel[1],ModelNumber,".out")]]$warnings
MplusOutput[[paste0(ModelLabel[1],ModelNumber,".out")]]$summaries

####################################################################################################

#Plot class output from Mplus for the current model
####################################################################################################

#For plotting all imputation sets separately 
PlotList <- list()
for (ImpSet in 2:NumImpute) {
  MplusData <- MplusOutput[[paste0(ModelLabel[1],ImpSet,".out")]]$savedata
  
  ClassMeanData <- MplusData %>%
    select(-starts_with("CPROB"), -MERGEID)%>%
    melt(id.vars ="C") %>%
    dcast(C + variable ~ "value", fun.aggregate = mean, na.rm = TRUE)
  
  ClassProp = prop.table(table(MplusData$C))
  
  p <- ggplot(ClassMeanData, aes(x = variable, y = value, group = as.factor(C), color = as.factor(C))) +
    geom_point(size = 2)+   
    geom_line(size = 1)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))+
    xlab("")+
    ylab("Mean")+
    ggtitle(paste("Imputation Set", ImpSet))+
    scale_x_discrete(limits = AxisOrd ,labels = AxisLabels)+
    coord_cartesian(ylim = Ylimits)+
    scale_color_manual(values = pal2, name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                                                 paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                                                 paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                                                 paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                                                 paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))
  PlotList[[ImpSet-1]] <- p
}

#Plot all imputed dataset LPAs in a grid
plot_grid(plotlist = PlotList, ncol = 10)

#For any individual datasets
#Get class output from Mplus file
MplusData <- MplusOutput[[paste0("X",3,ModelLabel[1],tolower(VarName),".out")]]$savedata
#MplusData$C <- recode(MplusData$C, "2" = 1, "1" = 2)
VarNum = length(YearRange)

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
  ggtitle("Mood")+
  scale_x_continuous(breaks = c(YearRange))+
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)+
  coord_cartesian(ylim = c(-0.25,2.25))+
  scale_color_manual(values = pal2, name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                                               paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                                               paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                                               paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                                               paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))



ClassProp = prop.table(table(MplusData$C))

# #For type = imputation because there is no savedata element
# MplusData <- MplusOutput$parameters[["unstandardized"]]
# ClassProp <- MplusOutput$class_counts[["mostLikely"]][["proportion"]]
# ClassMeanData <- MplusData %>%
#   filter(paramHeader == "Means", param != "C#1") %>%
#   rename("variable" = param,"value" = est, "C" = LatentClass) %>%
#   mutate(C = as.factor(C), variable = as.factor(variable))

ClassMeanPlot1 <- ggplot(ClassMeanData, aes(x = variable, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_point(size = 2)+   
  geom_line(size = 1)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("")+
  ylab("Mean")+
  ggtitle("Mood")+
  scale_x_discrete(limits = AxisOrd ,labels = AxisLabels)+
  coord_cartesian(ylim = Ylimits)+
  scale_color_manual(values = pal2, name = "Class", labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                             paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                             paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                             paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                             paste("5 (", round(ClassProp[5]*100),"%)", sep = "")))


#Compare non-missing and imputed data for outcome variables

#Combine missing key with imputed dataset, melt into longform
MissingCompare <- full_join(MissKey, MplusData, by = "MERGEID") %>%
  select(-starts_with("CPROB"),-C) %>%
  melt(id.vars = c("MERGEID", "Missing"))

#Plot original and imputed values by scale
MissingCompPlot <- ggplot(MissingCompare, aes(x = variable, y = value, color = Missing)) +
  geom_jitter(aes(shape = Missing, color = Missing), size = 1)+
  scale_color_manual(name = "Missing", labels = c("Original", "Imputed"), values = pal2)+
  scale_shape_manual(name = "Missing", labels = c("Original", "Imputed"), values = c(16,16))+
  xlab("EATQ Scale")+
  ylab("Score")+
  scale_y_continuous(limits = c(-0.5, 6), breaks = c(0:6))+
  scale_x_discrete(limits = AxisOrd, labels = AxisLabels)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45), plot.title = element_text(hjust = 0.5))+
  ggtitle("Comparison of original and imputed data \nImputed dataset 1")

#Same as above for mice imputation
MiceMissing <- as.data.frame(MissKey) %>%
  select(starts_with("MOOD"), MERGEID) %>%
  melt(id.vars = "MERGEID")

MissingCompPlot <- ggplot(aes(x = variable, y = value, color = Missing)) +
  geom_jitter(aes(shape = Missing, color = Missing), size = 1)+
  scale_color_manual(name = "Missing", labels = c("Original", "Imputed"), values = pal2)+
  scale_shape_manual(name = "Missing", labels = c("Original", "Imputed"), values = c(16,16))+
  xlab("EATQ Scale")+
  ylab("Score")+
  scale_y_continuous(limits = c(-0.5, 6), breaks = c(0:6))+
  scale_x_discrete(limits = AxisOrd, labels = AxisLabels)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45), plot.title = element_text(hjust = 0.5))+
  ggtitle("Comparison of original and imputed data \nImputed dataset 1")

#Plot density of original and imputed values
#Initialize plot list
DensePlotList <- list()

#Loop across all imputed datasets (from ModelData defined in class imputation above) and plot density by EATQ variable and original vs. imputed
for(i in 1:NumImpute) {

  m <- full_join(ModelData[[1]], MissKey, by = "MERGEID")%>%
    select(ACTCONT:HIP, Missing, MERGEID) %>%
    melt(id.vars = c("MERGEID", "Missing"))
  
  m$variable <- factor(m$variable, levels = AxisOrd)
  
  p <-  ggplot(m, aes(x = value, fill = Missing)) +
    geom_density(alpha = .2)+
    theme_bw()+
    ylab("Density")+
    xlab("Score")+
    scale_x_continuous(breaks = c(0:5))+
    scale_fill_manual(values = pal2, labels = c('Original', 'Imputed'))+
    facet_grid(rows = vars(variable))+
    ggtitle(paste('Imputation Set',i))
  
  DensePlotList[[i]] <- p
}

#Plot each imputation set figure in a grid
plot_grid(plotlist = DensePlotList, ncol = 5)



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
