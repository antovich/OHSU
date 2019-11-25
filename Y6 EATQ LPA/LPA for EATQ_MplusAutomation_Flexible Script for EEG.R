# Uses MPlusAutomation package to run LPA

#Clear variables? 'TRUE' or 'FALSE'
ClearWorkspace = TRUE
if(ClearWorkspace) {rm(list = ls())}

#Set seed to ensure reproducability
set.seed(123)

#Set working drive
setwd('H:/Projects/Y6 EATQ LPA for Sarah n400')

#Load data manipulation and LPA packages
library(tidyverse)
library(reshape2)
library(MplusAutomation)

###########################################################################################

#Options 
###########################################################################################

#Input data filename
Filename = 'EEG Dteam_EATQ Dataset 10-1-19 (not final).csv'
#Input data location
Location = 'H:/Projects/Y6 EATQ LPA for Sarah n400/Data/'

#Location to create and store Mplus input and output files
MplusLocation <- 'H:/Projects/Y6 EATQ LPA for Sarah n400/Mplus Scripts/LPA'
MplusDataFile <- 'MplusDataByAge.dat'
MplusLoopFile <- 'MplusModelLoop.txt'

#Number of classes
FirstClass = 1
LastClass = 10

#Which group for plotting? 'ADHD' or 'Control'
Group = "ADHD"

#Which year for ADHD status? 'Y1' or 'Y6'
Year = "Y1"

#Parent report or child report? 'Parent' OR 'Child' OR 'Parent & Child'
Reporter = "Parent & Child"

#For Parent & Child version, should the mean plots be aligned (e.g., PCPCPC) or separated (e.g., PPPCCC)?
Separate = FALSE

#Means plots y-axis limits:
yLimits = c(-5.5,2.5)

#setup palette for plots
pal = c('#30B8DA','#FF6347','#2FCA72','#FFC34A', '#8f34eb')

###########################################################################################

#Varible ordering
###########################################################################################

if(Reporter == "Child"){
  #Shortened varible names for MPLus, must be in order of file
  RenameVariables = c("MERGEID","ACTCONT", "AFFIL", "AGGRESS", "ATTFOC","DEPRESS", 
                      "FEAR",	"FRUST", "INHIB",	"LIP", "PERCEPT", "SHY", "HIP")
  
  #Order of variables for plotting
  PlotOrd = c("ATTFOC","INHIB","ACTCONT","PERCEPT","HIP","LIP","AFFIL","SHY","AGGRESS","FRUST","DEPRESS","FEAR")
  
  #Labels for plotting, must be in same order as 'PlotOrd'
  PlotLab = c("Attention","Inhibitory Control","Activation Control","Perceptual Sensitivity","High Intensity Pleasure",
              "Low Intensity Pleasure","Affiliation","Shyness","Aggression","Frustration","Depressive Mood","Fear")
  
} else if(Reporter == "Parent") {
  #Shortened varible names for MPLus, must be in order of file
  RenameVariables = c("MERGEID","ACTCONT", "AFFIL", "AGGRESS", "ATTFOC", "DEPRESS","FEAR", "FRUST", 
                      "INHIB", "SHY", "HIP")
  
  #Order of variables for plotting
  PlotOrd = c("ATTFOC","INHIB","ACTCONT","HIP","AFFIL","SHY","AGGRESS","FRUST","DEPRESS","FEAR")
  
  #Labels for plotting, must be in same order as 'PlotOrd'
  PlotLab = c("Attention","Inhibitory Control","Activation Control","High Intensity Pleasure",
              "Affiliation","Shyness","Aggression","Frustration","Depressive Mood","Fear")
  
} else if (Reporter == "Parent & Child") {
  #Shortened varible names for MPLus, must be in order of file and dataframe (created below)
  RenameVariables = c("MERGEID","ACTCONTP", "AFFILP", "AGGRESSP", "ATTFOCP", "DEPRESSP","FEARP", "FRUSTP", 
                      "INHIBP", "SHYP", "HIPP", "ACTCONTC", "AFFILC", "AGGRESSC", "ATTFOCC","DEPRESSC", 
                      "FEARC",	"FRUSTC", "INHIBC",	"LIPC", "PERCEPTC", "SHYC", "HIPC")
  
  if(Separate) {#Order of variables for plotting (parent/child separate)
    PlotOrd = c("ATTFOCP","INHIBP","ACTCONTP","HIPP","AFFILP","SHYP","AGGRESSP","FRUSTP","DEPRESSP","FEARP",
                "ATTFOCC","INHIBC","ACTCONTC","PERCEPTC","HIPC","LIPC","AFFILC","SHYC","AGGRESSC","FRUSTC","DEPRESSC","FEARC")
    
    #Labels for plotting, must be in same order as 'PlotOrd'
    PlotLab = c("P: Attention","P: Inhibitory Control","P: Activation Control","P: High Intensity Pleasure",
                "P: Affiliation","P: Shyness","P: Aggression","P: Frustration","P: Depressive Mood","P: Fear",
                "C: Attention","C: Inhibitory Control","C: Activation Control","C: Perceptual Sensitivity","C: High Intensity Pleasure",
                "C: Low Intensity Pleasure","C: Affiliation","C: Shyness","C: Aggression","C: Frustration","C: Depressive Mood","C: Fear")
    
  } else {
    
    #Order of variables for plotting (parent/child aligned)
    PlotOrd = c("ATTFOCP","ATTFOCC","INHIBP","INHIBC","ACTCONTP","ACTCONTC","PERCEPTC","HIPP","HIPC",
                "LIPC","AFFILP","AFFILC","SHYP","SHYC","AGGRESSP","AGGRESSC","FRUSTP","FRUSTC","DEPRESSP","DEPRESSC","FEARP","FEARC")
    
    
    #Labels for plotting, must be in same order as 'PlotOrd'
    PlotLab = c("P: Attention","C: Attention","P: Inhibitory Control","C: Inhibitory Control","P: Activation Control","C: Activation Control",
                "C: Perceptual Sensitivity","P: High Intensity Pleasure","C: High Intensity Pleasure",
                "C: Low Intensity Pleasure","P: Affiliation","C: Affiliation","P: Shyness","C: Shyness","P: Aggression","C: Aggression",
                "P: Frustration","C: Frustration","P: Depressive Mood","C: Depressive Mood","P: Fear","C: Fear")
  }}
###########################################################################################

#Read in data
###########################################################################################
#Read in data as tidyverse tibble data frame

FullData <- read_csv(paste0(Location, Filename), na = c("", "NA", "-999"))

###########################################################################################

#Get EATQ and z scores
###########################################################################################
StatusVar <-  paste(Year, "_ADHD_STATUS", sep = "")

if(Reporter == "Child"){
  #Selecting controls only (status == 1), only EATQ variables, performing listwise deletion for missing data (all primary data is missing for these anyway)
  EATQ_Cont <-FullData %>%
    filter(FullData[StatusVar] == 1) %>% 
    select(MERGEID, EEG_C_EATQ_ACTIVCONT:EEG_C_EATQ_HIP) %>%
    na.omit()
  
  #Selecting ADHD only (status == 3), only EATQ scales, performing listwise deletion for missing data
  EATQ_ADHD <-FullData %>%
    filter(FullData[StatusVar] == 3) %>% 
    select(MERGEID, EEG_C_EATQ_ACTIVCONT:EEG_C_EATQ_HIP) %>%
    na.omit()
  
} else if(Reporter == "Parent"){
  #Selecting controls only (status == 1), only EATQ variables, performing listwise deletion for missing data (all primary data is missing for these anyway)
  EATQ_Cont <-FullData %>%
    filter(FullData[StatusVar] == 1) %>% 
    select(MERGEID, EEG_P_EATQ_ACTIVCONT:EEG_P_EATQ_HIP) %>%
    na.omit()
  
  #Selecting ADHD only (status == 3), only EATQ scales, performing listwise deletion for missing data
  EATQ_ADHD <-FullData %>%
    filter(FullData[StatusVar] == 3) %>% 
    select(MERGEID, EEG_P_EATQ_ACTIVCONT:EEG_P_EATQ_HIP) %>%
    na.omit()
  
} else if(Reporter == "Parent & Child"){
  #Selecting controls only (status == 1), only EATQ variables, performing listwise deletion for missing data (all primary data is missing for these anyway)
  EATQ_Cont <-FullData %>%
    filter(FullData[StatusVar] == 1) %>% 
    select(MERGEID, EEG_P_EATQ_ACTIVCONT:EEG_C_EATQ_HIP, -EEG_P_EATQ_EFFCONT, -EEG_P_EATQ_SURGENCY, -EEG_P_EATQ_NEGAFFECT) %>%
    na.omit()
  
  #Selecting ADHD only (status == 3), only EATQ scales, performing listwise deletion for missing data
  EATQ_ADHD <-FullData %>%
    filter(FullData[StatusVar] == 3) %>% 
    select(MERGEID, EEG_P_EATQ_ACTIVCONT:EEG_C_EATQ_HIP, -EEG_P_EATQ_EFFCONT, -EEG_P_EATQ_SURGENCY, -EEG_P_EATQ_NEGAFFECT) %>%
    na.omit()
  
} else {"Reporter variable not defined"}

#Get list of control subject means and SDs by EATQ scale
ContMeans = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,mean)
ContSD = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,sd)

#Convert ADHD scale scores to z scores based on control means using sweep() and attach MERGEID
EATQ_ADHD_z_Cont = cbind(EATQ_ADHD[,1],sweep(EATQ_ADHD[,2:ncol(EATQ_ADHD)],2,ContMeans,"-")  %>%
                           sweep(2,ContSD,"/"))
#For readability and compatibility with Mplus, shorten variable names
names(EATQ_ADHD_z_Cont) <- RenameVariables

#Convert Control scale scores z scores based on control means using scale() and attach MERGEID
EATQ_Cont_z_Cont = cbind(EATQ_Cont[,1],scale(EATQ_Cont[,2:ncol(EATQ_Cont)]))

#For readability and Mplus, shorten variable names
names(EATQ_Cont_z_Cont) <- RenameVariables

#Get normal zscores for ADHD (ADHD-referenced)
EATQ_ADHD_z_NotReferenced <- cbind(EATQ_ADHD[,1],scale(EATQ_ADHD[,2:ncol(EATQ_ADHD)]))
###########################################################################################

#Prep Mplus files to run
###########################################################################################

if(Group == "ADHD") {
  ZData <- EATQ_ADHD_z_Cont
} else if(Group == "Control") {
    ZData <- EATQ_Cont_z_Cont
  }

#Creates FAMID variable for clustering in Mplus
ZData$FAMID <- as.numeric(substr(ZData$MERGEID, start = 1, stop =5 ))

#Use MplusAutomation to prepare file for Mplus (remove header, missing as ".", save as .txt)
prepareMplusData(ZData, paste(MplusLocation, MplusDataFile, sep ="/"))

#Create txt file designating iterative Mplus models
writeLines(paste(
  '[[init]]
iterators = classes;
classes = ',FirstClass,':',LastClass,';
filename = "[[classes]]-class LPA ', Group,' ', Reporter, '.inp";
outputDirectory = "', MplusLocation, '";
[[/init]]

TITLE: LPA ',Group,' ',Reporter,' Comparison ;
DATA: FILE = "', paste(MplusLocation, MplusDataFile,sep = "/"), '";

VARIABLE: 
NAMES ARE
', paste(RenameVariables, collapse = " \n"),'
FAMID;

IDVARIABLE IS MERGEID;

CLUSTER IS FAMID;

USEVARIABLES ARE 
', paste(RenameVariables[2:length(RenameVariables)], collapse = " \n"),' ;

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

OUTPUT:
!TECH11 PROVIDES LMR-LRT and TECH14 PROVIDES BLRT (but not available for type = complex)
TECH1 TECH4 TECH11;

SAVEDATA:
FILE IS "[[classes]]-class  ', Group,' ', Reporter,' Ouput.csv";
SAVE = CPROB;'
  , sep = ""), paste(MplusLocation, MplusLoopFile, sep ="/"))


#Uses the script above to create individual input files for Mplus
createModels(paste(MplusLocation, MplusLoopFile, sep ="/"))
###########################################################################################

#Run input files in Mplus and extract the model data
###########################################################################################

#Runs all the input files in the directory
runModels(MplusLocation, replaceOutfile = "modifiedDate")

#Get output from Mplus output files
MplusOutput <- readModels(MplusLocation)
###########################################################################################

#Get and plot fit information for each model
###########################################################################################

#Build a dataframe with all the summary/fit indice information for each model using the current measure 
SummariesAll <- NULL
for (i in paste0("X",FirstClass:LastClass,".class.lpa.adhd.parent...child.out")) {
  SummariesAll <- bind_rows(SummariesAll, MplusOutput[[i]]$summaries)
} 

#Create a number of classes variable from the Mplus output filename, 
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
  ggtitle(paste0("Fit Indices: EATQ LPA"))

###########################################################################################

# Plot mean data by variable and class
###########################################################################################

#Specify model number
ModelNumber <- 5

#Get Mplus output for a specific model number
MplusDataOut <- MplusOutput[[paste0("X",ModelNumber,".class.lpa.adhd.parent...child.out")]]$savedata

#Get total number of variables for plotting
VarNum = length(RenameVariables)-1

#Summarize data by placing in long format then aggregating by getting mean by variable and class
ClassMeanData <- MplusDataOut %>%
  select(1:VarNum,C)%>%
  melt(id.vars ="C") %>%
  dcast(C + variable ~ "value", fun.aggregate = mean, na.rm = TRUE)

#Get proportion of subjects in each class
ClassProp = prop.table(table(MplusDataOut$C))

#Plot class means by variable
ClassMeanPlot <- ggplot(ClassMeanData, aes(x = variable, y = value, group = as.factor(C), color = as.factor(C))) +
  geom_hline(yintercept=0,color = "black", alpha = 0.3)+ #intercept
  geom_point(size = 2.25) + #mean data points
  geom_line(size = 1.25) + #line connecting data points
  theme_bw(base_size = 14)+ #use light theme and set base font size
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5)) + #right align and angle x axis text and center plot title
  scale_x_discrete(limits = PlotOrd, labels = PlotLab)+ #provide order and labels for x axis
  coord_cartesian(ylim = yLimits)+ #zoom y axis scale
  labs(x = NULL, y = 'z-score (control referenced)') + #x and y axis labels
  ggtitle(paste("Concurrent ", Reporter, " EATQ ", NumClass, "-Class LPA from ", Year," ",Group," Status", sep =""))+ #build plot title from variables at top
  scale_color_manual(values = pal, #color palette for classes
                     name = "Class", #legend title
                     labels = c(paste("1 (", round(ClassProp[1]*100),"%)", sep = ""),
                                paste("2 (", round(ClassProp[2]*100),"%)", sep = ""),
                                paste("3 (", round(ClassProp[3]*100),"%)", sep = ""),
                                paste("4 (", round(ClassProp[4]*100),"%)", sep = ""),
                                paste("5 (", round(ClassProp[5]*100),"%)", sep = ""))) #provide class label and percentage for legend

###########################################################################################