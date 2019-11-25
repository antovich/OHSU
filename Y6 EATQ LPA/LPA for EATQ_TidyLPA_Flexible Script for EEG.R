#LPA using TidyLPA for EATQ temperament scales using Y6 data for ADHD and control separately
#Parent and child report and Y1 or Y6 ADHD status
#EATQ values standardized against control mean

#Background tasks
###########################################################################################
#Clear variables? 'TRUE' or 'FALSE'
ClearWorkspace = TRUE
    if(ClearWorkspace) {rm(list = ls())}

#Set seed to ensure reproducability
set.seed(123)

#Set working drive
setwd('H:/Projects/Y6 EATQ LPA for Sarah n400')

#Load data manipulation and LPA packages
library(tidyverse)
library(tidyLPA)
library(reshape2)
library(wesanderson)
library(RColorBrewer)
###########################################################################################

#OPTIONS 
###########################################################################################

#Filename
Filename = 'EEG Dteam_EATQ Dataset 10-1-19 (not final).csv'

#Location
Location = 'H:/Projects/Y6 EATQ LPA for Sarah n400/Data/'

#Number of classes
NumClass = 2

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

#Fit indices to plot (see list and descriptions below)
PlotIndices = c("LogLik","AIC","BIC","SABIC", "Entropy","BLRT_p")

#setup palette for plots (a few different options)
pal1 = wes_palette("Darjeeling1")
pal2 = brewer.pal(4, "Set1")
pal3 = c('#30B8DA','#FF6347','#2FCA72','#FFC34A', '#8f34eb')
  
#select palette
pal = pal3
###########################################################################################

#Write files?
###########################################################################################
#Write joined data to file
WriteJoinData = FALSE
WriteJoinName = "Y6 EATQ & Demographic with Y1 Status.csv"

#Write z-score data to file
WriteZData = FALSE
WriteZName = c(paste("ADHD", Year, "Status", Reporter, 'EATQ zScores.csv'),paste("Control", Year, "Status", Reporter, 'EATQ zScores.csv'))

#Write full class data to file
WriteClassFullData = FALSE
WriteClassFullName = paste(Group, Year, "Status", Reporter, 'EATQ LPA Class and zScores EEG Sample.csv')

#Append additional class assignments to file
AppendClassData = TRUE
###########################################################################################

#Varible ordering
###########################################################################################

if(Reporter == "Child"){
#Shortened varible names for MPLus, must be in order of file
RenameVariables = c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","LIP", "Percept", "Shy","HIP")

#Order of variables for plotting
PlotOrd = c("AttFoc","Inhib","ActCont","Percept","HIP","LIP","Affil","Shy","Aggress","Frust","Depress","Fear")

#Labels for plotting, must be in same order as 'PlotOrd'
PlotLab = c("Attention","Inhibitory Control","Activation Control","Perceptual Sensitivity","High Intensity Pleasure",
            "Low Intensity Pleasure","Affiliation","Shyness","Aggression","Frustration","Depressive Mood","Fear")

} else if(Reporter == "Parent") {
#Shortened varible names for MPLus, must be in order of file
RenameVariables = c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","Shy","HIP")

#Order of variables for plotting
PlotOrd = c("AttFoc","Inhib","ActCont","HIP","Affil","Shy","Aggress","Frust","Depress","Fear")

#Labels for plotting, must be in same order as 'PlotOrd'
PlotLab = c("Attention","Inhibitory Control","Activation Control","High Intensity Pleasure",
            "Affiliation","Shyness","Aggression","Frustration","Depressive Mood","Fear")

} else if (Reporter == "Parent & Child") {
#Shortened varible names for MPLus, must be in order of file and dataframe (created below)
RenameVariables = c("MERGEID","ActCont_P", "Affil_P", "Aggress_P", "AttFoc_P", "Depress_P","Fear_P", "Frust_P", 
                    "Inhib_P", "Shy_P", "HIP_P", "ActCont_C", "Affil_C", "Aggress_C", "AttFoc_C","Depress_C", 
                    "Fear_C",	"Frust_C", "Inhib_C",	"LIP_C", "Percept_C", "Shy_C", "HIP_C")

if(Separate) {#Order of variables for plotting (parent/child separate)
PlotOrd = c("AttFoc_P","Inhib_P","ActCont_P","HIP_P","Affil_P","Shy_P","Aggress_P","Frust_P","Depress_P","Fear_P",
            "AttFoc_C","Inhib_C","ActCont_C","Percept_C","HIP_C","LIP_C","Affil_C","Shy_C","Aggress_C","Frust_C","Depress_C","Fear_C")

#Labels for plotting, must be in same order as 'PlotOrd'
PlotLab = c("P: Attention","P: Inhibitory Control","P: Activation Control","P: High Intensity Pleasure",
            "P: Affiliation","P: Shyness","P: Aggression","P: Frustration","P: Depressive Mood","P: Fear",
            "C: Attention","C: Inhibitory Control","C: Activation Control","C: Perceptual Sensitivity","C: High Intensity Pleasure",
            "C: Low Intensity Pleasure","C: Affiliation","C: Shyness","C: Aggression","C: Frustration","C: Depressive Mood","C: Fear")

} else {

#Order of variables for plotting (parent/child aligned)
PlotOrd = c("AttFoc_P","AttFoc_C","Inhib_P","Inhib_C","ActCont_P","ActCont_C","Percept_C","HIP_P","HIP_C",
            "LIP_C","Affil_P","Affil_C","Shy_P","Shy_C","Aggress_P","Aggress_C","Frust_P","Frust_C","Depress_P","Depress_C","Fear_P","Fear_C")
           

#Labels for plotting, must be in same order as 'PlotOrd'
PlotLab = c("P: Attention","C: Attention","P: Inhibitory Control","C: Inhibitory Control","P: Activation Control","C: Activation Control",
            "C: Perceptual Sensitivity","P: High Intensity Pleasure","C: High Intensity Pleasure",
            "C: Low Intensity Pleasure","P: Affiliation","C: Affiliation","P: Shyness","C: Shyness","P: Aggression","C: Aggression",
            "P: Frustration","C: Frustration","P: Depressive Mood","C: Depressive Mood","P: Fear","C: Fear")
}}
###########################################################################################

#READ IN DATA
###########################################################################################
#Read in data as tidyverse tibble data frame
FullData <- read_csv(paste0(Location, Filename), na = c("", "NA", "-999"))

#Read in Y1 data for Y1 status variable and rename variable for consistency
Y1Data <- read_csv('H:/Projects/Y1 Factor Scores for Mike n1400/Data/all_data_adhd_ML_project_persistent_11-Sep-2018.csv', na = c("-999","NA",""))%>%
  rename("Y1_ADHD_STATUS" = DX)
###########################################################################################

#Join EATQ dataset with Y1 status variable
###########################################################################################
FullData <- left_join(FullData, select(Y1Data, MERGEID, Y1_ADHD_STATUS), by = "MERGEID") 

#Write joined dataset to file
if(WriteJoinData){
write.csv(FullData, WriteJoinName, row.names = FALSE)}
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

if(WriteZData) {
#Write zscore data to csv
write.csv(EATQ_ADHD_z_Cont, WriteZName[1], row.names = FALSE)
write.csv(EATQ_Cont_z_Cont, paste0(Location, WriteZName[2]), row.names = FALSE)
}

#Get normal zscores for ADHD (ADHD-referenced)
EATQ_ADHD_z_NotReferenced <- cbind(EATQ_ADHD[,1],scale(EATQ_ADHD[,2:ncol(EATQ_ADHD)]))
###########################################################################################

#LPA analysis
###########################################################################################
#Get profiles using mclust by default using estimate profiles(df, number of profiles) 
#Can adjust variance structure or send to Mplus (instead of mclust in R) with package = "MplusAutomation)
ADHD_Mod <-estimate_profiles(EATQ_ADHD_z_Cont[,2:ncol(EATQ_ADHD_z_Cont)],NumClass)
Cont_Mod <- estimate_profiles(EATQ_Cont_z_Cont[,2:ncol(EATQ_Cont_z_Cont)],NumClass)

#Get all available fit indices for the model
ADHD_Fit <- get_fit(ADHD_Mod)
Cont_Fit <- get_fit(Cont_Mod)
#Remove non-informative 1.0 values for entropy and probability for 1-class model
if(1 %in% ADHD_Fit$Model) {
  ADHD_Fit$Entropy[1]=NA
  ADHD_Fit$prob_max[1]=NA
  ADHD_Fit$prob_min[1]=NA}
if(1 %in% Cont_Fit$Model) {
  Cont_Fit$Entropy[1]=NA
  Cont_Fit$prob_max[1]=NA
  Cont_Fit$prob_min[1]=NA}
###########################################################################################

if(length(NumClass)>1){
  
#Model fit and fit indice plotting
###########################################################################################
#Compare different models using specific fit indices
FitCompare <- compare_solutions(ADHD_Mod, statistics = c("BIC","SABIC"))

#Reshape fit indices dataframe for plotting
ADHD_Fit_Melt <- melt(ADHD_Fit, id.vars = "Classes", measure.vars = PlotIndices,
                      variable.name = "FitIndice")
Cont_Fit_Melt <- melt(Cont_Fit, id.vars = "Classes", measure.vars = PlotIndices,
                      variable.name = "FitIndice")
MplusFit <- cbind("Classes" = 1:10, 
                  "FitIndice" = "VLMR", 
                  "value" = c(NA, 0.0167,0.3509,0.4243, 0.5871, 0.4939,0.5659,0.4323,0.8094,0.6518)) %>%
  as.data.frame()%>%
  mutate(value = as.numeric(as.character(value)), Classes = as.numeric(as.character(Classes)))

Cont_Fit_Melt <- rbind(Cont_Fit_Melt, MplusFit)


#Plot fit indices
# LogLik: Log-likelihood of the data, given the model.
# AIC: Aikake information criterion; based on -2 log-likelihood, and penalized by number of parameters.
# AWE: Approximate weight of evidence; combines information on model fit and on classification errors (Celeux et al., 1997).
# BIC: Bayesian information criterion; based on -2 log-likelihood, and penalized by number of parameters adjusted by sample size.
# CAIC: Consistent Aikake information criterion; based on -2 log-likelihood, and penalized by number of parameters adjusted by sample size.
# CLC: Classification Likelihood Criterion; based on -2 log-likelihood, and penalized by the entropy (Biernacki, 1997).
# KIC: Kullback information criterion; based on -2 log-likelihood, and penalized by 3 times the number of parameters -1 (Cavanaugh, 1999).
# SABIC: Sample size-adjusted bayesian information criterion (Sclove, 1987).
# ICL: Integrated completed likelihood (Biernacki, Celeux, & Govaert, 2000).
# Entropy: A measure of classification uncertainty, reverse-coded so that 1 reflects complete certainty of classification, and 0 complete uncertainty (see Celeux & Soromenho, 1996).
# prob_min.: Minimum of the diagonal of the average latent class probabilities for most likely class membership, by assigned class. The minimum should be as high as possible, reflecting greater classification certainty (cases are assigned to classes they have a high probability of belonging to; see Jung & Wickrama, 2008).
# prob_max.: Maximum of the diagonal of the average latent class probabilities for most likely class membership, by assigned class. The maximum should also be as high as possible, reflecting greater classification certainty (cases are assigned to classes they have a high probability of belonging to).
# n_min.: Proportion of the sample assigned to the smallest class (based on most likely class membership).
# n_max.: Proportion of the sample assigned to the largest class (based on most likely class membership).
# BLRT_val: bootstrapped likelihood test.
# BLRT_p: p-value for the bootstrapped likelihood ratio test.

if(Group == "ADHD"){
ADHDFitPlots <- ggplot(ADHD_Fit_Melt,aes(Classes,value))+
            geom_point(size =2, color = "#00AFBB")+
            geom_line(size = 1.25, color = "#00AFBB")+
            theme_bw(base_size = 10, base_family = "sans") +
            theme(plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size = 8))+
            facet_grid(FitIndice ~ ., scales= "free_y") +
            scale_x_continuous(name = paste("Number of Classes"), breaks = c(NumClass), labels = c(NumClass)) +
            ylab("Indice Value") +
            ggtitle(paste(Group, " ", Year," Status ", Reporter, " EATQ LPA \nFit Indices by Number of Classes", sep =""))
ADHDFitPlots

} else if(Group == "Control"){
ContFitPlots <- ggplot(Cont_Fit_Melt,aes(Classes,value))+
            geom_point(size =2, color = "#00AFBB")+
            geom_line(size = 1.25, color = "#00AFBB")+
            theme_bw(base_size = 12, base_family = "sans") +
            theme(plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size = 8))+
            facet_grid(FitIndice ~ ., scales= "free_y") +
            scale_x_continuous(name = paste("Number of Classes"), breaks = c(NumClass), labels = c(NumClass)) +
            ylab("Indice Value") +
            ggtitle(paste(Group, " ", Year," Status ", Reporter, " EATQ LPA \nFit Indices by Number of Classes", sep =""))
ContFitPlots
}
###########################################################################################
  
} else if(length(NumClass)==1){

#Plotting means by class
###########################################################################################
#Get dateframe with raw data and classes
ADHD_Classes <- add_column(get_data(ADHD_Mod), MERGEID = EATQ_ADHD_z_Cont$MERGEID, .before = 1)
Cont_Classes <- add_column(get_data(Cont_Mod), MERGEID = EATQ_Cont_z_Cont$MERGEID, .before = 1)

# # Reassign class labels to align after visual inspection, rename column heads for solutions being exported
# if(NumClass == 4 & Reporter == Parent) {
# ADHD_Classes4 <- mutate(ADHD_Classes, Class = recode(Class, "2" = 1, "4" = 2, "1" = 4)) %>% #"Old" = New
#   rename("CPROB1" = CPROB2, "CPROB2" = CPROB4, "CPROB4" = CPROB1) %>% #"New" = Old 
#   select(MERGEID, "4Class_Prob1" = CPROB1, "4Class_Prob2" = CPROB2, "4Class_Prob3" = CPROB3, "4Class_Prob4" = CPROB4, "4Class_Assign" = Class)
# } else if(NumClass == 2 & Reporter == Parent) {
#   ADHD_Classes2 <- select(ADHD_Classes, MERGEID, ActCont:HIP, "2Class_Prob1" = CPROB1, "2Class_Prob2" = CPROB2,"2Class_Assign" = Class)
# }
# 
# #Merge 2- and 4-class solutions and write new file to CSV
# write.csv(full_join(ADHD_Classes2, ADHD_Classes4, by = "MERGEID"), WriteClassFullName, row.names = FALSE)

# if(NumClass == 3 & Reporter == "Parent") {
#   ADHD_Classes <- mutate(ADHD_Classes, Class = recode(Class, "2" = 3, "3" = 2)) #"Old" = New
# }
# if(NumClass == 4 & Reporter == "Parent & Child") {
#   ADHD_Classes <- mutate(ADHD_Classes, Class = recode(Class, "3" = 2, "2" = 4, "4" = 3)) #"Old" = New
# }
# if(NumClass == 5 & Reporter == "Parent & Child") {
#   ADHD_Classes <- mutate(ADHD_Classes, Class = recode(Class, "5" = 3, "4" = 2, "2" = 1, "3" = 4, "1" = 5)) #"Old" = New
# }


ADHD_Classes_Out <- select(ADHD_Classes, 1:(ncol(ADHD_Classes)-sum(NumClass,1)), -model_number, -classes_number, Class)
names(ADHD_Classes_Out)[ncol(ADHD_Classes_Out)] <- paste("Classes_", NumClass, sep = "")

#Frequency tables for the new class variable
ADHDclassCount = count(ADHD_Classes, Class)
ADHDclassCount = cbind(ADHDclassCount,percent = prop.table(ADHDclassCount$n))

ContclassCount = count(Cont_Classes, Class)
ContclassCount = cbind(ContclassCount,percent = prop.table(ContclassCount$n))


# Reshape data for plotting

#Melt dataframe into long format (scales as rows)
ADHD_Melt <- melt(ADHD_Classes, id.vars = "Class", measure.vars = 4:(ncol(ADHD_Classes)-sum(NumClass,1)), variable.name = "Features")
Cont_Melt <- melt(Cont_Classes, id.vars = "Class", measure.vars = 4:(ncol(Cont_Classes)-sum(NumClass,1)), variable.name = "Features")

#Cast as averages within feature by class
ADHD_Cast <- dcast(ADHD_Melt, Features + Class~"Mean", mean)
ADHD_Cast$Class <- as.factor(ADHD_Cast$Class)

Cont_Cast <- dcast(Cont_Melt, Features + Class~"Mean", mean)
Cont_Cast$Class <- as.factor(Cont_Cast$Class)


# Plot scale averages by profile
if(Group == "ADHD"){
adhdPlot <- ADHD_Cast %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Class, color = Class)) +
  scale_color_manual(values=pal, name="Class", labels=c(
    paste("1 (", round(ADHDclassCount$percent[1]*100),"%)", sep = ""),
    paste("2 (", round(ADHDclassCount$percent[2]*100),"%)", sep = ""),
    paste("3 (", round(ADHDclassCount$percent[3]*100),"%)", sep = ""),
    paste("4 (", round(ADHDclassCount$percent[4]*100),"%)", sep = ""),
    paste("5 (", round(ADHDclassCount$percent[5]*100),"%)", sep = ""))) +
  geom_hline(yintercept=0,color = "black", alpha = 0.3)+
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_y_continuous(limits = yLimits, breaks = c(-4:4)) +
  #Order of features for figure
  scale_x_discrete(limits = PlotOrd, labels = PlotLab) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = 'z-score (control referenced)') +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste("Concurrent ", Reporter, " EATQ ", NumClass, "-Class LPA from ", Year," ",Group," Status", sep =""))

if(WriteClassFullData){
  #Save class table data to a CSV
  write.csv(ADHD_Classes_Out, WriteClassFullName, row.names = FALSE)
}

if(AppendClassData){
  #Save just classes to CSV
  temp_table <- read.csv(WriteClassFullName)
  temp_table <- cbind(temp_table, ADHD_Classes_Out[ncol(ADHD_Classes_Out)])
  write.csv(temp_table, WriteClassFullName, row.names = FALSE)
}

adhdPlot

} else if(Group == "Control"){
contPlot <- Cont_Cast %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Class, color = Class)) +
  scale_color_manual(values=pal, name="Class", labels=c(
    paste("1 (", round(ContclassCount$percent[1]*100),"%)", sep = ""),
    paste("2 (", round(ContclassCount$percent[2]*100),"%)", sep = ""),
    paste("3 (", round(ContclassCount$percent[3]*100),"%)", sep = ""),
    paste("4 (", round(ContclassCount$percent[4]*100),"%)", sep = ""),
    paste("5 (", round(ContclassCount$percent[5]*100),"%)", sep = ""))) +
  geom_hline(yintercept=0,color = "black", alpha = 0.3)+
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_y_continuous(limits = yLimits, breaks = c(-4:4)) +
  #Order of features for figure
  scale_x_discrete(limits = PlotOrd, labels = PlotLab) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = 'z-score') +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste("Y6 ", Reporter, " EATQ ", NumClass, "-Class LPA from ", Year," ",Group," Status", sep =""))

if(WriteClassFullData){
  #Save class table data to a CSV
  write.csv(Cont_Classes_Out, WriteClassFullName, row.names = FALSE)
}

if(AppendClassData){
  #Save just classes to CSV
  temp_table <- read.csv(WriteClassFullName)
  temp_table <- cbind(temp_table, Cont_Classes_Out[ncol(Cont_Classes_Out)])
  write.csv(temp_table, WriteClassFullName, row.names = FALSE)
}
contPlot
} else {"Group varible not defined"}
}
###########################################################################################