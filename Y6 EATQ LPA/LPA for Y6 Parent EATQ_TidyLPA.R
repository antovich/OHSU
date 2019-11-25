#LPA for parent EATQ temperament measure scales using Y6 data for ADHD and control separately
#All values standardized against control mean
#Using tidyLPA

#Set working drive
setwd('H:/Projects/Y6 EATQ LPA for Sarah n400')

#Load data manipulation and LPA packages
library(tidyverse)
library(tidyLPA)
library(reshape2)


#Read in data as tidyverse tibble data frame
EATQ <- read_csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Y6 EATQ and Demo data 8-21-19.csv', na = c("", "NA", "-999"))

#Read in Y1 data for Y1 status variable and rename variable for consistency
Y1Data <- read_csv('H:/Projects/Y1 Factor Scores for Mike n1400/Data/all_data_adhd_ML_project_persistent_11-Sep-2018.csv', na = c("-999","NA",""))%>%
  rename("Y1_ADHD_STATUS" = DX)

#Join EATQ dataset with Y1 status variable
EATQ <- left_join(EATQ, select(Y1Data, MERGEID, Y1_ADHD_STATUS), by = "MERGEID") 

# #Optional - Write to file.
#write.csv(EATQ, "Y6 EATQ & Demographic with Y1 Status.csv", row.names = FALSE)

# #Optional - Look at patterns of change in diagnosis
# #Get Y1 and Y6 status
# DXchange = paste(EATQ$Y1_ADHD_STATUS, EATQ$Y6_ADHD_STATUS, sep = " to ")
# #Convert to dataframe
# DXchange = as.data.frame(table(DXchange))
# #Substitute dx number with diagnosis string
# DXchange[,1] = gsub("1", "Control", DXchange[,1])
# DXchange[,1] = gsub("2","Subthreshold", DXchange[,1])
# DXchange[,1] = gsub("3","ADHD", DXchange[,1])
# DXchange[,1] = gsub("4","Not clean control", DXchange[,1])


#Selecting controls only (status == 1), only EATQ variables, performing listwise deletion for missing data (all primary data is missing for these anyway)
EATQ_Cont <-EATQ %>%
  filter(Y1_ADHD_STATUS == 1) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Selecting ADHD only (status == 3), only EATQ scales, performing listwise deletion for missing data
EATQ_ADHD <-EATQ %>%
  filter(Y1_ADHD_STATUS == 3) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Get list of control subject means and SDs by EATQ scale
ContMeans = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,mean)
ContSD = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,sd)

#Convert ADHD scale scores to z scores based on control means using sweep() and attach MERGEID
EATQ_ADHD_z_Cont = cbind(EATQ_ADHD[,1],sweep(EATQ_ADHD[,2:ncol(EATQ_ADHD)],2,ContMeans,"-")  %>%
                           sweep(2,ContSD,"/"))
#For readability and compatibility with Mplus, shorten variable names
names(EATQ_ADHD_z_Cont) <- c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","Shy","HIP")

#Convert Control scale scores z scores based on control means using scale() and attach MERGEID
EATQ_Cont_z_Cont = cbind(EATQ_Cont[,1],scale(EATQ_Cont[,2:ncol(EATQ_Cont)]))
                          
#For readability and Mplus, shorten variable names
names(EATQ_Cont_z_Cont) <- c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","Shy","HIP")

# #Optional write zscore data to csv
#write.csv(EATQ_ADHD_z_Cont, 'Y6 EATQ ADHD zScores (control referenced).csv', row.names = FALSE)
#write.csv(EATQ_Cont_z_Cont, 'Y6 EATQ Control zScores.csv', row.names = FALSE)

# #Optional - Get normal zscores for ADHD (ADHD-referenced)
#TMCQ_ADHD_z <- cbind(EATQ_ADHD[,1],scale(EATQ_ADHD[,2:ncol(EATQ_ADHD)]))

#Set seed for number generation to ensure reproducability
set.seed(123)

#Get profiles using mclust by default using estimate profiles(df, number of profiles) 
#Can adjust variance structure or send to Mplus (instead of mclust in R) with package = "MplusAutomation)
ADHD_Mod <-estimate_profiles(EATQ_ADHD_z_Cont[,2:11],3)
Cont_Mod <- estimate_profiles(EATQ_Cont_z_Cont[,2:11],3)

# #Compare different models using specific fit indices
# compare_solutions(ADHD_Mod, statistics = c("BIC","SABIC"))

# #Get all available fit indices for the model
# ADHD_Fit <- get_fit(ADHD_Mod)
# Cont_Fit <- get_fit(Cont_Mod)
# 
# #Reshape fit indice dataframe for plotting
# ADHD_Fit_Melt <- melt(ADHD_Fit, id.vars = "Classes", measure.vars = c("LogLik","AIC","BIC","SABIC","Entropy","BLRT_p" ),
#                       variable.name = "FitIndice")
# Cont_Fit_Melt <- melt(Cont_Fit, id.vars = "Classes", measure.vars = c("LogLik","AIC","BIC","SABIC","Entropy","BLRT_p" ),
#                       variable.name = "FitIndice")
# #Plot fit indices
# ggplot(ADHD_Fit_Melt,aes(Classes,value))+
#   geom_point(size =2, color = "blue")+
#   geom_line(size = 1.25, color = "blue")+
#   theme_bw(base_size = 14, base_family = "sans") +
#   facet_grid(FitIndice ~ ., scales= "free_y") +
#   scale_x_continuous(name = "Number of Classes (Cont Y1 DX)", breaks = c(1:10), labels = c(1:10)) +
#   ylab("Indice Value")

#Get dateframe with raw data and classes
ADHD_Classes <- add_column(get_data(ADHD_Mod), MERGEID = EATQ_ADHD_z_Cont$MERGEID, .before = 1)
Cont_Classes <- add_column(get_data(Cont_Mod), MERGEID = EATQ_Cont_z_Cont$MERGEID, .before = 1)

#Frequency tables for the new class variable
ADHDclassCount = count(ADHD_Classes, Class)
ADHDclassCount = cbind(ADHDclassCount,percent = prop.table(ADHDclassCount$n))

ContclassCount = count(Cont_Classes, Class)
ContclassCount = cbind(ContclassCount,percent = prop.table(ContclassCount$n))

# #Plot with tidyLPA's native plotting
# plot_profiles(ADHD_Mod, sd = FALSE, rawdata = FALSE)

# Reshape data for plotting

#Melt dataframe into long format (scales as rows)
ADHD_Melt <- melt(ADHD_Classes, id.vars = "Class", measure.vars = 4:13, variable.name = "Features")
Cont_Melt <- melt(Cont_Classes, id.vars = "Class", measure.vars = 4:13, variable.name = "Features")

#Cast as averages within feature by class
ADHD_Cast <- dcast(ADHD_Melt, Features + Class~"Mean", mean)
ADHD_Cast$Class <- as.factor(ADHD_Cast$Class)

Cont_Cast <- dcast(Cont_Melt, Features + Class~"Mean", mean)
Cont_Cast$Class <- as.factor(Cont_Cast$Class)

# Plot scale averages by profile
adhdPlot <- ADHD_Cast %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Class, color = Class)) +
  scale_color_hue(name="Profile", labels=c(
                      paste("1 (", round(ADHDclassCount$percent[1]*100),"%)", sep = ""),
                      paste("2 (", round(ADHDclassCount$percent[2]*100),"%)", sep = ""),
                      paste("3 (", round(ADHDclassCount$percent[3]*100),"%)", sep = ""),
                      paste("4 (", round(ADHDclassCount$percent[4]*100),"%)", sep = ""))) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  ylim(-3.5,2.5) +
  #Order of features for figure
  scale_x_discrete(limits = c("AttFoc",
                              "Inhib",
                              "ActCont",
                              "Shy",
                              "HIP",
                              "Affil",
                              "Aggress",
                              "Frust",
                              "Depress",
                              "Fear"),

                   #Labels for features
                   labels = c("Attention",
                              "Inhibitory Control",
                              "Activation Control",
                              "Shyness",
                              "High Intensity Pleasure",
                              "Affiliation",
                              "Aggression",
                              "Frustration",
                              "Depressive Mood",
                              "Fear")) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = "ADHD zScore (control referenced)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Y1 Status LPA 3-Class")

# Plot scale averages by profile
contPlot <- Cont_Cast %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Class, color = Class)) +
  scale_color_hue(name="Profile", labels=c(
    paste("1 (", round(ContclassCount$percent[1]*100),"%)", sep = ""),
    paste("2 (", round(ContclassCount$percent[2]*100),"%)", sep = ""),
    paste("3 (", round(ContclassCount$percent[3]*100),"%)", sep = ""),
    paste("4 (", round(ContclassCount$percent[4]*100),"%)", sep = ""))) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  ylim(-3.5,2.5) +
  #Order of features for figure
  scale_x_discrete(limits = c("AttFoc",
                              "Inhib",
                              "ActCont",
                              "Shy",
                              "HIP",
                              "Affil",
                              "Aggress",
                              "Frust",
                              "Depress",
                              "Fear"),
                   
                   #Labels for features
                   labels = c("Attention",
                              "Inhibitory Control",
                              "Activation Control",
                              "Shyness",
                              "High Intensity Pleasure",
                              "Affiliation",
                              "Aggression",
                              "Frustration",
                              "Depressive Mood",
                              "Fear")) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = "Control zScore") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Y1 Status LPA 3-Class")

#Load dynamic ploting package
library(plotly)

#Plot as interactive graph (dynamic version can be saved as cloud share file only)
ggplotly(adhdPlot, tooltip = c("Features", "Mean")) %>%
  layout(legend = list(orientation = "h", y = 1.2))

