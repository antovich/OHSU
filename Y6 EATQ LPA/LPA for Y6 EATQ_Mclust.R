
#LPA for TMCQ temperament measure scales using Y6 data for ADHD and control separately
#All values standardized against control mean

#Load data manipulation package
library(tidyverse)

#Read in data
EATQ <- read_csv('H:/Projects/Data/Y6 EATQ LPA for Sarah/Y6 EATQ and Demo data 8-8-19.csv', na = c("", "NA", "-999"))

#Selecting controls only (DX == 1), only EATQ variables, performing listwise deletion for missing data
EATQ_Cont <-EATQ %>%
  filter(Y6_ADHD_STATUS == 1) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Selecting ADHD only (DX == 3), only EATQ scales, performing listwise deletion for missing data
EATQ_ADHD <-EATQ %>%
  filter(Y6_ADHD_STATUS == 3) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Get list of control subject means and SDs by EATQ scale
ContMeans = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,mean)
ContSD = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,sd)

#Convert ADHD scale scores to z scores based on control means
EATQ_ADHD_z_Cont = cbind(EATQ_ADHD[,1],sweep(EATQ_ADHD[,2:ncol(EATQ_ADHD)],2,ContMeans,"-")  %>%
  sweep(2,ContSD,"/"))

#Convert Control scale scores z scores based on control means
EATQ_Cont_z_Cont = cbind(EATQ_Cont[,1],sweep(EATQ_Cont[,2:ncol(EATQ_Cont)],2,ContMeans,"-")  %>%
                           sweep(2,ContSD,"/"))

write.csv(EATQ_ADHD_z_Cont, 'ADHDzScores.csv', row.names = FALSE)
write.csv(EATQ_Cont_z_Cont, 'CONTzScores.csv', row.names = FALSE)

#Optional get normal zscores for ADHD 
#TMCQ_ADHD_z <- as.data.frame(scale(TMCQ_ADHD, center = TRUE, scale = TRUE))

#Load cluster analysis package
library(mclust)

#Run cluster analysis and plot BIC and get summary to determine best model (about a min to run)
BIC <- mclustBIC(EATQ_ADHD_z_Cont)
plot(BIC)
summary(BIC)

#Get ICL criterion to compare with BIC 
ICL <- mclustICL(EATQ_ADHD_z_Cont)
plot(ICL)

#From summary, selected best model and get summary of that model 
#For visualization/description of model types: https://journal.r-project.org/archive/2016/RJ-2016-021/RJ-2016-021.pdf
mod1 <- Mclust(EATQ_ADHD_z_Cont, modelNames = "EII", G = 2, x = BIC)
summary(mod1)

#Load data manipulation package
library(reshape2)

#Get the means for each variable for each latent group from the selected model
means <- data.frame(mod1$parameters$mean, stringsAsFactors = FALSE) %>%
  #Turn rownames into a separate column (to prevent them from being dropped during reshaping)
  rownames_to_column() %>%
  #Rename new rowname column
  rename(Features = rowname) %>%
  #Melt to create longform data (all means in one column)
  melt(id.vars = "Features", variable.name = "Profile", value.name = "Mean")

p <- means %>%
  #Provide names for profiles after examination
  mutate(Profile = recode(Profile, 
                          X1 = "GROUP1: ?%",
                          X2 = "GROUP2: ?%")) %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  #Order of features for figure
  #scale_x_discrete(#limits = c("Y1_P_TMCQ_DISCOMF",
                              #"Y1_P_TMCQ_ANGER",
                              #"Y1_P_TMCQ_ASSERT",
                              #"Y1_P_TMCQ_FEAR",
                              #"Y1_P_TMCQ_SAD",
                              #"Y1_P_TMCQ_SHY",
                              #"Y1_P_TMCQ_OPENNESS",
                              #"Y1_P_TMCQ_AFFIL",
                              #"Y1_P_TMCQ_HIP",
                              #"Y1_P_TMCQ_LIP",
                              #"Y1_P_TMCQ_PERCEPT",
                              #"Y1_P_TMCQ_ACTIVITY",
                              #"Y1_P_TMCQ_ACTIVCONT",
                              #"Y1_P_TMCQ_ATTFOCUS",
                              #"Y1_P_TMCQ_IMPULS",
                              #"Y1_P_TMCQ_SOOTHE",
                              #"Y1_P_TMCQ_INHIBIT"),
                   #Labels for features
                  # labels = c("Discomfort",
                              #"Anger/Frustration",
                              #"Assertiveness/Dominance",
                              #"Fear",
                              #"Sadness",
                              #"Shyness",
                              #"Fantasy/Openness",
                              #"Affiliation",
                              #"High Intensity Pleasure",
                              #"Low Intensity Pleasure",
                              #"Perceptual Sensitivity",
                              #"Activity Level",
                              #"Activation Control",
                              #"Attentional Focusing",
                              #"Impulsivity",
                              #"Soothability/Falling Reactivity",
                              #"Inhibitory Control")) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = "z score based on Control mean") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

#Load ploting package
library(plotly)

ggplotly(p, tooltip = c("Features", "Mean")) %>%
  layout(legend = list(orientation = "h", y = 1.2))

