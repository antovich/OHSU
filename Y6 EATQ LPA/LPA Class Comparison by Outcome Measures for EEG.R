library(reshape2)
library(purrr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(RColorBrewer)

CurrRecode = FALSE

# Blue-green palette
# pal = c("#7fcdbb","#41b6c4","#1d91c0", "#253494")

# Blue palette
pal = c("#a8deff","#64bdf5","#2172ed","#0048b5")

# Orange palette
# pal = c("#ffc100","#ff9a00","#ff7400", "#ff4d00")

if(CurrRecode) {
  # Recode class variable from current LPA script for plotting (which must be run first)
  ADHD_Classes <- ADHD_Classes %>%
    mutate(Class = recode(Class, "2"=3,"3"=2))}

#Get Table 1 data to bring in the IQ and ADHDRS scores
Tab1 <- read.csv('H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c("", -999, -888, NA), fileEncoding = "UTF-8-BOM")

#Merge Table 1, class, and outcome variable data sets CHANGE CONT_CLASSes or ADHD_CLASSES depending on group of interest
OutcomeData <- left_join(select(ADHD_Classes,MERGEID,Class),
  select(FullData, MERGEID, YEAR, EEG_AGE, EEG_ADHD_STATUS, SEX, EEG_DTEAM_MOOD, EEG_DTEAM_ANX, EEG_DTEAM_CD, EEG_DTEAM_ODD), by = "MERGEID") %>%
  left_join(select(Tab1, MERGEID, WISCWIAT_FSIQ, Y5_P_ADHDRS_TOT_TS, Y6_P_ADHDRS_TOT_TS), by = "MERGEID")

#Get ADHD rating scale total t scores from the appropriate year based on the year variable from the outcome data set
#Initialize new variable
OutcomeData$ADHDRS_TOT_T <- NA
#Select data from appropriate year
OutcomeData$ADHDRS_TOT_T[OutcomeData$YEAR == 6] <- OutcomeData$Y6_P_ADHDRS_TOT_TS[OutcomeData$YEAR == 6]
OutcomeData$ADHDRS_TOT_T[OutcomeData$YEAR == 5] <- OutcomeData$Y5_P_ADHDRS_TOT_TS[OutcomeData$YEAR == 5]

#PROPORTION PLOTS
#####################################################################################

FilterVars <- c("SEX","EEG_ADHD_STATUS","EEG_DTEAM_MOOD","EEG_DTEAM_ANX","EEG_DTEAM_ODD","EEG_DTEAM_CD")
OutcomeDataMelt <- melt(OutcomeData, id = "Class") %>%
  filter(variable %in% FilterVars)

OutcomeDataFreq <- group_by(OutcomeDataMelt,Class,variable,value) %>%
  summarize(n = n())%>%
  mutate(freq = n/sum(n))

plot_list1 <- list()
CatVarList <- c("SEX", "EEG_ADHD_STATUS", "EEG_DTEAM_MOOD", "EEG_DTEAM_ANX", "EEG_DTEAM_ODD", "EEG_DTEAM_CD")
CatLabList <- list(c("M","F"), c("Control", "Subthresh", "ADHD", "NC Control", "Missing"),
                   c("No", "Yes","Missing"),c("No", "Yes","Missing"),c("No", "Yes","Missing"),c("No", "Missing"))
CatTitleList <- c("Gender", "Concurrent \nADHD Status", "Mood", "Anxiety", "ODD", "CD")

for(v in CatVarList) {
p <- ggplot(filter(OutcomeDataFreq, variable == v)) +
  geom_col(aes(as.factor(Class), freq, fill = as.factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = CatTitleList[[match(v, CatVarList)]] , labels = CatLabList[[match(v, CatVarList)]]) +
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
plot_list1[[match(v, CatVarList)]] <- p
}

plot_grid(plotlist = plot_list1, align = "v", axis = "lr", ncol = 2)

#MEAN PLOTS
#####################################################################################

OutcomeMeans <- OutcomeData %>%
  group_by(Class)%>%
  summarize_at(ContVarList, mean, na.rm = TRUE)%>%
  melt(id.var = "Class", value.name = "Mean")

plot_list2 <- list()
ContVarList <- c("EEG_AGE", "WISCWIAT_FSIQ", "ADHDRS_TOT_T")
ContTitleList <- c("Age", "Y1 WISC-WIAT IQ", "Concurrent \nADHD Rating Scale \nTotal t-score")
ContYLimitList <- list(c(12.5, 15.5), c(70,120), c(45,85))

for(v in ContVarList) {
  p <- ggplot(filter(OutcomeMeans, variable == v), aes(Class, Mean))+
    geom_col(fill = pal[2])+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "none")+
    ylab(ContTitleList[[match(v, ContVarList)]])+
    scale_x_continuous(breaks = c(1:length(unique(OutcomeMeans$Class))))+
    coord_cartesian(ylim = ContYLimitList[[match(v, ContVarList)]])
  plot_list2[[match(v, ContVarList)]] <- p
}

plot_grid(plotlist = plot_list2, align = "v", axis = "lr", ncol = 2)
