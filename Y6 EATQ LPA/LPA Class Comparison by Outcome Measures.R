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

OutcomeVars = c(
              #age
                "Y6_AGEYEARS",
              #gender
                "SEX",                    #sex (1=M, 2=F)
              #Y6 ADHD status             #Y6 status (1=control, 2=subthreshold, 3=ADHD, 4=not clean control)
                "Y6_ADHD_STATUS",
              #IQ
                "Y6_WISCWIAT_FSIQ",       #WISCWIAT full scale IQ
                "Y6_WAISWIAT_FSIQ",       #WAISWIAT full scale IQ
              #symptom severity (ADHD rating scale, symptom count, Conners)
                "Y6_P_C_ADHDRS_INTSX",    #inattentive symptom count
                "Y6_P_C_ADHDRS_HYPSX",    #hyperactive symptom count
                "Y6_P_C_ADHDRS_TOT_TS",   #overall tscore for ADHD rating scale
                "Y6_P_C_CON3_HYP_TS",     #conners hyperactive tscore
                "Y6_P_C_CON3_INT_TS",     #conners inattentive tscore
                "Y6_P_C_SWAN_TOT",        #total score from swan
              #concurrent Y6 co-morbidity (e.g., percentage of each),
                "Y6_DTEAM_MOOD_PAST",     #past mood disorder dignosis (1=N, 3=Y)
                "Y6_DTEAM_MOOD",          #curr mood disorder dignosis (1=N, 3=Y)
                "Y6_DTEAM_ANX",           #anxiety dignosis (1=N, 3=Y)
                "Y6_DTEAM_ODD",           #ODD dignosis (1=N, 3=Y)
                "Y6_DTEAM_CD",            #CD dignosis (1=N, 3=Y)
              #parent report SDQ impairment by profile
                "Y6_P_C_SDQ_IM_TS")       #SDQ impact tscore


if(CurrRecode) {
  # Recode class variable from current LPA script for plotting (which must be run first)
  ADHD_Classes <- ADHD_Classes %>%
    mutate(Class = recode(Class, "2"=3,"3"=2))}

OutcomeData <- left_join(select(ADHD_Classes,MERGEID,Class),select(FullData, MERGEID, OutcomeVars), by = "MERGEID") 

#PROPORTION PLOTS
#####################################################################################

FilterVars <- c("SEX","Y6_ADHD_STATUS","Y6_DTEAM_MOOD_PAST","Y6_DTEAM_MOOD","Y6_DTEAM_ANX","Y6_DTEAM_ODD","Y6_DTEAM_CD")
OutcomeDataMelt <- melt(OutcomeData, id = "Class") %>%
  filter(variable %in% FilterVars)

OutcomeDataFreq <- group_by(OutcomeDataMelt,Class,variable,value) %>%
  summarize(n = n())%>%
  mutate(freq = n/sum(n))


SexPlot <- ggplot(filter(OutcomeDataFreq, variable == "SEX")) +
  geom_col(aes(as.factor(Class), freq, fill = as.factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = "Gender", labels = c("M", "F"))+
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ADHDstatusPlot <- ggplot(filter(OutcomeDataFreq, variable == "Y6_ADHD_STATUS")) +
  geom_col(aes(as.factor(Class), freq, fill = factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = "Y6 ADHD \nStatus", labels = c( "Control",  "Subthresh", "ADHD","NC Control", "Missing"))+
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

MoodPastPlot <- ggplot(filter(OutcomeDataFreq, variable == "Y6_DTEAM_MOOD_PAST")) +
  geom_col(aes(as.factor(Class), freq, fill = as.factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = "Mood \n(Past)", labels = c("No", "Yes","Missing"))+
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

MoodPresentPlot <- ggplot(filter(OutcomeDataFreq, variable == "Y6_DTEAM_MOOD")) +
  geom_col(aes(as.factor(Class), freq, fill = as.factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = "Mood", labels = c("No", "Yes","Missing"))+
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

AnxPlot <- ggplot(filter(OutcomeDataFreq, variable == "Y6_DTEAM_ANX")) +
  geom_col(aes(as.factor(Class), freq, fill = as.factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = "Anxiety", labels = c("No", "Yes","Missing"))+
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ODDPlot <- ggplot(filter(OutcomeDataFreq, variable == "Y6_DTEAM_ODD")) +
  geom_col(aes(as.factor(Class), freq, fill = as.factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = "ODD", labels = c("No", "Yes","Missing"))+
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

CDPlot <- ggplot(filter(OutcomeDataFreq, variable == "Y6_DTEAM_CD")) +
  geom_col(aes(as.factor(Class), freq, fill = as.factor(value)))+
  scale_fill_manual(values = pal, na.value = "grey40", name = "CD", labels = c("No", "Yes","Missing"))+
  ylab("Proportion")+
  xlab("Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_grid(MoodPresentPlot, AnxPlot, ODDPlot, CDPlot, SexPlot, ADHDstatusPlot, align = "v", axis = "lr", ncol = 2)

#MEAN PLOTS
#####################################################################################

OutcomeMeans <- OutcomeData %>%
  #Must be dataframe only for melt function
  as.data.frame() %>%
  melt(., id = c("MERGEID", "Class")) %>%
  dcast(.,Class~variable,mean, na.rm=TRUE)

AgePlot <- ggplot(select(OutcomeMeans,Class, Y6_AGEYEARS),aes(Class,Y6_AGEYEARS))+
  geom_col(fill = pal[2])+
  scale_fill_manual(values = pal, name = "Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")+
  ylab("Age")+
  coord_cartesian(ylim = c(12.5,15.5))

IQPlot <- ggplot(select(OutcomeMeans,Class, Y6_WISCWIAT_FSIQ),aes(Class,Y6_WISCWIAT_FSIQ))+
  geom_col(fill=pal[2])+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")+
  ylab("Y6 WISC-WIAT IQ")+
  coord_cartesian(ylim = c(90,120))

ConHypPlot <- ggplot(select(OutcomeMeans,Class, Y6_P_C_CON3_HYP_TS),aes(Class,Y6_P_C_CON3_HYP_TS))+
  geom_col(aes(fill=as.factor(Class)))+
  scale_fill_manual(values = pal, name = "Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")+
  ylab("Conners Hyperactive t-score")+
  coord_cartesian(ylim = c(45,80))

ConIntPlot <- ggplot(select(OutcomeMeans,Class, Y6_P_C_CON3_INT_TS),aes(Class,Y6_P_C_CON3_INT_TS))+
  geom_col(aes(fill=as.factor(Class)))+
  scale_fill_manual(values = pal, name = "Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")+
  ylab("Conners Inattentive t-score")+
  coord_cartesian(ylim = c(45,80))

ADHDTotPlot <- ggplot(select(OutcomeMeans,Class, Y6_P_C_ADHDRS_TOT_TS),aes(Class,Y6_P_C_ADHDRS_TOT_TS))+
  geom_col(fill=pal[2])+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")+
  ylab("Y6 ADHD Rating Scale \nTotal t-score")+
  coord_cartesian(ylim = c(45,80))

SwanPlot <- ggplot(select(OutcomeMeans,Class, Y6_P_C_SWAN_TOT),aes(Class,Y6_P_C_SWAN_TOT))+
  geom_col(aes(fill=as.factor(Class)))+
  scale_fill_manual(values = pal, name = "Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")+
  ylab("SWAN Total Score")+
  coord_cartesian(ylim = c(45,80))

SDQPlot <- ggplot(select(OutcomeMeans,Class, Y6_P_C_SDQ_IM_TS),aes(Class,Y6_P_C_SDQ_IM_TS))+
  geom_col(aes(fill=as.factor(Class)))+
  scale_fill_manual(values = pal, name = "Class")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")+
  scale_y_continuous(breaks = c(0:2))+
  ylab("SDQ Impact t-score")+
  coord_cartesian(ylim = c(0,2))

plot_grid(ADHDTotPlot, IQPlot, AgePlot, align = "v", axis = "l", ncol = 2)

