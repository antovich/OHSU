#Script to get unit-weighted composites for OHSU and ABCD related to ADHD symptoms and EF 
#and to use those scores in correlation analysis in R and Mplus, including figures showing the bivariate correlation and fit line 

#Load relevant packages
library(lavaan)
library(tidyverse)
library(MplusAutomation)
library(cowplot)

############################################################################################################
#OHSU datasets
############################################################################################################

#Load datasets, indicate NA values
#Primary OHSU dataset
OHSU1 <- read.csv('H:/Projects/Y1 Factor Scores for Mike n1400/Data/all_data_adhd_ML_project_persistent_11-Sep-2018.csv', 
                  na.strings = c("-999","NA",""))
#Supplement to add additional questionnaire items
OHSU2 <- read.csv('H:/Projects/Y1 Factor Scores for Mike n1400/Data/Y1 KSAD INT_HYP_IRR_TMCQ 8-20-19.csv',
                  na.strings = c("-999","NA",""))
#Supplement to add additional cognitive items
OHSU3 <- read.csv('H:/Projects/Y1 Factor Scores for Mike n1400/Data/Y1 Trails n_850.csv',
                  na.strings = c("-999","NA",""))

#Merge datasets (overlapping variables have .x or .y suffix for 1st and 2nd datasets respectively if not removed)
OHSU <- left_join(OHSU1, OHSU2, by = "MERGEID")%>%
  #Join third dataset, removing overlapping variables
  left_join(., select(OHSU3, -"FAMID",-"SEX",-"Y1_TRAILS_COND2", -"Y1_TRAILS_COND3"), by = "MERGEID")%>%
  #Filter by age to match ABCD
  filter(V1AGEYEARS > 9 & V1AGEYEARS < 11)%>%
  ## OPTIONAL ## filter by ADHD status
  filter(DTEAM_DECISION == 3)%>%
  #Scale that variables that will be used for the unit-weighted composite; retains old vars and names new vars ending in _scale
  mutate_at(vars(
    #ADHD
    Y1_P_C_ADHDRS_HYP_RS, Y1_KSAD_HYPSX, Y1_P_C_CON3_HYP_RS,
    Y1_P_SWAN_HYP, Y1_P_C_SDQ_HY_RS, Y1_P_C_ADHDRS_INT_RS, 
    Y1_P_C_CON3_INT_RS, Y1_P_SWAN_INT, Y1_KSAD_INTSX,
    #ALLEF
    CW_RES, Y1_TRAILS_RES, STOP_SSRTAVE_Y1, 
    Y1_DIGITS_BKWD_RS, SSBK_NUMCOMPLETE_Y1, Y1_DIGITS_FRWD_RS, 
    SSFD_NUMCOMPLETE_Y1, N1BACKACC_Y1, N2BACKACC_Y1,
    Y1_CLWRD_COND1, Y1_CLWRD_COND2, Y1_TRAILS_COND2, Y1_TRAILS_COND3), list(scale=scale))

############################################################################################################
#ABCD Datasets
############################################################################################################

#Read in primary ABCD data, using master file from DCAN lab
ABCD1 <-  read.csv('H:/Projects/ABCD Correlations/Data/abcd_proj_masterfile.csv', 
                        na.strings = c("-999","NA",""),fileEncoding="UTF-8-BOM")

#Supplement to add REL family data for the larger ABCD dataset
ABCD2 <- read.table('H:/Projects/ABCD Correlations/Data/Family/acspsw03.txt', 
                     na.strings = c("-999","NA",""), header = TRUE)[-1,]

#Supplement to filtering variables from DCAN lab for the ABCD dataset
ABCD3 <- read.csv('H:/Projects/ABCD Correlations/Data/abcd_proj_splitgroups_ADHD_diags.csv', 
                    na.strings = c("-999","NA",""))

#Join ABCD datasets
ABCDcomb <- left_join(ABCD1, ABCD2, by = 'subjectkey')%>%
  left_join(ABCD3, by = 'subjectkey') %>%
  #If filtering, specify filter vars
  ## OPTIONAL ## filter out rule out cases
  filter(DCAN_ruleout_combined_filter == 1) %>%
  
  ## OPTIONAL ## filter by ADHD, looser KSAD status
  filter(ksads_14_853_p.y == 1)
  
  ## OPTIONAL ## #filter by ADHD, strict DCAN status
  filter(DCAN_ADHD_confirmed.y == 1)

#If taking a random subset
set.seed(1234) #set seed to get reproducible sample
#Bind together the entire ADHD sample (based on KSAD)
ABCDcomb <- rbind(filter(ABCDcomb,ksads_14_853_p.y == 1),
      #with a random sample of the control cases
      sample_n(filter(ABCDcomb, ksads_14_853_p.y == 0),
      #with sample size based on the proportion of Control cases in the OHSU sample
      round(nrow(filter(ABCDcomb,ksads_14_853_p.y == 1))/(nrow(filter(OHSU, DTEAM_DECISION == 3))/(nrow(filter(OHSU, DTEAM_DECISION == 1)))))))

#Get full dataset and create summary variables
ABCD <- ABCDcomb%>%
  #Create variables with symptom count sum for KSAD int and hyp
  mutate(KSADhyp = rowSums(select(., ksads_14_401_p, ksads_14_417_p,
                                  ksads_14_84_p, ksads_14_86_p,
                                  ksads_14_402_p, ksads_14_402_p,
                                  ksads_14_404_p, ksads_14_420_p,
                                  ksads_14_403_p, ksads_14_419_p,
                                  ksads_14_408_p, ksads_14_424_p,
                                  ksads_14_405_p, ksads_14_421_p,
                                  ksads_14_406_p, ksads_14_422_p,
                                  ksads_14_407_p, ksads_14_423_p), na.rm = TRUE),
         KSADint = rowSums(select(., ksads_14_394_p, ksads_14_410_p,
                                  ksads_14_76_p, ksads_14_78_p,
                                  ksads_14_395_p, ksads_14_411_p,
                                  ksads_14_396_p, ksads_14_412_p,
                                  ksads_14_397_p, ksads_14_413_p,
                                  ksads_14_76_p, ksads_14_78_p,
                                  ksads_14_399_p, ksads_14_415_p,
                                  ksads_14_80_p, ksads_14_82_p,
                                  ksads_14_400_p, ksads_14_416_p), na.rm = TRUE))%>%
  #Scale relevant variables
  mutate(KSADhypScaled =as.numeric(scale(KSADhyp)), 
         KSADintScaled = as.numeric(scale(KSADint)),
         CBCLScaled = as.numeric(scale(cbcl_scr_dsm5_adhd_r)),
         flankScaled = as.numeric(scale(nihtbx_flanker_fc)),
         cardScaled = as.numeric(scale(nihtbx_cardsort_fc)),
         patternScaled = as.numeric(scale(nihtbx_pattern_fc)))

############################################################################################################
#ABCD Composites
############################################################################################################

#Unit-weighted average ADHD compsite using scaled symptom count scores
ABCD$ADHDcomposite <- rowMeans(select(ABCD, KSADhypScaled, KSADintScaled, CBCLScaled))

#Unit-weighted average EF compsite (using fully corrected t scores)
#Scales were taken from Thompson et al. (2019) article on the neurocognitive battery ad problem behaviors in ABCD study
ABCD$EFcomposite <- rowMeans(select(ABCD, flankScaled, cardScaled, patternScaled))

#Total symptom count for KSAD and CBCL
ABCD$ADHDscomposite_sum <- rowSums(select(ABCD,KSADhyp, KSADint, cbcl_scr_dsm5_adhd_r))

############################################################################################################
#OHSU composites
############################################################################################################

#Get unit-weighted scores for each factor; composite structure was taken from published CFA (details from Hanna)

#Working memory
OHSU$WMcomposite <- rowMeans(select(OHSU, Y1_DIGITS_BKWD_RS_scale, SSBK_NUMCOMPLETE_Y1_scale, Y1_DIGITS_FRWD_RS_scale, 
                                      SSFD_NUMCOMPLETE_Y1_scale, N1BACKACC_Y1_scale, N2BACKACC_Y1_scale), na.rm=TRUE)

#Inhibitory control
OHSU$Inhibitcomposite <- rowMeans(select(OHSU, CW_RES_scale, Y1_TRAILS_RES_scale, STOP_SSRTAVE_Y1_scale), na.rm=TRUE) 

#Speed
OHSU$Speedcomposite <- rowMeans(select(OHSU, Y1_CLWRD_COND1_scale, Y1_CLWRD_COND2_scale, 
                                         Y1_TRAILS_COND2_scale, Y1_TRAILS_COND3_scale), na.rm=TRUE)

#Teacher report ADHD symptoms
OHSU$Teach_ADHDcomposite <- rowMeans(select(OHSU, Y1_T_ADHDRS_TOT_RS_scale, Y1_T_C_CON3_INT_RS_scale, 
                                                     Y1_T_C_CON3_HYP_RS_scale, Y1_T_C_SDQ_HY_RS_scale), na.rm=TRUE)
#Parent report ADHD symptoms
OHSU$ADHDcomposite <- rowMeans(select(OHSU, Y1_P_C_ADHDRS_HYP_RS_scale, Y1_KSAD_HYPSX_scale, 
                                                                   Y1_P_C_CON3_HYP_RS_scale,Y1_P_SWAN_HYP_scale, Y1_P_C_SDQ_HY_RS_scale,
                                                                   Y1_P_C_ADHDRS_INT_RS_scale, Y1_P_C_CON3_INT_RS_scale, 
                                                                   Y1_P_SWAN_INT_scale, Y1_KSAD_INTSX_scale), na.rm=TRUE) 

#Executive function composite using just WM and inhibitory control
OHSU$EFcomposite_orig <- rowMeans(select(OHSU, CW_RES_scale, Y1_TRAILS_RES_scale, STOP_SSRTAVE_Y1_scale, 
                                                   Y1_DIGITS_BKWD_RS_scale, SSBK_NUMCOMPLETE_Y1_scale, Y1_DIGITS_FRWD_RS_scale, 
                                                   SSFD_NUMCOMPLETE_Y1_scale, N1BACKACC_Y1_scale, N2BACKACC_Y1_scale), na.rm=TRUE) 

#Executive function composite using speed, WM, and inhibitory control
OHSU$EFcompsite_expand <- rowMeans(select(OHSU, CW_RES_scale, Y1_TRAILS_RES_scale, STOP_SSRTAVE_Y1_scale, 
                                           Y1_DIGITS_BKWD_RS_scale, SSBK_NUMCOMPLETE_Y1_scale, Y1_DIGITS_FRWD_RS_scale, 
                                           SSFD_NUMCOMPLETE_Y1_scale, N1BACKACC_Y1_scale, N2BACKACC_Y1_scale,
                                           Y1_CLWRD_COND1_scale, Y1_CLWRD_COND2_scale, Y1_TRAILS_COND2_scale, Y1_TRAILS_COND3_scale), 
                                      na.rm=TRUE)

#Untransformed composite, just a sum of all ADHD symptoms from all measures in the composite above
OHSU$ADHDcomposite_sum <- rowSums(select(OHSU, Y1_P_C_ADHDRS_HYP_RS, Y1_KSAD_HYPSX, Y1_P_C_CON3_HYP_RS,
                                                                   Y1_P_SWAN_HYP, Y1_P_C_SDQ_HY_RS, Y1_P_C_ADHDRS_INT_RS, Y1_P_C_CON3_INT_RS, Y1_P_SWAN_INT, Y1_KSAD_INTSX), na.rm=TRUE) 

############################################################################################################
#Basic correlations (for prelim analysis)
############################################################################################################

#Test the basic correlation between EF and ADHD composites without considering clustering vars or missing data method
OHSUcor <- cor.test(OHSU$EFcomposite_orig, OHSU$ADHDcomposite, alternative = "two.sided", method = "pearson")
ABCDcor <- cor.test(ABCD$EFcomposite, ABCD$ADHDcomposite, alternative = "two.sided", method = "spearman")

############################################################################################################
#Save composite vars
############################################################################################################

write.csv(select(OHSU, MERGEID, FAMID, SEX, V1AGEYEARS, DTEAM_DECISION, ADHDcomposite),row.names = FALSE, 
          'H:/Projects/ABCD Correlations/Figures & Output/ADHD_Composite_OHSU.csv')

write.csv(select(ABCD, subjectkey, site_id_l, site_name, interview_age.x, rel_family_id, gender, DCAN_ADHD_confirmed, ADHDcomposite),row.names = FALSE, 
          'H:/Projects/ABCD Correlations/Figures & Output/ADHD_Composite_ABCD.csv')


############################################################################################################
#Plotting
############################################################################################################

#Study location for plottin OHSU or ABCD
StudyLoc <- 'ABCD'

#Scatterplot with simple fit line for overall ADHD/EF relationship
ggplot(get(StudyLoc), aes(x = EFcomposite, y = ADHDcomposite)) +
  geom_point(shape=19, size = 2, na.rm = TRUE)+
  geom_smooth(method='lm', formula= y~x, size = 1)+
  theme_minimal(base_size = 14)+ #Font size
  xlab("EF Composite")+
  ylab("ADHD Symptom Composite")+
  ggtitle(StudyLoc)+
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Save correlation plot
ggsave(paste0('H:/Projects/ABCD Correlations/Figures & Output/',StudyLoc,' simple.jpeg'), dpi = 600, width = 6, height = 6)

#Create separate plots for each ABCD site
#Preallocate plotlist and correlation var
plotList <- list()
r <- NULL
#For loop over the cites
for(site in 1:21){
  #Get correlation coefficient and p-value for annotating plot
r <- cor.test(filter(ABCD, site_id_l.x == site)$EFcomposite, filter(ABCD, site_id_l.x == site)$ADHDcomposite, alternative = "two.sided", method = "pearson")
p <- ggplot(filter(ABCD, site_id_l.x == site), aes(x = EFcomposite, y = ADHDcomposite)) +
  geom_point(shape=19, size = 2, na.rm = TRUE)+ #Scatterplot
  geom_smooth(method='lm', formula= y~x, size = 1)+ #Simple line of best fit
  theme_minimal(base_size = 14)+ #Font size
  annotate("text", x = 0, y = -1.5, label = paste0('r = ',round(r$estimate, 2),', p = ',round(r$p.value,3)))+ #add r and p-value to plot
  xlab("")+ #Remove x and y labels, which will instead be added to the plot grid below
  ylab("")+
  xlim(-1.75, 1.75)+
  ylim(-1.75, 1.75)+
  ggtitle(filter(ABCD, site_id_l.x == site)$site_name.x[[1]])+ #Get title from list of site names
  theme(plot.title = element_text(hjust = 0.5, size = 15))
#save current plot to the plot list
plotList[[site]] <- p
}

#Plot all the site plots in a single plotgrid
plot_grid(plotlist = plotList[1:21], ncol = 7)+
  #Add axis labels to the plot grid (instead of individual plots)
  draw_label("Executive Function Composite", x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("ADHD Composite", x=  0, y=0.5, vjust= 1.5, angle=90)

#Save plot grid
ggsave('H:/Projects/ABCD Correlations/Figures & Output/ABCD sites.jpeg', dpi = 600, width = 17, height = 12)


#Plot kernal density for two ADHD composite versions in ABCD sample
plot1 <- ggplot(ABCD, aes(x = ADHDcomposite)) + geom_density(alpha = 0.5, fill = 'blue')+
  theme_bw()
plot2 <- ggplot(ABCD, aes(x = ADHDcomposite_sum)) + geom_density(alpha = 0.5, fill = 'red')+
  theme_bw()
plot_grid(plot1, plot2)


############################################################################################################
#MPlus
############################################################################################################

#specify site, name for dataset, location of files, and the EF measure to be used

StudySite <- 'ABCD'
dataFile <- 'OHSUdata_TEST.dat'
outputFile <- 'Overall ADHD Only TEST.inp'
FileLoc <- 'H:/Projects/ABCD Correlations/Mplus Scripts/OHSU/'
CurEFMeasure <- 'EFcomposite'
CurADHDMeasure <- 'ADHDcomposite'

#Use MplusAutomation to prepare dataset, using specifications above
prepareMplusData(select(get(StudySite), ifelse(StudySite == 'OHSU', 'FAMID', ifelse(StudySite == 'ABCD', 'rel_family_id', 'NA')), CurADHDMeasure, CurEFMeasure), paste0(FileLoc, dataFile))

#Create Mplus script to find Pearson's correlation (using WITH and standardized XY to get standardized covariance)
#Missing data handled with FIML, family variable used for clustering (TYPE = COMPLEX), though note that this doesn't affect the estimates (only adjust SEs)
writeLines(paste0(
  'TITLE: Your title goes here
  DATA: FILE = "',FileLoc,'
  ',dataFile,'";
  VARIABLE: 
  NAMES = FAM ',CurADHDMeasure,' ',CurEFMeasure,';

  CLUSTER = FAM;

  MISSING=.;

  ANALYSIS:
  TYPE = COMPLEX;

  MODEL: ',CurADHDMeasure,' WITH ',CurEFMeasure,';

  OUTPUT:
  STANDARDIZED(STDYX) CINTERVAL ;'), paste0(FileLoc,outputFile))

#Run all models in the Mplus location specified above
runModels(FileLoc)

#Set of individual EF measures to loop over
EF_measure <- c('flankScaled', 'cardScaled', 'patternScaled')
SitesFileLoc <- 'H:/Projects/ABCD Correlations/Mplus Scripts/ABCD/Individual EF Measures'


#For loop over the individual EF measures listed above
for(measure in EF_measure) {
  #For loop over each of the sites
for(site in 1:21){
  #Prepare an Mplus dataset for each site/measure combination
  prepareMplusData(select(filter(ABCD,site_id_l == site), rel_family_id, ADHDcomposite, measure), paste(SitesFileLoc,'/Site',site,measure,'Data.dat'))
  #Create a separate Mplus input file for each site/measure combination
  writeLines(paste0(
  'TITLE: Your title goes here
  DATA: FILE = "',SitesFileLoc,'
  /Site ',site,' ',measure,' Data.dat";
  VARIABLE: 
  NAMES = FAM ADHD ',measure,';

  CLUSTER = FAM;

  MISSING=.;

  ANALYSIS:
  TYPE = COMPLEX;

  MODEL: ADHD WITH ',measure,';

  OUTPUT:
  STANDARDIZED(STDYX) CINTERVAL ;'), paste0(SitesFileLoc,"/Site",site,measure,"Input.inp"))
  }
}
#Run all models in the location specified below
runModels(paste0(SitesFileLoc,"/"))

#Read-in Mplus model output
MplusOutput <- readModels(SitesFileLoc)

#Preallocate vars that will be read from the Mplus output in the loop below
rVals <- NULL
subNum <- NULL
siteNum <- NULL
measName <- NULL
#Counting var (bad habit...)
a = 1

#For loop over each site and measure to extract Mplus output for r, subject numbers, site name, and measurename
for(site in 1:21){
  for (measure in EF_measure) {
    #Extract data from within lists of lists in Mplus output
rVals[a] <- MplusOutput[[paste("site",site,tolower(measure),"input.out", sep = ".")]][['parameters']][['stdyx.standardized']]$est[1]
subNum[a] <- MplusOutput[[paste("site",site,tolower(measure),"input.out", sep = ".")]][['summaries']]$Observations
siteNum[a] <- site
measName[a] <- measure
#Move counting variable up (bad habit...)
a = a+1
  }
}

#Collated data extracted from Mplus above into a dataframe
CorrelationData <- data.frame(SiteNum = siteNum, EFMeasure = measName, r = rVals, N = subNum)

#Save correlation dataframe to the clipboard for pasting into Excel
write.table(CorrelationData, file = "clipboard",sep="\t", row.names=FALSE)
