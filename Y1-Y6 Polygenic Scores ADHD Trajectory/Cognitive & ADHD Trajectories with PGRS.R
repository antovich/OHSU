##
## CFA for Y1 variables, n of 1439
##

#Set working directory and load relevant packages
setwd("H:/Projects/Y1 Factor Scores for Mike n1400")
library(lavaan)
library(tidyverse)

datapath <- 'H:/Projects/Y1 Factor Scores for Mike n1400/Data/'

#Load datasets, indicate NA values
data1 <- read.csv(paste(datapath, 'all_data_adhd_ML_project_persistent_11-Sep-2018.csv', sep =""), 
                  na.strings = c("-999","NA",""))
data2 <- read.csv(paste(datapath,'Y1 KSAD INT_HYP_IRR_TMCQ 8-20-19.csv', sep =""),
                  na.strings = c("-999","NA",""))
data3 <- read.csv(paste(datapath,'Y1 Trails n_850.csv', sep =""),
                  na.strings = c("-999","NA",""))

# #Check which variable names are the same across datasets
# intersect(colnames(data1),colnames(data2))
# intersect(colnames(data1),colnames(data3))
# intersect(colnames(data2),colnames(data3))

# #Check whether subject numbers are overlapping
# anti_join(data1,data2, by = "MERGEID")$MERGEID
# anti_join(data2,data1, by = "MERGEID")$MERGEID
# anti_join(data3,data1, by = "MERGEID")$MERGEID

#Merge datasets (overlapping variables have .x or .y suffix for 1st and 2nd datasets respectively if not removed)
data <- left_join(data1, data2, by = "MERGEID")%>%
  #Join third dataset, removing overlapping variables
  left_join(., select(data3, -"FAMID",-"SEX",-"Y1_TRAILS_COND2", -"Y1_TRAILS_COND3"), by = "MERGEID")

#Optional, get z-scores for data and replace in dataframe (included residual variables although they appear to be standardized residuals)
dataScaled <- scale(select(data, Y1_WISCWIAT_FSIQ:Y1_TRAILS_RES))
dataScaled <- cbind(select(data, MERGEID:V2AGEYEARS), dataScaled)

#Unit-weighted scores for each factor
data$Arousal_UnitWeight <-  rowMeans(select(dataScaled, V_Y1, DPRIME1_Y1, DPRIME2_Y1), na.rm=TRUE) 

data$WM_UnitWeight <- rowMeans(select(dataScaled, Y1_DIGITS_BKWD_RS, SSBK_NUMCOMPLETE_Y1, Y1_DIGITS_FRWD_RS, 
                                      SSFD_NUMCOMPLETE_Y1, N1BACKACC_Y1, N2BACKACC_Y1), na.rm=TRUE)

data$Inhibition_UnitWeight <- rowMeans(select(dataScaled, CW_RES, Y1_TRAILS_RES, STOP_SSRTAVE_Y1), na.rm=TRUE) 

data$Speed_UnitWeight <- rowMeans(select(dataScaled, Y1_CLWRD_COND1, Y1_CLWRD_COND2, Y1_TRAILS_COND2, Y1_TRAILS_COND3), na.rm=TRUE)

data$Surgency_UnitWeight <- rowMeans(select(dataScaled, Y1_P_TMCQ_HIP, Y1_P_TMCQ_ACTIVITY, Y1_P_TMCQ_ASSERT), na.rm = TRUE)

data$SadnessAnxiety_UnitWeight <- rowMeans(select(dataScaled, Y1_P_TMCQ_FEAR, Y1_P_TMCQ_SAD, Y1_P_TMCQ_DISCOMF), na.rm=TRUE)

data$Irritability_UnitWeight <- rowMeans(select(dataScaled, Y1_P_TMCQ_ANGER, Y1_TMCQ_SOOTH_MOD, Y1_KSAD_ODD_IRR), na.rm=TRUE)

data$TeachReportADHDSx_UnitWeight <- rowMeans(select(dataScaled, Y1_T_ADHDRS_TOT_RS, Y1_T_C_CON3_INT_RS, Y1_T_C_CON3_HYP_RS, Y1_T_C_SDQ_HY_RS), na.rm=TRUE)

data$ParentReportADHDSx_UnitWeight <- rowMeans(select(dataScaled, Y1_P_C_ADHDRS_HYP_RS, Y1_KSAD_HYPSX, Y1_P_C_CON3_HYP_RS,
                                                      Y1_P_SWAN_HYP, Y1_P_C_SDQ_HY_RS, Y1_P_C_ADHDRS_INT_RS, Y1_P_C_CON3_INT_RS, Y1_P_SWAN_INT, Y1_KSAD_INTSX), na.rm=TRUE) 

#
# LAVAAN
#

#latent variable definition =~ is measured by
#regression ~ is regressed on
#(residual) (co)variance ~~ is correlated with
#intercept ~ 1 intercept

#By default:
#The factor loading of the first indicator of a latent variable is fixed to 1,
#residual variances are added automatically, and
#exogenous latent variables are correlated

#For this iteration, run as separate CFA models

#Latent variable specification

Arousal_Mod <- '
#Indicators
  Arousal =~ V_Y1 + DPRIME1_Y1 + DPRIME2_Y1 '

WM_Mod <- '
#Indicators
  WM =~ Y1_DIGITS_BKWD_RS + SSBK_NUMCOMPLETE_Y1 + Y1_DIGITS_FRWD_RS +
    SSFD_NUMCOMPLETE_Y1 + N1BACKACC_Y1 + N2BACKACC_Y1
#Covariance
    N1BACKACC_Y1 ~~ N2BACKACC_Y1
    Y1_DIGITS_BKWD_RS ~~ Y1_DIGITS_FRWD_RS '

Inhibit_Mod <- '
#Indicators
  Inhibition =~ CW_RES + Y1_TRAILS_RES + STOP_SSRTAVE_Y1 '

Speed_Mod <- '
#Indicators
  Speed =~ Y1_CLWRD_COND1 + Y1_CLWRD_COND2 + Y1_TRAILS_COND2 + Y1_TRAILS_COND3
#Covariance
    Y1_CLWRD_COND1 ~~ Y1_CLWRD_COND2 '

Surge_Mod <- '
#Indicators
  Surgency =~ Y1_P_TMCQ_HIP + Y1_P_TMCQ_ACTIVITY + Y1_P_TMCQ_ASSERT '

Sad_Mod <- '
#Indicators
  SadnessAnxiety =~ Y1_P_TMCQ_FEAR + Y1_P_TMCQ_SAD + Y1_P_TMCQ_DISCOMF '

Irrit_Mod <- '
#Indicators
  Irritability =~ Y1_P_TMCQ_ANGER + Y1_TMCQ_SOOTH_MOD + Y1_KSAD_ODD_IRR '

TSX_Mod <- '
#Indicators
  TeacherReportSymptoms =~ Y1_T_ADHDRS_TOT_RS + Y1_T_C_CON3_INT_RS + Y1_T_C_CON3_HYP_RS + 
    Y1_T_C_SDQ_HY_RS
#Covariance
    Y1_T_C_CON3_INT_RS ~~ Y1_T_C_CON3_HYP_RS'

PSX_Mod <- '
#Indicators
  ParentReportSymptoms =~ Y1_P_C_ADHDRS_HYP_RS + + Y1_P_C_ADHDRS_INT_RS + Y1_KSAD_HYPSX + + Y1_KSAD_INTSX + Y1_P_C_CON3_HYP_RS +
    Y1_P_C_CON3_INT_RS + Y1_P_SWAN_HYP + Y1_P_C_SDQ_HY_RS + Y1_P_SWAN_INT 
#Covariance
    Y1_P_C_CON3_HYP_RS ~~ Y1_P_C_SDQ_HY_RS
    Y1_P_C_ADHDRS_HYP_RS ~~ Y1_P_C_ADHDRS_INT_RS
    Y1_P_SWAN_HYP ~~ Y1_P_SWAN_INT
    Y1_KSAD_HYPSX ~~ Y1_KSAD_INTSX
    Y1_P_C_CON3_HYP_RS ~~ Y1_P_C_CON3_INT_RS
    Y1_P_SWAN_HYP ~~ Y1_KSAD_HYPSX
    Y1_KSAD_HYPSX ~~ Y1_P_C_ADHDRS_HYP_RS
    Y1_P_SWAN_HYP ~~ Y1_P_C_ADHDRS_HYP_RS
    Y1_P_C_CON3_HYP_RS ~~ Y1_P_C_ADHDRS_HYP_RS
    Y1_P_C_SDQ_HY_RS ~~ Y1_KSAD_HYPSX
    Y1_P_C_SDQ_HY_RS ~~ Y1_P_SWAN_HYP '

#Fit model and get model output, forcing STANDARDIZED latent variables
# and using FIML for missing data (same as Mplus)
Arousal_ModFit <- cfa(Arousal_Mod, data=data, missing="fiml", std.lv = TRUE)
WM_ModFit <- cfa(WM_Mod, data=data, missing="fiml", std.lv = TRUE)
Inhibit_ModFit <- cfa(Inhibit_Mod, data=data, missing="fiml", std.lv = TRUE)
Speed_ModFit <- cfa(Speed_Mod, data=data, missing="fiml", std.lv = TRUE)
Surge_ModFit <- cfa(Surge_Mod, data=data, missing="fiml", std.lv = TRUE)
Sad_ModFit <- cfa(Sad_Mod, data=data, missing="fiml", std.lv = TRUE)
Irrit_ModFit <- cfa(Irrit_Mod, data=data, missing="fiml", std.lv = TRUE)
TSX_ModFit <- cfa(TSX_Mod, data=data, missing="fiml", std.lv = TRUE)
PSX_ModFit <- cfa(PSX_Mod, data=data, missing="fiml", std.lv = TRUE)


# #Alternative: Fit model and get model output, with UNSTANDARDIZED latent variables 
# and using FIML for missing data (same as Mplus)
# US_Arousal_ModFit <- cfa(Arousal_Mod, data=data, missing="fiml")
# US_WM_ModFit <- cfa(WM_Mod, data=data, missing="fiml")
# US_Inhibit_ModFit <- cfa(Inhibit_Mod, data=data, missing="fiml")
# US_Speed_ModFit <- cfa(Speed_Mod, data=data, missing="fiml")
# US_Surge_ModFit <- cfa(Surge_Mod, data=data, missing="fiml")
# US_Sad_ModFit <- cfa(Sad_Mod, data=data, missing="fiml")
# US_Irrit_ModFit <- cfa(Irrit_Mod, data=data, missing="fiml")
# US_TSX_ModFit <- cfa(TSX_Mod, data=data, missing="fiml")
# US_PSX_ModFit <- cfa(PSX_Mod, data=data, missing="fiml")

#Get summary of fit
summary(TSX_ModFit, fit.measures=TRUE)

#Get factor scores (by default, estimates of latent variable using regression)
#Can adjust method = "regression", "Bartlett"
data$Arousal_FactorScores = lavPredict(Arousal_ModFit);
data$WM_FactorScores = lavPredict(WM_ModFit)
data$Inhibition_FactorScores = lavPredict(Inhibit_ModFit)
data$Speed_FactorScores = lavPredict(Speed_ModFit)
data$Surgency_FactorScores = lavPredict(Surge_ModFit)
data$SadnessAnxiety_FactorScores = lavPredict(Sad_ModFit)
data$Irritability_FactorScores = lavPredict(Irrit_ModFit)
data$TeacherReportADHDSx_FactorScores = lavPredict(TSX_ModFit)
data$ParentReportADHDSx_FactorScores = lavPredict(PSX_ModFit)

write.csv(data, paste(datapath,"Y1_Factor_Score_Data_",format(Sys.Date(),"%m_%d_%y"),".csv", sep=''), row.names = FALSE)
