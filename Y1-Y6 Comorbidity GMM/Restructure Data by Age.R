library(tidyverse)
library(reshape2)

#Load data and get rounded age variables (each group will be centered around the beginning of the year, e.g., 6.50-7.49 for age 7)
DataByAge <- read.csv('H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Data/Y1-Y6 enhanced Table 1 9-12-19.csv', na.strings = c("",-999,-888, NA),fileEncoding="UTF-8-BOM")%>%
  mutate(
  AgeY1 = round(V1AGEYEARS),
  AgeY2 = round(Y2AGEYEARS),
  AgeY3 = round(Y3AGEYEARS),
  AgeY4 = round(Y4AGEYEARS),
  AgeY5 = round(Y5AGEYEARS),
  AgeY6 = round(Y6AGEYEARS))

#Get info about how different the rounded age value is from the original age value for later comparison
DataByAgeCheck1 <- cbind(DataByAge$AgeY1-DataByAge$V1AGEYEARS,
                         DataByAge$AgeY2-DataByAge$Y2AGEYEARS,
                         DataByAge$AgeY3-DataByAge$Y3AGEYEARS,
                         DataByAge$AgeY4-DataByAge$Y4AGEYEARS,
                         DataByAge$AgeY5-DataByAge$Y5AGEYEARS,
                         DataByAge$AgeY6-DataByAge$Y6AGEYEARS)

#Plot histogram of observations by age for original ages (for later comparison)
OriginalYears <- reshape2::melt(DataByAge, id.vars = c("MERGEID"), measure.vars = c("V1AGEYEARS","Y2AGEYEARS","Y3AGEYEARS","Y4AGEYEARS","Y5AGEYEARS","Y6AGEYEARS")) 
Original <- ggplot(OriginalYears, aes(x = value)) +
  geom_histogram(color="#0c2c84", fill="#1d91c0", bins = 13, binwidth = 1, boundary = 6.999)+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank())+
  scale_y_continuous(breaks = seq(0,600, by = 50))+
  xlab("Age Bin (Original)")+
  ylab("Count")+
  scale_x_continuous(breaks = 6:19)

#Plot histogram of observations by age for rounded ages (for later comparison)
RangeCheck1 <- melt(DataByAge, id.vars = c("MERGEID"), measure.vars = c("AgeY1","AgeY2","AgeY3","AgeY4","AgeY5","AgeY6"))
Rounded <- ggplot(RangeCheck1, aes(x = value)) +
  geom_histogram(color="#0c2c84", fill="#1d91c0", bins = 13, binwidth = 1, boundary = 6.999)+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank())+
  scale_y_continuous(breaks = seq(0,600, by = 50))+
  xlab("Age Bin (Rounded)")+
  ylab("Count")+
  scale_x_continuous(breaks = 7:19)

#Plot histogram of difference between original and rounded ages (for later comparison)
DifferenceCheck1 <- reshape2::melt(as.data.frame(DataByAgeCheck1), measure.vars = c("V1","V2","V3","V4","V5","V6"))
DiffCheck1 <- ggplot(DifferenceCheck1, aes(x = value)) +
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 0.125, boundary = 0)+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank())+
  scale_y_continuous(breaks = seq(0,400, by = 50))+
  scale_x_continuous(limits = c(-1,1))+
  xlab("Difference in Age (Rounded - Original)")+
  ylab("Count")

#load data.table package
library(data.table)

#For iteratively removing duplicates, run from here down to next bar #
##############################################################################################

#Check for duplicates after rounding (i.e., kids who were seen for different waves within a year)
Duplicates <- filter(DataByAge, AgeY1 == AgeY2 |
                                AgeY1 == AgeY3 |
                                AgeY1 == AgeY4 |
                                AgeY1 == AgeY5 |
                                AgeY1 == AgeY6 |
                        
                                AgeY2 == AgeY3 |
                                AgeY2 == AgeY4 |
                                AgeY2 == AgeY5 |
                                AgeY2 == AgeY6 |
                         
                                AgeY3 == AgeY4 |
                                AgeY3 == AgeY5 |
                                AgeY3 == AgeY6 |    
                        
                                AgeY4 == AgeY5 |
                                AgeY4 == AgeY6 | 
                        
                                AgeY5 == AgeY6)%>%
  select(MERGEID, AgeY1:AgeY6, V1AGEYEARS:Y6AGEYEARS)

#Script to automatically adjust the rounded age value to avoide duplicates 
#The script starts with the older waves and moves down to the younger waves
#For each pair of ages in the duplicate, it adjusts the rounded age value that is furthest from the actual age value
#For each pair, if the upper year is furthest, the year is adjusted up a year
#Same but downward if lower year is furthest
#If adjusting the furthest of the age pair causes a new duplicate, it adjust the other age value
#Note: This does not check for possible duplicates in the final step, so it may need to be run iteratively several times

#Preallocate new variable
Duplicates_Corrected <- Duplicates

#Loop trough each row of the duplicates
for (i in 1:nrow(Duplicates)) {
  #If duplicate between these years (and not NA)
  if (Duplicates$AgeY5[i] %in% Duplicates$AgeY6[i] && !is.na(Duplicates$AgeY5[i])) {
    #Test which is furthest from the its rounded age assignement
    a <- which.max(c(abs(Duplicates$AgeY5[i]-Duplicates$Y5AGEYEARS[i]),abs(Duplicates$AgeY6[i]-Duplicates$Y6AGEYEARS[i])))
    #If the lower year is further, move the lower year down 1, unless that duplicates another year
        if (a == 1) {if((Duplicates$AgeY5[i]-1) %in% c(Duplicates$AgeY1[i], Duplicates$AgeY2[i], Duplicates$AgeY3[i], Duplicates$AgeY4[i])) 
                      {Duplicates_Corrected$AgeY6[i] = (Duplicates$AgeY6[i]+1)
                      } else {Duplicates_Corrected$AgeY5[i] = (Duplicates$AgeY5[i]-1)}
    #If the upper year is further, move the uper year up 1, unless that duplicates another year
      } else if (a == 2) {if((Duplicates$AgeY6[i]+1) %in% c(Duplicates$AgeY1[i],Duplicates$AgeY2[i],Duplicates$AgeY3[i],Duplicates$AgeY4[i])) 
                      {Duplicates_Corrected$AgeY5[i] = (Duplicates$AgeY5[i]-1) 
                      } else {Duplicates_Corrected$AgeY6[i] = (Duplicates$AgeY6[i]+1)}
                         }
    } #repeat for Y4 & Y5
  if (Duplicates$AgeY4[i] %in% Duplicates$AgeY5[i] && !is.na(Duplicates$AgeY4[i])) {
      a <- which.max(c(abs(Duplicates$AgeY4[i]-Duplicates$Y4AGEYEARS[i]),abs(Duplicates$AgeY5[i]-Duplicates$Y5AGEYEARS[i])))
      if (a == 1) {if((Duplicates$AgeY4[i]-1) %in% c(Duplicates$AgeY1[i],Duplicates$AgeY2[i],Duplicates$AgeY3[i],Duplicates$AgeY6[i])) 
                    {Duplicates_Corrected$AgeY5[i] = (Duplicates$AgeY5[i]+1)
                    } else {Duplicates_Corrected$AgeY4[i] = (Duplicates$AgeY4[i]-1)}
   } else if (a == 2) {if((Duplicates$AgeY5[i]+1) %in% c(Duplicates$AgeY1[i],Duplicates$AgeY2[i],Duplicates$AgeY3[i],Duplicates$AgeY6[i])) 
                    {Duplicates_Corrected$AgeY4[i] = (Duplicates$AgeY4[i]-1) 
                    } else {Duplicates_Corrected$AgeY5[i] = (Duplicates$AgeY5[i]+1)}
                      }
  } #repeat for Y3 & Y4
  if (Duplicates$AgeY3[i] %in% Duplicates$AgeY4[i] && !is.na(Duplicates$AgeY3[i])) {
    a <- which.max(c(abs(Duplicates$AgeY3[i]-Duplicates$Y3AGEYEARS[i]),abs(Duplicates$AgeY4[i]-Duplicates$Y4AGEYEARS[i])))
    if (a == 1) {if((Duplicates$AgeY3[i]-1) %in% c(Duplicates$AgeY1[i],Duplicates$AgeY2[i],Duplicates$AgeY5[i],Duplicates$AgeY6[i])) 
                    {Duplicates_Corrected$AgeY4[i] = (Duplicates$AgeY4[i]+1)
                    } else {Duplicates_Corrected$AgeY3[i] = (Duplicates$AgeY3[i]-1)}
    } else if (a == 2) {if((Duplicates$AgeY4[i]+1) %in% c(Duplicates$AgeY1[i],Duplicates$AgeY2[i],Duplicates$AgeY5[i],Duplicates$AgeY6[i])) 
                    {Duplicates_Corrected$AgeY3[i] = (Duplicates$AgeY3[i]-1) 
                    } else {Duplicates_Corrected$AgeY4[i] = (Duplicates$AgeY4[i]+1)}
                        }
  } #repeat for Y2 & Y3
  if (Duplicates$AgeY2[i] %in% Duplicates$AgeY3[i] && !is.na(Duplicates$AgeY2[i])) {
    a <- which.max(c(abs(Duplicates$AgeY2[i]-Duplicates$Y2AGEYEARS[i]),abs(Duplicates$AgeY3[i]-Duplicates$Y3AGEYEARS[i])))
    if (a == 1) {if((Duplicates$AgeY2[i]-1) %in% c(Duplicates$AgeY1[i],Duplicates$AgeY4[i],Duplicates$AgeY5[i],Duplicates$AgeY6[i])) 
                {Duplicates_Corrected$AgeY3[i] = (Duplicates$AgeY3[i]+1)
                } else {Duplicates_Corrected$AgeY2[i] = (Duplicates$AgeY2[i]-1)}
    } else if (a == 2) {if((Duplicates$AgeY3[i]+1) %in% c(Duplicates$AgeY1[i],Duplicates$AgeY4[i],Duplicates$AgeY5[i],Duplicates$AgeY6[i])) 
                {Duplicates_Corrected$AgeY2[i] = (Duplicates$AgeY2[i]-1) 
                } else {Duplicates_Corrected$AgeY3[i] = (Duplicates$AgeY3[i]+1)}
                        }
  } #repeat for Y1 & Y2
  if (Duplicates$AgeY1[i] %in% Duplicates$AgeY2[i] && !is.na(Duplicates$AgeY1[i])) {
    a <- which.max(c(abs(Duplicates$AgeY1[i]-Duplicates$Y1AGEYEARS[i]),abs(Duplicates$AgeY2[i]-Duplicates$Y2AGEYEARS[i])))
    if (a == 1) {if((Duplicates$AgeY1[i]-1) %in% c(Duplicates$AgeY3[i],Duplicates$AgeY4[i],Duplicates$AgeY5[i],Duplicates$AgeY6[i])) 
                {Duplicates_Corrected$AgeY2[i] = (Duplicates$AgeY2[i]+1)
                } else {Duplicates_Corrected$AgeY1[i] = (Duplicates$AgeY1[i]-1)}
    } else if (a == 2) {if((Duplicates$AgeY2[i]+1) %in% c(Duplicates$AgeY3[i],Duplicates$AgeY4[i],Duplicates$AgeY5[i],Duplicates$AgeY6[i])) 
                {Duplicates_Corrected$AgeY1[i] = (Duplicates$AgeY1[i]-1) 
                } else {Duplicates_Corrected$AgeY2[i] = (Duplicates$AgeY2[i]+1)}
                        }
  }
 }

#Add revised age to main dataset

#Temporarily convert to data.table for manipulation
setDT(DataByAge)
setDT(Duplicates_Corrected)

#Replace main dataset (DataByAge) with new corrected values (Duplicates_Corrected) using MERGEID
DataByAge[Duplicates_Corrected, on  = "MERGEID", c("AgeY1","AgeY2","AgeY3","AgeY4","AgeY5","AgeY6") := .(i.AgeY1, i.AgeY2, i.AgeY3, i.AgeY4, i.AgeY5, i.AgeY6)]

#Recheck for duplicates after correction
Duplicates2 <- filter(DataByAge, AgeY1 == AgeY2 |
                       AgeY1 == AgeY3 |
                       AgeY1 == AgeY4 |
                       AgeY1 == AgeY5 |
                       AgeY1 == AgeY6 |
                       
                       AgeY2 == AgeY3 |
                       AgeY2 == AgeY4 |
                       AgeY2 == AgeY5 |
                       AgeY2 == AgeY6 |
                       
                       AgeY3 == AgeY4 |
                       AgeY3 == AgeY5 |
                       AgeY3 == AgeY6 |    
                       
                       AgeY4 == AgeY5 |
                       AgeY4 == AgeY6 | 
                       
                       AgeY5 == AgeY6)%>%
  select(MERGEID, AgeY1:AgeY6, V1AGEYEARS:Y6AGEYEARS)



# For iteratively removing duplicates, run down to here.
# Ran this section of the script iteratively 3 times until there were no longer any duplicates
##############################################################################################

DataByAge = as.data.frame(DataByAge)


#Make sure age values weren't moved too much by the duplicate adjustment

#Get difference between corrected rounded age and original age

DataByAgeCheck2 <- cbind(DataByAge$AgeY1-DataByAge$V1AGEYEARS,
                        DataByAge$AgeY2-DataByAge$Y2AGEYEARS,
                        DataByAge$AgeY3-DataByAge$Y3AGEYEARS,
                        DataByAge$AgeY4-DataByAge$Y4AGEYEARS,
                        DataByAge$AgeY5-DataByAge$Y5AGEYEARS,
                        DataByAge$AgeY6-DataByAge$Y6AGEYEARS)

#Look at the number of kids this duplication correction affected
DataByAgeCheck2 <- as.data.frame(DataByAgeCheck2)
filter(DataByAgeCheck2, abs(DataByAgeCheck2[1]) > 0.5 | abs(DataByAgeCheck2[2]) > 0.5 | 
         abs(DataByAgeCheck2[3]) > 0.5 | abs(DataByAgeCheck2[4]) > 0.5 | abs(DataByAgeCheck2[5]) > 0.5 | abs(DataByAgeCheck2[6]) > 0.5)

#Spread from original after simple rounding
min(DataByAgeCheck1, na.rm = TRUE)
max(DataByAgeCheck1, na.rm = TRUE)
mean(DataByAgeCheck1, na.rm = TRUE)
sd(DataByAgeCheck1, na.rm = TRUE)

#Spread from original after corrected rounding (iteratively removing duplicates)
min(DataByAgeCheck2, na.rm = TRUE)
max(DataByAge$AgeY1,DataByAge$AgeY2,DataByAge$AgeY3,DataByAge$AgeY4,DataByAge$AgeY5,DataByAge$AgeY6, na.rm = TRUE)
mean(DataByAgeCheck2, na.rm = TRUE)
sd(DataByAgeCheck2, na.rm = TRUE)


#Plot histogram of difference between original ages and corrected rounded ages (note, this includes count for each age value, not each kid)
DifferenceCheck2 <- reshape2::melt(as.data.frame(DataByAgeCheck2), measure.vars = c("V1","V2","V3","V4","V5","V6"))
DiffCheck2 <- ggplot(DifferenceCheck2, aes(x = value)) +
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 0.125, boundary = 0)+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank())+
  scale_y_continuous(breaks = seq(0,400, by = 50))+
  xlab("Difference in Age (Corrected - Original)")+
  ylab("Count")

#Plot histogram of observations by corrected rounded age
RangeCheck2 <- reshape2::melt(DataByAge, id.vars = c("MERGEID"), measure.vars = c("AgeY1","AgeY2","AgeY3","AgeY4","AgeY5","AgeY6"))
Corrected <- ggplot(RangeCheck2, aes(x = value)) +
  geom_histogram(color="#0c2c84", fill="#1d91c0", bins = 13, binwidth = 1, boundary = 6.999)+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank())+
  scale_y_continuous(breaks = seq(0,600, by = 50))+
  xlab("Age Bin (Corrected)")+
  ylab("Count")+
  scale_x_continuous(breaks = 7:19)

library(cowplot)

plot_grid(Original, Rounded, Corrected, nrow=1)
plot_grid(DiffCheck1,DiffCheck2, nrow = 1)

#Looks like there are a few values above abs(0.5), 
#which was the range for the original rounding, but nothing too extreme

#Align data by age using the corrected age bins
##########################################################################################################

#List of ages, years, and varibles to loop over
AgeList = 7:19
YearList = 1:6
VarList = c("_DTEAM_MOOD", "_DTEAM_ANX", "_DTEAM_ODD", "_DTEAM_CD")
row = 1 

#Preallocate matrix to look at sparseness
Sparse <- as.data.frame(matrix(ncol = 4, nrow = length(AgeList)*length(YearList)*length(VarList)))
colnames(Sparse) <- c("Age", "Measure", "Count", "NA Count")

#Loop through all possible ages in the corrected rounded dataset
for(Age in AgeList) {
  #Loop through each study year that will be assessed
  for(Year in YearList) {
    #Loop through each of the values that will be restructured by age
    for(Var in VarList){
      
      #Build variables for the current loop iteration
      AgeAtYear <- paste("AgeY", Year, sep="")
      FillVariable <- paste("Y", Year,Var, sep="")
      NewVariable <- paste("Age", Age, Var, sep="")

      #Create new variable with age and measure that will collect data from the fill variable 
      #at rows in which the childs age matches the current age in the loop
      DataByAge[[NewVariable]][DataByAge[[AgeAtYear]] %in% Age] = DataByAge[[FillVariable]][DataByAge[[AgeAtYear]] %in% Age]
      
      Sparse[row,1] = Age
      Sparse[row,2] = Var
      Sparse[row,3] = sum(!is.na(DataByAge[[NewVariable]]))
      Sparse[row,4] = sum(is.na(DataByAge[[NewVariable]]))
      
      row = row+1
    }
  }
}

library(reshape2)

#Look at sparseness by age

#Summarize sparseness dataframe to show count by age

Sparse <- melt(Sparse, id.vars = c("Age", "Measure"), measure.vars = c("Count", "NA Count"), value.name = "Count")
Sparse <- dcast(Sparse, Age + variable ~ ., mean, value.var = "Count")
colnames(Sparse) = c("Age", "Type", "Count")
Sparse$Type <- relevel(Sparse$Type, "NA Count")

#Plot sparseness
ggplot(Sparse, aes(x = Age, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank())+
  scale_x_continuous(breaks  = AgeList)+
  scale_y_continuous(breaks = seq(0,850, by = 50), limits = c(0,850))+
  scale_fill_discrete(labels = c("Missing", "Present"), name = "Status")

#Get a list of categories within each variable
melt(rapply(select(DataByAge, Age7_DTEAM_MOOD:Age19_DTEAM_CD),unique, how = "list"))

#Get counts of categories by age
Absent <- melt(rapply(select(DataByAge, Age7_DTEAM_MOOD:Age19_DTEAM_CD), function(x) sum(x==1,na.rm = TRUE), how = "list"))
Subthresh <- melt(rapply(select(DataByAge, Age7_DTEAM_MOOD:Age19_DTEAM_CD), function(x) sum(x==2,na.rm = TRUE), how = "list"))
Present <- melt(rapply(select(DataByAge, Age7_DTEAM_MOOD:Age19_DTEAM_CD), function(x) sum(x==3,na.rm = TRUE), how = "list"))
Ruleout <- melt(rapply(select(DataByAge, Age7_DTEAM_MOOD:Age19_DTEAM_CD), function(x) sum(x==5,na.rm = TRUE), how = "list"))
#Get dataframe with all counts by category, create an age column using rep, create a variable column by parsing variable string using str_remove
CatCount <- bind_rows(Absent, Subthresh, Present, Ruleout, .id = "source") %>%
  mutate(Age = rep(rep(7:19, each = 4),4)) %>%
  mutate(L1 = str_remove(L1, "Age\\d+_DTEAM_")) %>%
  rename(Category = "source", Variable = "L1")


#Plot category counts by variable and age
plot_list = list()
for(i in c("MOOD", "ANX", "ODD", "CD")) {
  p = ggplot(filter(CatCount, Variable == i), aes(x = Age, y = value, fill = Category)) +
          geom_bar(stat = "identity", width = 0.8) +
          theme_bw()+
          theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))+
          xlab("Age")+
          ylab("Count")+
          scale_fill_manual(name = "Category", labels = c("Absent", "Subthreshold", "Present", "Rule Out"), values = c("#00AFBB", "#E7B800", "#FC4E07", "grey23") )+
          scale_y_continuous(breaks = seq(0, 400, by = 25))+
          scale_x_continuous(breaks = 7:19)
  plot_list[[i]] = p
}

plot_grid(plot_list$MOOD+ggtitle("Mood"), plot_list$ANX+ggtitle("Anxiety"), plot_list$ODD+ggtitle("ODD"), plot_list$CD+ggtitle("CD"), nrow = 2)

#Look at missing data
library(naniar)

#Get missing vs present by age
MissingData <- select(DataByAge, Age7_DTEAM_MOOD, Age8_DTEAM_MOOD, Age9_DTEAM_MOOD, Age10_DTEAM_MOOD,
       Age11_DTEAM_MOOD, Age12_DTEAM_MOOD, Age13_DTEAM_MOOD, Age14_DTEAM_MOOD, Age15_DTEAM_MOOD,
       Age16_DTEAM_MOOD, Age17_DTEAM_MOOD, Age18_DTEAM_MOOD, Age19_DTEAM_MOOD) %>%
  rename("Age 7" = Age7_DTEAM_MOOD, "Age 8" = Age8_DTEAM_MOOD, "Age 9" = Age9_DTEAM_MOOD, "Age 10" = Age10_DTEAM_MOOD,
         "Age 11" = Age11_DTEAM_MOOD, "Age 12" = Age12_DTEAM_MOOD, "Age 13" = Age13_DTEAM_MOOD, "Age 14" = Age14_DTEAM_MOOD, "Age 15" = Age15_DTEAM_MOOD,
         "Age 16" = Age16_DTEAM_MOOD, "Age 17" = Age17_DTEAM_MOOD, "Age 18" = Age18_DTEAM_MOOD, "Age 19" = Age19_DTEAM_MOOD)

#Plot patterns of missingness by age
GGMiss <- gg_miss_upset(MissingData, sets = c("Age 19_NA","Age 18_NA","Age 17_NA","Age 16_NA","Age 15_NA","Age 14_NA",
                                    "Age 13_NA","Age 12_NA","Age 11_NA","Age 10_NA","Age 9_NA","Age 8_NA","Age 7_NA"), 
              nsets = 13, nintersects = 40, keep.order = TRUE, 
              mainbar.y.label = "Count of Missing by Pattern", sets.x.label = "Count of Missing by Age", 
              main.bar.color = "#008bd1", matrix.color = "gray23", sets.bar.color ="#008bd1",
              point.size = 4, line.size = 1, text.scale = 1.5)

#Plot missingness by subject and age
VisMiss <- vis_miss(MissingData) + ylab("Subject") + scale_y_continuous(breaks = seq(0,850, by = 50))

#Write new restructured data to CSV
write.csv(DataByAge, "H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Data/Y1-Y6 enhanced Table 1 9-12-19_By_Age.csv", row.names = FALSE)

