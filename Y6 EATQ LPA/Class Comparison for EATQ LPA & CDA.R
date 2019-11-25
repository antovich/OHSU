#
#Look at class assignment consistency across different models (LPA/CDA for Y1/Y6 status from Parent/Child)
#

#Set working drive
setwd("H:/Projects/Y6 EATQ LPA for Sarah n400")

library(plyr)
#Load data

Class_Data <- read.csv("H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Class Data Only ADHD.csv")

#Recoded class numbers based on visual inspection (i.e., coded similar looking classes with the same number across models for comparison)
#ADHD_Y1_Parent_Classes_3 was used as the reference order: old[1,2,3] = new[1,2,3] - NO CHANGE
#ADHD_Y6_Parent_Classes_3 revised order: old[1,2,3] = new[3,2,1]
#ADHD_Y1_Child_Classes_3 revised order: old[1,2,3] = new[1,2,3] - NO CHANGE
#ADHD_Y6_Child_Classes_3 revised order: old[1,2,3] = new[3,2,1]
#ADHD_Y1_Parent_CDA revised order: old[1,2,3] = new[1,2,3] - NO CHANGE
#ADHD_Y6_Parent_CDA revised order: old[1,2,3] = new[1,3,2]

Class_Data$ADHD_Y6_Parent_Classes_3 <- recode(Class_Data$ADHD_Y6_Parent_Classes_3, '1'=3,'2'=2,'3'= 1)
Class_Data$ADHD_Y6_Child_Classes_3 <- recode(Class_Data$ADHD_Y6_Child_Classes_3, '1'=3,'2'=2,'3'= 1)
Class_Data$ADHD_Y6_Parent_CDA <- recode(Class_Data$ADHD_Y6_Parent_CDA, '1'=1,'2'=3,'3'= 2)

Pattern_Count <- count(Class_Data, vars = c("ADHD_Y1_Child_Classes_3", "ADHD_Y1_Parent_Classes_3", "ADHD_Y6_Child_Classes_3",
                           "ADHD_Y6_Parent_Classes_3","ADHD_Y1_Parent_CDA","ADHD_Y6_Parent_CDA"))
# Count_Y1 <- count(Class_Data, vars = c("ADHD_Y1_Child_Classes_3", "ADHD_Y1_Parent_Classes_3", "ADHD_Y1_Parent_CDA"))
# Count_Y6 <- count(Class_Data, vars = c("ADHD_Y6_Child_Classes_3", "ADHD_Y6_Parent_Classes_3","ADHD_Y6_Parent_CDA"))

Count_Y1_Y6_Parent <- count(Class_Data, vars = c("ADHD_Y1_Parent_Classes_3", "ADHD_Y6_Parent_Classes_3"))
Count_Y1_Y6_Child <- count(Class_Data, vars = c("ADHD_Y1_Child_Classes_3", "ADHD_Y6_Child_Classes_3"))
Count_Y1_Parent_Child <- count(Class_Data, vars = c("ADHD_Y1_Parent_Classes_3", "ADHD_Y1_Child_Classes_3"))
Count_Y6_Parent_Child <- count(Class_Data, vars = c("ADHD_Y6_Parent_Classes_3", "ADHD_Y6_Child_Classes_3"))
Count_Y1_CDA_LPA <- count(Class_Data, vars = c("ADHD_Y1_Parent_Classes_3", "ADHD_Y1_Parent_CDA"))
Count_Y6_CDA_LPA <- count(Class_Data, vars = c("ADHD_Y6_Parent_Classes_3", "ADHD_Y6_Parent_CDA"))

write.csv(Class_Data,"H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Class Data Only ADHD_RECODED.csv", row.names = FALSE)
