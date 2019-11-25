library(reshape2)
library(purrr)
library(dplyr)

#Recode current Class variable from LPA script?
CurrRecode = FALSE


Coherence <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Class Data Only ADHD.csv', na = c("", "NA", "-999"))

#Because class labels (i.e., the number of the class) are arbitrary, 
#realigned via visual inspection of the data (see EATQ Model Plots.xsls), used Y1 Parent 2-Class as reference for classes 1 & 2 and Y1 Parent 3-class for classes 3 & 4
#For recode(), format is "Old" = "New"
Coherence$ADHD_Y1_Parent_CDA_re <- recode(Coherence$ADHD_Y1_Parent_CDA,"1"=3,"2"=1,"3"=2) 
Coherence$ADHD_Y6_Parent_CDA_re <- recode(Coherence$ADHD_Y6_Parent_CDA,"1"=3,"2"=2,"3"=1)
Coherence$ADHD_Y6_Parent_Classes_2_re <- recode(Coherence$ADHD_Y6_Parent_Classes_2, "1"=2,"2"=1)
Coherence$ADHD_Y1_Child_Classes_2_re <- recode(Coherence$ADHD_Y1_Child_Classes_2, "1"=2,"2"=1 )
Coherence$ADHD_Y6_Child_Classes_2_re <- recode(Coherence$ADHD_Y6_Child_Classes_2, "1"=2,"2"=1)
Coherence$ADHD_Y1_Parent_Classes_3_re <- recode(Coherence$ADHD_Y1_Parent_Classes_3, "1"=1,"2"=3,"3"=2)
Coherence$ADHD_Y6_Parent_Classes_3_re <- recode(Coherence$ADHD_Y6_Parent_Classes_3, "1"=2,"2"=3,"3"=1)
Coherence$ADHD_Y1_Child_Classes_3_re <- recode(Coherence$ADHD_Y1_Child_Classes_3, "1"=2,"2"=1,"3"=3)
Coherence$ADHD_Y6_Child_Classes_3_re <- recode(Coherence$ADHD_Y6_Child_Classes_3, "1"=3,"2"=1,"3"=2)
Coherence$ADHD_Y1_Parent_Classes_4_re <- recode(Coherence$ADHD_Y1_Parent_Classes_4, "1"=1,"2"=4,"3"=3,"4"=2)
Coherence$ADHD_Y6_Parent_Classes_4_re <- recode(Coherence$ADHD_Y6_Parent_Classes_4, "1"=2,"2"=3,"3"=1,"4"=4)
Coherence$ADHD_Y1_Child_Classes_4_re <- recode(Coherence$ADHD_Y1_Child_Classes_4, "1"=2,"2"=3,"3"=1,"4"=4)
Coherence$ADHD_Y6_Child_Classes_4_re <- recode(Coherence$ADHD_Y6_Child_Classes_4, "1"=4,"2"=1,"3"=3,"4"=2)
Coherence$ADHD_Y1_ParChi_Classes_3_re <- recode(Coherence$ADHD_Y1_ParChi_Classes_3, "1"=3,"2"=1,"3"=2)

if(CurrRecode) {
# Recode class variable from current LPA script for plotting (which must be run first)
ADHD_Classes <- ADHD_Classes %>%
  mutate(Class = recode(Class, "1"=3,"2"=1,"3"=2))
Coherence <- full_join(Coherence, select(ADHD_Classes, MERGEID, Class), by = "MERGEID") %>%
  rename(ADHD_Y1_ParChi_Classes_3_re = Class)}


write.csv(Coherence, "H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Class Data Only ADHD_Recoded_R.csv", row.names = FALSE)

    
CrossTabP2C2 <- group_by(Coherence,ADHD_Y1_Parent_Classes_2,ADHD_Y1_Child_Classes_2_re) %>%
  summarize(n = n())

CrossTabP2C4 <- group_by(Coherence,ADHD_Y1_Parent_Classes_2,ADHD_Y1_Child_Classes_4_re) %>%
  summarize(n = n())

CrossTabP2CDA <- group_by(Coherence,ADHD_Y1_Parent_Classes_2,ADHD_Y1_Parent_CDA_re) %>%
  summarize(n = n())

CrossTabP2PC4 <- group_by(Coherence,ADHD_Y1_Parent_Classes_2,ADHD_Y1_ParChi_Classes_3_re) %>%
  summarize(n = n())

