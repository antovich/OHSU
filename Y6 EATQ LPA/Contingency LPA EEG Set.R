# Compare class assignment across models

library(reshape2)
library(purrr)
library(dplyr)

ClassSet1 <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Figures & Output/ADHD Y1 Status Parent & Child EATQ LPA Class and zScores EEG Sample.csv', na = c("", "NA", "-999"))%>%
  select(MERGEID, "PC_2Class" = Classes_2,"PC_3Class" = Classes_3,"PC_4Class" = Classes_4,"PC_5Class" = Classes_5)
ClassSet2 <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Figures & Output/ADHD Y1 Status Parent EATQ LPA Class and zScores EEG Sample.csv', na = c("", "NA", "-999")) %>%
  select(MERGEID, "P_2Class" = Classes_2,"P_3Class" = Classes_3,"P_4Class" = Classes_4)

Contingency <- left_join(ClassSet1, ClassSet2, by = "MERGEID")

Cont_2_2 <- group_by(Contingency,P_2Class,PC_2Class) %>%
  summarize(n = n())

Cont_2_3 <- group_by(Contingency,P_2Class,PC_3Class) %>%
  summarize(n = n())

Cont_2_4 <- group_by(Contingency,P_2Class,PC_4Class) %>%
  summarize(n = n())

Cont_2_5 <- group_by(Contingency,P_2Class,PC_5Class) %>%
  summarize(n = n())

Cont_3_2 <- group_by(Contingency,P_3Class,PC_2Class) %>%
  summarize(n = n())

Cont_3_3 <- group_by(Contingency,P_3Class,PC_3Class) %>%
  summarize(n = n())

Cont_3_4 <- group_by(Contingency,P_3Class,PC_4Class) %>%
  summarize(n = n())

Cont_3_5 <- group_by(Contingency,P_3Class,PC_5Class) %>%
  summarize(n = n())

Cont_4_2 <- group_by(Contingency,P_4Class,PC_2Class) %>%
  summarize(n = n())

Cont_4_3 <- group_by(Contingency,P_4Class,PC_3Class) %>%
  summarize(n = n())

Cont_4_4 <- group_by(Contingency,P_4Class,PC_4Class) %>%
  summarize(n = n())

Cont_4_5 <- group_by(Contingency,P_4Class,PC_5Class) %>%
  summarize(n = n())