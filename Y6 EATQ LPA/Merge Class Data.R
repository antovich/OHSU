Y1Child <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/ADHD Y1 Status Child EATQ LPA Class and zScores.csv')
Y1Parent <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/ADHD Y1 Status Parent EATQ LPA Class and zScores.csv')
Y6Child <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/ADHD Y6 Status Child EATQ LPA Class and zScores.csv')
Y6Parent <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/ADHD Y6 Status Parent EATQ LPA Class and zScores.csv')
CDA <- read.csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Data/ADHDonly community detection group assignments.csv', na.strings = c(""))

library(dplyr)
Class_Data_Only <- full_join(select(Y1Child, MERGEID, c(14:16)),select(Y1Parent, MERGEID, c(12:14)), by = "MERGEID") %>%
  full_join(.,select(Y6Child, MERGEID, c(14:16)), by = "MERGEID") %>%
  full_join(.,select(Y6Parent, MERGEID, c(12:14)), by = "MERGEID") %>%
  full_join(.,CDA, by = "MERGEID")

write.csv(Class_Data_Only,'H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Class Data Only ADHD.csv', row.names = FALSE )
