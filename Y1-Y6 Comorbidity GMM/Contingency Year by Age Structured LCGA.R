
MplusPath <- "H:/Projects/Y1-Y6 Comorbidity GMM for Sarah/Mplus Scripts/LCGA"


#Provide list for loop below wit the file names and new variable names
ModelList <- list(c("/Cluster/3-class LCGA MOOD", " Output.csv", ".out", "Mood3Age"),
                  c("/Cluster/2-class LCGA ANX", " Output.csv", ".out", "Anx2Age"),
                  c('/Cluster/2-class LCGA ODD', " Output.csv", ".out", "ODD2Age"),
                  c('/Cluster/2-class LCGA CD', " Output.csv", ".out", "CD2Age"),
                  c("/Year/Restricted variance/3-class LCGA By Year MOOD", " Output.csv", ".out", "Mood3Year"),
                  c("/Year/Restricted variance/2-class LCGA By Year ANX", " Output.csv", ".out", "Anx2Year"),
                  c('/Year/Restricted variance/2-class LCGA By Year ODD', " Output.csv", ".out", "ODD2Year"),
                  c("/Year/Restricted variance/2-class LCGA By Year CD", " Output.csv", ".out", "CD2Year"))

for(i in 1:length(ModelList)) {
  x <- read.table(paste0(MplusPath, ModelList[[i]][1], ModelList[[i]][2]))%>% #READ IN TABLE
    `colnames<-`(str_split(str_match(read_file(paste0(MplusPath, ModelList[[i]][1], ModelList[[i]][3])), #EXTRACT VARIABLE NAMES FROM MPLUS OUTPUT
    "(?s)Order and format of variables\\r\\n\\r\\n\\s*(.*)\\r\\n\\r\\n\\s*Save file format")[,2],"\\s+")[[1]][seq(1,ncol(.)*2,2)]) %>%
    select(MERGEID, C) #SELECT CLASS AND ID COLUMNS
 assign(ModelList[[i]][4], x) 
}

#Get first model
AllMods <- get(ModelList[[1]][4])

#Rename "C" (class number) to first model name
names(AllMods)[names(AllMods) == "C"] = ModelList[[1]][4]

#For loop to merge and rename 
for (i in 2:length(ModelList)) {
  
  AllMods <- full_join(AllMods, get(ModelList[[i]][4]), by = "MERGEID")
  names(AllMods)[names(AllMods) == "C"] = ModelList[[i]][4]
  
}

#Look at proportions to match class labels
table(AllMods$Mood3Year)

AllMods$ODD2Year <- recode(AllMods$ODD2Year, "2" = 1, "1" = 2)
AllMods$Mood3Year <- recode(AllMods$Mood3Year, "3" = 1, "2" = 3, "1" = 2)


as.data.frame(table(AllMods$MOOD3Year,AllMods$MOOD3Age,dnn = c("Year", "Age")))


write_last_clip()

