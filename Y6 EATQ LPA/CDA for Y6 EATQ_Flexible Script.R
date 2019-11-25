
#Community detection for EATQ temperament measure scales using Y6 data for ADHD and control separately

#####################################
## OPTIONS ##########################
#####################################

#Which group for plotting? 'ADHD' or 'Control'
Group = "ADHD"

#Which year for ADHD status? 'Y1' or 'Y6'
Year = "Y1"

#Parent report or child report? 'Parent' or 'Child'
Reporter = "Parent"

#Threshold for correlation comparisons in CDA?
CorrThresh = 0.00

#Background setup
library(tidyverse)
library(reshape2)

#Set seed to ensure reproducability
set.seed(123)

#Set working drive
setwd('H:/Projects/Y6 EATQ LPA for Sarah n400')

#Write data to file? (default: no)
#Write joined data to file
WriteJoinData = FALSE
WriteJoinName = "Y6 EATQ & Demographic with Y1 Status.csv"

#Write z-score data to file
WriteZData = FALSE
WriteZName = c(paste("ADHD", Year, "Status", Reporter, 'EATQ zScores.csv'),paste("Control", Year, "Status", Reporter, 'EATQ zScores.csv'))

#Write full class data to file
WriteClassFullData = FALSE
WriteClassFullName = paste(Group, Year, "Status", Reporter, 'EATQ LPA Class and zScores.csv')

#Append additional class assignments to file
AppendClassData = FALSE


#Set up variable names and plotting order
if(Reporter == "Child"){
  #Shortened varible names for MPLus, must be in order of file
  RenameVariables = c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","LIP", "Percept", "Shy","HIP")
  
  #Order of variables for plotting
  PlotOrd = c("AttFoc","Inhib","ActCont","Percept","HIP","LIP","Affil","Shy","Aggress","Frust","Depress","Fear")
  
  #Labels for plotting, must be in same order as 'PlotOrd'
  PlotLab = c("Attention","Inhibitory Control","Activation Control","Perceptual Sensitivity","High Intensity Pleasure",
              "Low Intensity Pleasure","Affiliation","Shyness","Aggression","Frustration","Depressive Mood","Fear")
  
} else if(Reporter == "Parent") {
  #Shortened varible names for MPLus, must be in order of file
  RenameVariables = c("MERGEID","ActCont","Affil","Aggress","AttFoc","Depress","Fear","Frust","Inhib","Shy","HIP")
  
  #Order of variables for plotting
  PlotOrd = c("AttFoc","Inhib","ActCont","HIP","Affil","Shy","Aggress","Frust","Depress","Fear")
  
  #Labels for plotting, must be in same order as 'PlotOrd'
  PlotLab = c("Attention","Inhibitory Control","Activation Control","High Intensity Pleasure",
              "Affiliation","Shyness","Aggression","Frustration","Depressive Mood","Fear")
}


###########################################################################################
# READ IN DATA ############################################################################
###########################################################################################

#Read in data as tidyverse tibble data frame
EATQ <- read_csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Y6 EATQ and Demo data 8-21-19.csv', na = c("", "NA", "-999"))

#Read in Y1 data for Y1 status variable and rename variable for consistency
Y1Data <- read_csv('H:/Projects/Y1 Factor Scores for Mike n1400/Data/all_data_adhd_ML_project_persistent_11-Sep-2018.csv', na = c("-999","NA",""))%>%
  rename("Y1_ADHD_STATUS" = DX)

#Join EATQ dataset with Y1 status variable
EATQ <- left_join(EATQ, select(Y1Data, MERGEID, Y1_ADHD_STATUS), by = "MERGEID") 

############################################################################################
# RUN ######################################################################################
############################################################################################

#Write joined dataset to file
if(WriteJoinData){
  write.csv(EATQ, WriteJoinName, row.names = FALSE)}

#Look at patterns of change in diagnosis
#Get Y1 and Y6 status
DXchange = paste(EATQ$Y1_ADHD_STATUS, EATQ$Y6_ADHD_STATUS, sep = " to ")
#Convert to dataframe
DXchange = as.data.frame(table(DXchange))
#Substitute dx number with diagnosis string
DXchange[,1] = gsub("1", "Control", DXchange[,1])
DXchange[,1] = gsub("2","Subthreshold", DXchange[,1])
DXchange[,1] = gsub("3","ADHD", DXchange[,1])
DXchange[,1] = gsub("4","Not clean control", DXchange[,1])


#Get control-referenced z-scores
StatusVar <-  paste(Year, "_ADHD_STATUS", sep = "")

if(Reporter == "Child"){
  #Selecting controls only (status == 1), only EATQ variables, performing listwise deletion for missing data (all primary data is missing for these anyway)
  EATQ_Cont <-EATQ %>%
    filter(EATQ[StatusVar] == 1) %>% 
    select(MERGEID, Y6_C_EATQ_ACTIVCONT:Y6_C_EATQ_HIP) %>%
    na.omit()
  
  #Selecting ADHD only (status == 3), only EATQ scales, performing listwise deletion for missing data
  EATQ_ADHD <-EATQ %>%
    filter(EATQ[StatusVar] == 3) %>% 
    select(MERGEID, Y6_C_EATQ_ACTIVCONT:Y6_C_EATQ_HIP) %>%
    na.omit()
  
} else if(Reporter == "Parent"){
  #Selecting controls only (status == 1), only EATQ variables, performing listwise deletion for missing data (all primary data is missing for these anyway)
  EATQ_Cont <-EATQ %>%
    filter(EATQ[StatusVar] == 1) %>% 
    select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
    na.omit()
  
  #Selecting ADHD only (status == 3), only EATQ scales, performing listwise deletion for missing data
  EATQ_ADHD <-EATQ %>%
    filter(EATQ[StatusVar] == 3) %>% 
    select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
    na.omit()
  
} else {"Reporter variable not defined"}

#Get list of control subject means and SDs by EATQ scale
ContMeans = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,mean)
ContSD = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,sd)

#Convert ADHD scale scores to z scores based on control means using sweep() and attach MERGEID
EATQ_ADHD_z_Cont = cbind(EATQ_ADHD[,1],sweep(EATQ_ADHD[,2:ncol(EATQ_ADHD)],2,ContMeans,"-")  %>%
                           sweep(2,ContSD,"/"))
#For readability and compatibility with Mplus, shorten variable names
names(EATQ_ADHD_z_Cont) <- RenameVariables

#Convert Control scale scores z scores based on control means using scale() and attach MERGEID
EATQ_Cont_z_Cont = cbind(EATQ_Cont[,1],scale(EATQ_Cont[,2:ncol(EATQ_Cont)]))

#For readability and Mplus, shorten variable names
names(EATQ_Cont_z_Cont) <- RenameVariables

if(WriteZData) {
  #Write zscore data to csv
  write.csv(EATQ_ADHD_z_Cont, WriteZName[1], row.names = FALSE)
  write.csv(EATQ_Cont_z_Cont, WriteZName[2], row.names = FALSE)
}

#######################
# Community Detection #
#######################

#Load packages
library(tidygraph)
library(ggraph)
library(corrr)
library(tictoc)
library(Matrix)
library(igraph)
library(wesanderson)
library(dplyr)


#Make dataframe of correlation matrix of the transposed dataset; subjects are correlated rather than variables
#This also only includes the lower half of the matrix and filters out weak correlations (< .25)
if(Group == "ADHD") {
res.cor <- correlate(t(EATQ_ADHD_z_Cont[2:ncol(EATQ_ADHD_z_Cont)])) %>%           
  shave(upper = TRUE) %>%           
  stretch(na.rm = TRUE) %>%
  filter(abs(r) >= CorrThresh)     
}else if (Group == "Control"){
res.cor <- correlate(t(EATQ_Cont_z_Cont[2:ncol(EATQ_Cont_z_Cont)])) %>%           
  shave(upper = TRUE) %>%           
  stretch(na.rm = TRUE) %>%
  filter(abs(r) >= CorrThresh)      
}else {"Group not defined"}

#Converts to format for tidygraph
cor.graph <- as_tbl_graph(res.cor, directed = FALSE)

##########################################################################################################
# Can use a number of algorithms for determineing groups:
# https://www.rdocumentation.org/packages/tidygraph/versions/1.1.2/topics/group_graph
#group_components: Group by connected compenents using igraph::components()
#group_edge_betweenness: Group densely connected nodes using igraph::cluster_edge_betweenness()
#group_fast_greedy: Group nodes by optimising modularity using igraph::cluster_fast_greedy()
#group_infomap: Group nodes by minimizing description length using igraph::cluster_infomap()
#group_label_prop: Group nodes by propagating labels using igraph::cluster_label_prop()
#group_leading_eigen: Group nodes based on the leading eigenvector of the modularity matrix using igraph::cluster_leading_eigen()
#group_louvain: Group nodes by multilevel optimisation of modularity using igraph::cluster_louvain()
#group_optimal: Group nodes by optimising the moldularity score using igraph::cluster_optimal()
  #From igraph: Note that modularity optimization is an NP-complete problem, and all known algorithms for it have exponential time complexity. 
  #This means that you probably don't want to run this function on larger graphs. 
  #Graphs with up to fifty vertices should be fine, graphs with a couple of hundred vertices might be possible
#group_spinglass: Group nodes using simulated annealing with igraph::cluster_spinglass()
  #implementation	- Character scalar. Currently igraph contains two implementations for the Spin-glass community finding algorithm. 
  #The faster original implementation is the default. 
  #The other implementation, that takes into account negative weights, can be chosen by supplying 'neg' here.
#group_walktrap: Group nodes via short random walks using igraph::cluster_walktrap()
#group_biconnected_component: Group edges by their membership of the maximal binconnected components using 
###########################################################################################################
#Start timer to assess runtime
tic()

#Create graph network and detect communities
graph1 <- cor.graph %>%
  activate(nodes) %>%
  #If weights is NULL then each edge has the same weight
  mutate(community = as.factor(group_spinglass(weights = res.cor$r)))

layout <- create_layout(graph1, layout = 'fr', weights = abs(res.cor$r))
toc()

tic()
plot1 < - ggraph(layout) + 
  geom_edge_link(aes(width = abs(r)), colour = "darkgrey", alpha = 0.5, show.legend = FALSE) +
  geom_node_point(aes(colour = community), size = 5) +
  #geom_node_text(aes(label = NA), repel = TRUE) +
  theme_graph(base_family = 'sans')+
  scale_edge_width_continuous(range = 0.01, limits = c(0.85,1.0))
toc()

#When copying figure, used 1000width by 500height

#Get list of community members
EATQ_ADHD_z_Cont$Class <- layout$community

# #Optional save classdata
# #write.csv(select(EATQ_ADHD_z_Cont, MERGEID, Class), 'classSpinglass.csv', row.names = FALSE)
# Save adjusted figure as PDF
# #pdf("CDAgraph.pdf", width=10, height=10)
# 
# #Adjust graph aesthetics 9e.g., use Wes Anderson colors)
# plot1+
#   scale_color_manual(values=wes_palette(n = 4, "Darjeeling1"))
# #dev.off() #ClosePDF


# Because of the high column (participants) to row (measure subsets) ratio after transposing the data,
# may need to force to be positive definite, using nearPD which adjusts the eigenvalues to get the nearest PD matrix.
# Looks like this has very little affect on the actual correlation values.

#EATQ_ADHD_z_POSDEF = as.data.frame(as.matrix(nearPD(cor(t(EATQ_ADHD_z)), corr = TRUE)$mat))

##################################
## PLOT EATQ MEANS BY CLASS ######
##################################

#Frequency tables for the new class variable
classCount = count(EATQ_ADHD_z_Cont, Class)
classCount = cbind(classCount, percent = prop.table(classCount$n))

#Melt dataframe into long format (scales as rows)
MeltData <- melt(EATQ_ADHD_z_Cont, id.vars = "Class", measure.vars = 2:(1+length(PlotOrd)), variable.name = "Features")


#Cast as averages within feature by class
CastData <- dcast(MeltData, Features + Class~"Mean", mean)
CastData$Class <- as.factor(CastData$Class)

# Plot scale averages by profile
MeansPlot <- CastData %>%
  #Plot feature means by profile group
  ggplot(aes(Features, Mean, group = Class, color = Class)) +
  scale_color_hue(name="Profile", labels=c(
    paste("1 (", round(classCount$percent[1]*100),"%)", sep = ""),
    paste("2 (", round(classCount$percent[2]*100),"%)", sep = ""),
    paste("3 (", round(classCount$percent[3]*100),"%)", sep = ""),
    paste("4 (", round(classCount$percent[4]*100),"%)", sep = ""),
    paste("5 (", round(classCount$percent[5]*100),"%)", sep = ""))) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  ylim(-3,4) +
  #Order of features for figure
  scale_x_discrete(limits = PlotOrd,labels = PlotLab) +
  #Label axes and adjust label size/position
  labs(x = NULL, y = "z-score (control referenced)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", plot.title = element_text(hjust = 0.5))+
  ggtitle(paste(Group, " ", Year," Status ", Reporter, " EATQ CDA ", sep =""))

