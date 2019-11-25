
#Community detection for EATQ temperament measure scales using Y6 data for ADHD and control separately

#Load data manipulation package
library(tidyverse)

#Read in data
EATQ <- read_csv('H:/Projects/Y6 EATQ LPA for Sarah n400/Data/Y6 EATQ and Demo data 8-8-19.csv', na = c("", "NA", "-999"))

#Selecting controls only (DX == 1), only MERGEDID and EATQ variables, performing listwise deletion for missing data
EATQ_Cont <-EATQ %>%
  filter(Y6_ADHD_STATUS == 1) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Selecting ADHD only (DX == 3), only MERGEID and EATQ variables, performing listwise deletion for missing data
EATQ_ADHD <-EATQ %>%
  filter(Y6_ADHD_STATUS == 3) %>% 
  select(MERGEID, Y6_P_EATQ_ACTIVCONT:Y6_P_EATQ_HIP) %>%
  na.omit()

#Get list of control subject means and SDs by EATQ scale
ContMeans = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,mean)
ContSD = apply(EATQ_Cont[,2:ncol(EATQ_Cont)],2,sd)

#Convert ADHD scale scores to z scores based on control means
EATQ_ADHD_z_Cont = cbind(EATQ_ADHD[,1],sweep(EATQ_ADHD[,2:ncol(EATQ_ADHD)],2,ContMeans,"-")  %>%
                           sweep(2,ContSD,"/"))

#Convert Control scale scores z scores based on control means
EATQ_Cont_z_Cont = cbind(EATQ_Cont[,1],sweep(EATQ_Cont[,2:ncol(EATQ_Cont)],2,ContMeans,"-")  %>%
                           sweep(2,ContSD,"/"))

#Optional, save data to file
#write.csv(EATQ_ADHD_z_Cont, 'ADHDzScores.csv', row.names = FALSE)
#write.csv(EATQ_Cont_z_Cont, 'CONTzScores.csv', row.names = FALSE)

#Optional get zscores for ADHD (based on ADHD, not referenced to control)
EATQ_ADHD_z <- as.data.frame(scale(EATQ_ADHD, center = TRUE, scale = TRUE))


#######################
# Community Detection #
#######################

#Load packages
library(tidygraph)
library(tidyverse)
library(ggraph)
library(corrr)
library(tictoc)
library(Matrix)
library(igraph)
library(wesanderson)

#Make dataframe of correlation matrix of the transposed dataset; subjects are correlated rather than variables
#This also only includes the lower half of the matrix and filters out weak correlations (< .25)
res.cor <- correlate(t(EATQ_ADHD_z)) %>%           
  shave(upper = TRUE) %>%           
  stretch(na.rm = TRUE) %>%
  filter(abs(r) >= 0.25)      

#Converts to format for tidygraph
cor.graph <- as_tbl_graph(res.cor, directed = FALSE)

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
#group_spinglass: Group nodes using simulated annealing with igraph::cluster_spinglass()
#group_walktrap: Group nodes via short random walks using igraph::cluster_walktrap()
#group_biconnected_component: Group edges by their membership of the maximal binconnected components using 

#Start timer to assess runtime
tic()
#Set seed for consistent outcomes
set.seed(123)
#Create graph network and detect communities (plot aesthetics can be adjusted afterward)
graph1 <- cor.graph %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_fast_greedy())) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(width = 0.1, colour = "darkgray") +
  geom_node_point(aes(colour = community), size = 2) +
  geom_node_text(aes(label = NA), repel = TRUE) +
  theme_graph(base_family = 'Helvetica')
toc()

#Get list of community members
communityMembership = graph1$data[,4]

#Optional save figure as PDF
#pdf("CDAgraph.pdf", width=10, height=10)

#Adjust graph aesthetics
graph1+
  geom_edge_link(width = 1, colour = "darkgray") +
  geom_node_point(aes(colour = community), size = 10) +
  geom_node_text(aes(label = NA), repel = TRUE)+
  scale_color_manual(values=wes_palette(n = 4, "Darjeeling1"))
#dev.off() #ClosePDF


# Because of the high column (participants) to row (measure subsets) ratio after transposing the data,
# may need to force to be positive definite, using nearPD which adjusts the eigenvalues to get the nearest PD matrix.
# Looks like this has very little affect on the actual correlation values.

#EATQ_ADHD_z_POSDEF = as.data.frame(as.matrix(nearPD(cor(t(EATQ_ADHD_z)), corr = TRUE)$mat))
