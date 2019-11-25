#for TMCQ temperament measures using Y1 data only.

#Set working drive
setwd('H:/Projects')

#Load data manipulation package
library(tidyverse)

#Read in data
TMCQ <- read_csv("all_data_adhd_ML_project_persistent_11-Sep-2018.csv")

#Selecting controls only (DX == 1), only TMCQ variables, performing listwise deletion for missing data
TMCQ_Cont <-TMCQ %>%
  filter(DX == 1) %>% 
  select(Y1_P_TMCQ_ACTIVCONT:Y1_P_TMCQ_OPENNESS) %>%
  na.omit()

#Selecting ADHD only (DX == 3), only TMCQ variables, performing listwise deletion for missing data
TMCQ_ADHD <-TMCQ %>%
  filter(DX == 3) %>% 
  select(Y1_P_TMCQ_ACTIVCONT:Y1_P_TMCQ_OPENNESS) %>%
  na.omit()

#ADHD sublist based on above
TMCQ_ADHD_Sublist <-TMCQ %>%
  filter(DX == 3) %>% 
  select(MERGEID, Y1_P_TMCQ_ACTIVCONT:Y1_P_TMCQ_OPENNESS) %>%
  na.omit()

#Get list of control subject means and SDs by TMCQ variable
ContMeans = apply(TMCQ_Cont,2,mean)
ContSD = apply(TMCQ_Cont,2,sd)

#Convert to z scores
TMCQ_ADHD_z_Cont = sweep(TMCQ_ADHD,2,ContMeans,"-")  %>%
  sweep(2,ContSD,"/")

TMCQ_ADHD_z <- as.data.frame(scale(TMCQ_ADHD, center = TRUE, scale = TRUE))


#######################
# Community Detection #
#######################

library(tidygraph)
library(tidyverse)
library(ggraph)
library(corrr)
library(tictoc)
library(Matrix)
library(igraph)
library(wesanderson)

smallData <- as.data.frame(as.matrix(nearPD(cor(t(TMCQ_ADHD_z[1:25,])), corr = TRUE)$mat))
smallData <- as_cordf(smallData)

res.cor <- smallData %>%           
  shave(upper = TRUE) %>%           
  stretch(na.rm = TRUE) %>%
  filter(abs(r) >= 0.25)      

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

tic()
set.seed(123)
graph1 <- cor.graph %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_fast_greedy())) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(width = 0.1, colour = "darkgray") +
  geom_node_point(aes(colour = community), size = 2) +
  geom_node_text(aes(label = NA), repel = TRUE) +
  theme_graph(base_family = 'Helvetica')
toc()

communityMembership = graph1$data[,4]

#pdf("CDAgraph.pdf", width=10, height=10)
graph1+
  geom_edge_link(width = 1, colour = "darkgray") +
  geom_node_point(aes(colour = community), size = 10) +
  geom_node_text(aes(label = NA), repel = TRUE)+
  scale_color_manual(values=wes_palette(n = 4, "Darjeeling1"))
#dev.off()

install.packages("visNetwork")
library(visNetwork)
