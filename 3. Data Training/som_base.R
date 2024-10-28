# Install Packages
install.packages("kohonen", repos = "http://cran.us.r-project.org")
install.packages("dummies", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("sp", repos = "http://cran.us.r-project.org")
install.packages("factoextra", repos = "http://cran.us.r-project.org")
install.packages("NbClust", repos = "http://cran.us.r-project.org")
install.packages("dplyr")
install.packages("Rcpp")
install.packages('dendextend')
install.packages("ggdendro")
install.packages("ggplot2")
install.packages("clValid")
install.packages("fpc")## a first non-trivial case
#(d.t <- dip.test(c(0,0, 1,1))) # "perfect bi-modal for n=4" --> p-value = 0
#stopifnot(d.t$p.value == 0)
install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("devtools")
devtools::install_version('dummies', '1.5.6')

install.packages("dplyr")
install.packages("clv")
install.packages("clusterCrit")
install.packages("clValid")
install.packages("rdist")
install.packages("aricode")
install.packages("gmp") 
install.packages("ClusterR")
install.packages("ade4")
install.packages("clustrd")
install.packages("memisc")
install.packages("foreign")
install.packages("clusterSim")
install.packages("dummies")

# Load Libraries
library(clustrd)
library(factoextra)
library(NbClust)
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(readr)
library(dplyr)
library(ggdendro)
library(magrittr)
library(dendextend)
library(clValid)
library(cluster)
library(fpc)
library(factoextra)
library(dplyr)
library(clv)
library(clusterCrit)
library(klaR)
library(rdist)
library(RColorBrewer)
library(aricode)
library(ClusterR)
library(gtools)
library(ade4)
library(clValid)
library(haven)
library(kohonen)


####################################################################################################

require(dummies)
require(kohonen)
source('coolBlueHotRed.R')

pretty_palette <- c("#1f77b4", '#2ca02c', '#ff7f0e', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#e9f542', '#f542ad')

is_model_trained <- TRUE

data_train <- read.csv("data_preprocessed_ordinal.csv", sep=",", header = TRUE)

if (is_model_trained) {
  som_model <- readRDS("model_7x7_5000_ordinal.rds")
} else {
  data_train_matrix <- as.matrix(data_train)
  data_train_matrix <- data_train_matrix[, -c(1:2)]
  som_grid <- somgrid(xdim = 7, ydim = 7, topo = "hexagonal")
  
  system.time(som_model <- som(data_train_matrix,
                               grid=som_grid, 
                               rlen=3000,
                               alpha=c(0.05, 0.01),
                               keep.data = TRUE ))
  
  rm(som_grid, data_train_matrix)
  is_model_trained <- TRUE
  saveRDS(som_model, "model_7x7_5000_ordinal.rds")
}


plot(som_model, type = "mapping")
plot(som_model, type = "changes")
plot(som_model, type = "mapping", main="Mapping", palette.name=coolBlueHotRed) # export
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed) # export
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed) 
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors) # export
plot(som_model, type = "codes", main="") # export

# Heatmap
source('plotHeatMap.R')
plotHeatMap(som_model, data_train, variable=0) # export 


# Extract m-prototype vectors
m_prototypes <- som_model$codes[[1]]
m_prototypes <- m_prototypes[, -c(1:2)]
m_prototypes <- m_prototypes[, colSums(m_prototypes != 0) > 0]

pretty_palette <- c("#1f77b4", '#2ca02c', '#d62728', '#ff7f0e', '#9467bd', '#8c564b', '#e377c2', '#e9f542', '#f542ad')

# Perform clustering on SOM prototypes using AHC
som_cluster <- cutree(hclust(dist(m_prototypes), method = "ward.D2"), 4)
plot(som_model, type="mapping", 
     bgcol = pretty_palette[som_cluster], 
     keepMargins = F,
     col = NA,
     main = "")
add.cluster.boundaries(som_model, som_cluster) # export

# Perform clustering on SOM prototypes using k-means
som_cluster <- cutree(hclust(dist(m_prototypes), method = "ward.D2"), 4)
#km.res <- eclust(m_prototypes, "hclust", k = 4, graph = FALSE)
#som_cluster <- km.res$cluster
plot(som_model, bgcol = pretty_palette[som_cluster], main = "Clusters") # EXPORT

# Draw AHC dendrogram 
dc <- dist(m_prototypes)
cah <- hclust(dc, method = "ward.D2")
plot(cah, hang=-1,labels=F)
rect.hclust(cah, k=4) # export

dd <- dist(som_model$codes[[1]])
hc<- hclust(dist(som_model$codes[[1]]), method = "ward.D2")
dend <- as.dendrogram(hc)

dend %>% set("branches_k_color",            
value = c("red", "blue", "violet", "green"), k = 4) %>% 
plot(main = "")
dend %>% rect.dendrogram(k=4, border = 8, lty = 5, lwd = 2)

# Save cluster label in data_train
som_ahc_cluster <- cutree(hclust(dist(m_prototypes), method = "ward.D"), 4)
data_train$cluster <- som_ahc_cluster[som_model$unit.classif]
#data_train$neuron <- som_model$unit.classif

# Save models and matrix with neuron and clustering label
saveRDS(som_model, "model_7x7_ordinal_4_clusters.rds")
write.csv(m_prototypes, file = 'm_prototypes_ordinal.csv')
write.csv(data_train, file = "data_train_ordinal.csv", row.names= FALSE)

####################################################################################################
# Compute CVIS for raw data
####################################################################################################

# Check with prototype vectors
m_prototypes <- som_model$codes[[1]]
distance_matrix_prototypes <- dist(m_prototypes)
som_cluster <- cutree(hclust(distance_matrix_prototypes, method = "ward.D2"), 2)

intern <- clValid(m_prototypes, 2:7, clMethods=c("hierarchical", "kmeans"), validation="internal")
summary(intern)

cvi_name = "Calinski_Harabasz"
cvi_name = "Davies_Bouldin"
# Compute cvi for different k clusters
for (num_clusters in c(2, 3, 4, 5, 6)) {
  cluster_label_ward <- cutree(hclust(dist(m_prototypes), method = "ward.D2"), num_clusters)
  print(intCriteria(m_prototypes, cluster_label_ward, cvi_name))
}

# Compute wss for different k clusters
dd <- dist(m_prototypes)
fviz_nbclust(m_prototypes, diss=dd, kmeans, method = "wss")

# Other implementations CH index with Euclidean distance
calinhara(m_prototypes, som_cluster)
intCriteria(m_prototypes, som_cluster, "Calinski_Harabasz")[1]

data_mastery <- read.csv("data_cluster_mastery.csv")
label_mastery <- as.matrix(data_mastery)
data_train$mastery <- label_mastery
