remove(list=ls())
# Set working directory WINHOME
setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE")

# Set working directory MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/SHARE")

#load libraries
library(haven)
library(data.table)
library(fpc)
library(factoextra)
library(cluster)
library(dplyr)
library(sharp)
#read and manipulate dataset
df <- read_dta("output/small_r1.dta")
set.seed(1900)  # for reproducibility
df_sample <- df %>%
  dplyr::sample_frac(.10)
df_small <- df_sample[, c("r1cancre", "r1hibpe", "r1lunge", "r1diabe","r1stroke","r1bmi", "r1hearte", "raeducl", "r1iearn_constant","r1hownrnt", "r1agey" )]
df_small <- na.omit(df_small)
df_small <- as.matrix(df_small)
# Normalize the data
df_norm <- scale(df_small)

#-------------------------------------------------------------------------------
#                             Initial Clustering
#-------------------------------------------------------------------------------
# Perform k-means clustering
# Choose the number of clusters (k), here we choose 3 as an example
set.seed(123)  # for reproducibility
km_result <- kmeans(df_small, centers = 3)

# The cluster assignment for each individual is stored in km_result$cluster
df_small$cluster <- km_result$cluster

# Compute distance matrix
dist_matrix <- dist(df_norm)

# Compute cluster statistics
cluster_stats <- cluster.stats(dist_matrix, km_result$cluster)

# Print cluster statistics
print(cluster_stats)

# Visualize clusters
fviz_cluster(km_result, data = df_norm)

model <- lm(r1agey ~ r1cancre+ r1hibpe+ r1lunge+ r1diabe+r1stroke+r1bmi+ 
              r1hearte+ raeducl+ r1iearn_constant+r1hownrnt, data = df)

# Print the summary of the model
summary(model)

stab_clust <- Clustering(xdata = df_norm)
Clusters(stab_clust)
summary(stab_clust)
CalibrationPlot(stab_clust)
print(stab_clust)

stab_clust_PAM <- Clustering(xdata = df_norm, implementation = PAMClustering)
stab_clust_PAM
summary(stab_clust_PAM)
CalibrationPlot(stab_clust_PAM)
print(stab_clust_PAM)
plot(stab_clust_PAM$sampled_pairs)

clusplot(df_norm, stab_clust_PAM$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
