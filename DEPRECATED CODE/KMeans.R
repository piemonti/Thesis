remove(list=ls())
# Set working directory WINHOME
#setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE")

# Set working directory MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/SHARE")
# Set working directory
setwd("C:/Users/stud1/Documents/SHARE")

#load libraries
library(haven)
library(data.table)
library(fpc)
library(ggfortify)
library(reshape2)
library(factoextra)
library(cluster)
library(dplyr)
library(tibble)
library(parallel)
library(sharp)
library(robust)
library(mclust)
library(hopkins)
library(HDclassif)
library(cluster)
library(future)
library(future.apply)
library(openxlsx)
#read and manipulate dataset
df <- read_dta("output/share_small.dta")
# set.seed(1900)  # for reproducibility
# df_sample <- df %>%
#   dplyr::sample_frac(.10)
#-------------------------------------------------------------------------------
#                             KMeans clustering 
#-------------------------------------------------------------------------------
r1_df_small <- df[, c("r1cancre", "r1hibpe", "r1lunge", 
                   "r1diabe","r1stroke", "r1bmi","r1hearte", "low_educ","secondary_educ", "r1iearn_constant",
                   "r1hownrnt", "r1agey", "r1gripsum", "r1smoken", "r1vgactx" )]
r1_df_small <- na.omit(r1_df_small)
#df_small <- as.matrix(df_small)
# Normalize the data
r1_df_norm <- scale(r1_df_small)
pca_res = prcomp(r1_df_norm, scale. = F)

PCA_plot = autoplot(pca_res, data = r1_df_norm)
print(PCA_plot)
hopkins_stat = hopkins::hopkins(r1_df_norm)

hopkins_stat # highly clusterable data

# Calculate correlation matrix
correlation_matrix <- cor(r1_df_norm)

# Print the correlation matrix
print(correlation_matrix)

#KMEANS FOR WAVE 1 
set.seed(1900)
morbi_clusters<- kmeans(r1_df_norm, 3)
clusplot(r1_df_norm, morbi_clusters$cluster)
morbi_clusters$size
morbi_clusters$centers
r1_df_small <- as.data.frame(r1_df_small)
r1_df_small$cluster <- morbi_clusters$cluster


# Calculate summary statistics for all clusters
summary_stats_r1 <- aggregate(r1_df_small, by=list(cluster=r1_df_small$cluster), FUN=summary)
print(summary_stats_r1)
write.xlsx(summary_stats_r1, "summary_stats_wave1.xlsx")

sil <- rep(0, 15)
for (k in 2:15) {
  km.res <- kmeans(r1_df_small, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(r1_df_small))
  sil[k] <- mean(ss[, 3])
}
plot(1:15, sil, type = "b", xlab = "Number of clusters", ylab = "Average Silhouette Width")



r6_df_small <- df[, c("r6cancre", "r6hibpe", "r6lunge", 
                      "r6diabe","r6stroke", "r6bmi","r6hearte", 
                      "secondary_educ", "tertiary_educ",
                      "r6itearn_constant",
                      "r6hownrnt", "r6agey" )]
r6_df_small <- na.omit(r6_df_small)
# Normalize the data
r6_df_norm <- scale(r6_df_small)
# Calculate correlation matrix
correlation_matrix <- cor(r6_df_norm)

# Print the correlation matrix
print(correlation_matrix)

pca_res = prcomp(r6_df_norm, scale. = F)

PCA_plot = autoplot(pca_res, data = r6_df_norm)
print(PCA_plot)
hopkins_stat = hopkins::hopkins(r6_df_norm)

hopkins_stat # highly clusterable data


#KMEANS FOR WAVE 6
set.seed(1900)
morbi_clusters<- kmeans(r6_df_norm, 3)
clusplot(r6_df_norm, morbi_clusters$cluster)
morbi_clusters$size
morbi_clusters$centers
r6_df_small <- as.data.frame(r6_df_small)
r6_df_small$cluster <- morbi_clusters$cluster

# Calculate summary statistics for all clusters
summary_stats_r6 <- aggregate(r6_df_small, by=list(cluster=r6_df_small$cluster), FUN=summary)
print(summary_stats_r6)

sil <- rep(0, 15)
for (k in 2:15) {
  km.res <- kmeans(r6_df_small, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(r6_df_small))
  sil[k] <- mean(ss[, 3])
}
plot(1:15, sil, type = "b", xlab = "Number of clusters", ylab = "Average Silhouette Width")
write.xlsx(summary_stats_r6, "summary_stats_wave6.xlsx")


#-------------------------------------------------------------------------------
#                           Implementing PAM using sharp
#-------------------------------------------------------------------------------
r1_df_norm_frame <- data.table::as.data.table(r1_df_norm)
r6_df_norm_frame <- data.table::as.data.table(r6_df_norm)
set.seed(1900)  # for reproducibility
 df_sample_r1 <- r1_df_norm_frame %>%
   dplyr::sample_frac(.20)
 df_sample_r6 <- r6_df_norm_frame %>%
   dplyr::sample_frac(.20)
#Wave 1
stab_clust_PAM_r1 <- Clustering(xdata = df_sample_r1, implementation = PAMClustering)
stab_clust_PAM_r1
summary(stab_clust_PAM_r1)
CalibrationPlot(stab_clust_PAM_r1)
print(stab_clust_PAM_r1)
plot(stab_clust_PAM_r1$sampled_pairs)
#Wave 6
stab_clust_PAM_r6 <- Clustering(xdata = df_sample_r6, implementation = PAMClustering)
stab_clust_PAM_r6
summary(stab_clust_PAM_r6)
CalibrationPlot(stab_clust_PAM_r6)
print(stab_clust_PAM_r6)
plot(stab_clust_PAM_r6$sampled_pairs)


# Function to compute the gap statistic
compute_gap_statistic <- function(data, max_k, nrefs = 100, seed = 1900) {
  set.seed(seed)
  gap <- clusGap(data, FUN = kmeans, K.max = max_k, B = nrefs)
  return(gap)
}

# Usage example
max_clusters <- 15  # Maximum number of clusters to consider
gap_stat_r1 <- compute_gap_statistic(r1_df_small, max_clusters)
gap_stat_r6 <- compute_gap_statistic(r6_df_small, max_clusters)
gap_stat_r1_norm <- compute_gap_statistic(r1_df_norm, max_clusters)
gap_stat_r6_norm <- compute_gap_statistic(r6_df_norm, max_clusters)

gap_stat_r1_norm
gap_stat_r6_norm
# Plotting gap statistic
plot(gap_stat_r1$Tab[, "gap"], xlab = "Number of clusters (K)", ylab = "Gap Statistic",
     main = "Gap Statistic Plot - Wave 1", type = "b", xaxt = "n")
axis(1, at = 1:max_clusters)

plot(gap_stat_r6$Tab[, "gap"], xlab = "Number of clusters (K)", ylab = "Gap Statistic",
     main = "Gap Statistic Plot - Wave 6", type = "b", xaxt = "n")
axis(1, at = 1:max_clusters)

plot(gap_stat_r1_norm$Tab[, "gap"], xlab = "Number of clusters (K)", ylab = "Gap Statistic",
     main = "Gap Statistic Plot - Wave 1norm", type = "b", xaxt = "n")
axis(1, at = 1:max_clusters)

plot(gap_stat_r6_norm$Tab[, "gap"], xlab = "Number of clusters (K)", ylab = "Gap Statistic",
     main = "Gap Statistic Plot - Wave 6norm", type = "b", xaxt = "n")
axis(1, at = 1:max_clusters)

# # Step 2: Identify Common Individuals
# common_individuals <- intersect(rownames(r1_df_small), rownames(r6_df_small))
# 
# # Step 3: Compute Co-Membership Score
# co_membership_score <- function(wave1_clusters, wave2_clusters, common_individuals) {
#   common_clusters <- sum(wave1_clusters[common_individuals] == wave2_clusters[common_individuals])
#   total_common_individuals <- length(common_individuals)
#   co_membership <- common_clusters / total_common_individuals
#   return(co_membership)
# }
# co_membership_score_r1_r6 <- co_membership_score(r1_df_small$cluster, r6_df_small$cluster, common_individuals)
# 
# 
# co_membership_score_r1_r6

