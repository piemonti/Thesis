
remove(list = ls())
gc()
# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(reshape2)
library(stargazer)
library(tabulator)
library(fastcluster)
library(factoextra)
library(dendextend)
library(cluster)
library(ggdendro)
#MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA")
#WIN HOME
setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")
#SERVER
#setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")
#read datasets
hrs <- read_dta("HRS/output/hrs_small.dta")
share <- read_dta("SHARE/output/share_small.dta")
share_w1 <- read_dta("SHARE/output/share_w1.dta")
share_w6 <- read_dta("SHARE/output/share_w6.dta")
hrs_w7 <- read_dta("HRS/output/hrs_w7.dta")
hrs_w11 <- read_dta("HRS/output/hrs_w11.dta")

#------------- SHARE ----------------
#cross-sectional SHARE wave 1
share_r1 <- share_w1[, c("r1earnings_constant", "low_educ", "secondary_educ", "tertiary_educ","r1bmi",
                      "r1_risky", "r1no_phys_act", "r1low_phys_act", "r1good_phys_act", "r1agey", "r1_morbi_2", "r1_morbi_3_plus", "r1_healthy",
                      "r1adlfive_0", "r1adlfive_1_2", "r1adlfive_3_plus", "r1iadl_0", "r1_iadl_1_2", "r1_iadl_3_plus", "r1_retired")]
set.seed(1900)  # for reproducibility


share_r1s <- na.omit(share_r1)
share_r1_scaled <- scale(share_r1s)
# Sample 20% of the data
share_r1sm <- as.data.frame(share_r1_scaled) 
share_r1sm <- share_r1sm %>%
  dplyr::sample_frac(.20)
# Dissimilarity matrix
d <- dist(share_r1_scaled, method = "euclidean")


as.matrix(d)[1:6, 1:6]

# Hierarchical clustering using Ward Linkage
hc1 <- hclust(d, method = "ward.D2")
# Plot the obtained dendrogram
#fviz_dend(hc1, cex = 0.5)

# Compute cophentic distance
res.coph <- cophenetic(hc1)
# Correlation between cophenetic distance and # the original distance
cor(d, res.coph)

#--Linkage methods (values from 0 to 1, higher is better. Threshold:0.75):
# Ward.D2: 0.7100689
# Complete: 0.5737532
# Centroid: 0.8254506
# Centroid is the best method, but it is not recommended for large datasets because
# it is computationally expensive. Ward.D2 is the second best method and it is better at capturing
#\different clusters
# # Function for hierarchical clustering to be used in clusGap
# hclusCut <- function(x, k, d.meth = "euclidean", ...) {
#   list(cluster = cutree(hclust(dist(x, method = d.meth), method = "ward.D2"), k = k))
# }
#
# # Calculate the gap statistic
# set.seed(1900)  # For reproducibility
# gap_stat <- clusGap(share_r1sm, FUN = hclusCut, K.max = 12, B = 150)
#
# # Print the gap statistic results
# print(gap_stat)
# # Plot the gap statistic
# gapplot <- plot(gap_stat, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
# ggsave("gap.pdf", gapplot, dpi = 300, width = 10, height = 10)

grp <- cutree(hc1, k = 12)
table(grp)
# Create a data frame by adding the cluster assignments to your original data
final_data <- cbind(share_r1s, cluster = factor(grp))
cluster_summaries <- by(final_data, final_data$cluster, summary)

# Capture the output of the cluster summaries
dend_data <- dendro_data(hc1)
ggdendrogram(dend_data)
dend <- as.dendrogram(hc1)
dend <- color_branches(dend, k = 12)
plot(cut(dend, h=30)$upper,
     main="Upper tree of cut at h=30")

# Calculate the size of each cluster (number of observations per branch)
cluster_sizes <- as.data.frame(table(grp))
names(cluster_sizes) <- c("cluster", "size")
print(cluster_sizes)
sink("cluster_sizes112.tex")
 stargazer(cluster_sizes, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()
require("igraph")
prova <- fviz_dend(cut(dend, h=30)$upper, k = 12, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggsave("phylo1.pdf", prova, dpi = 300, width = 10, height = 10)

grp <- cutree(hc1, k = 12)
table(grp)
# Create a data frame by adding the cluster assignments to your original data
final_data <- cbind(share_r1s, cluster = factor(grp))

cluster_summaries <- by(final_data, final_data$cluster, summary)



#------------- Wave 6 ----------------

share_r6 <- share_w6[, c("r6earnings_constant", "low_educ", "secondary_educ", "tertiary_educ","r6bmi",
                         "r6_risky", "r6no_phys_act", "r6low_phys_act", "r6good_phys_act", "r6agey", "r6_morbi_2", "r6_morbi_3_plus", "r6_healthy",
                         "r6adlfive_0", "r6adlfive_1_2", "r6adlfive_3_plus", "r6iadl_0", "r6_iadl_1_2", "r6_iadl_3_plus", "r6_retired")]
set.seed(1900)  # for reproducibility
# share_r1s <- share_r1 %>%
#   dplyr::sample_frac(.10)

share_r6s <- na.omit(share_r6)
share_r6_scaled <- scale(share_r6s)

# Dissimilarity matrix
d <- dist(share_r6_scaled, method = "euclidean")


as.matrix(d)[1:6, 1:6]

# Hierarchical clustering using Ward Linkage
hc6 <- hclust(d, method = "ward.D2")
# Plot the obtained dendrogram
#fviz_dend(hc6, cex = 0.5)

# Compute cophentic distance
res.coph2 <- cophenetic(hc6)
# Correlation between cophenetic distance and # the original distance
cor(d, res.coph2)

#--Linkage methods (values from 0 to 1, higher is better. Threshold:0.75):
# Ward.D2: 0.7546292
# Complete: 0.5737532
# Centroid: 0.8254506
# Centroid is the best method, but it is not recommended for large datasets because
# it is computationally expensive. Ward.D2 is the second best method and it is better at capturing
#\different clusters
# Function for hierarchical clustering to be used in clusGap
# hclusCut <- function(x, k, d.meth = "euclidean", ...) {
#   list(cluster = cutree(hclust(dist(x, method = d.meth), method = "ward.D2"), k = k))
# }
#
# # Calculate the gap statistic
# set.seed(1900)  # For reproducibility
# gap_stat2 <- clusGap(share_r6_scaled, FUN = hclusCut, K.max = 12, B = 150)
#
# # Print the gap statistic results
# print(gap_stat2)
# # Plot the gap statistic
# plot(gap_stat2, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#


grp6 <- cutree(hc6, k = 12)
table(grp6)
# Create a data frame by adding the cluster assignments to your original data
final_data6 <- cbind(share_r6s, cluster = factor(grp6))
cluster_summaries6 <- by(final_data6, final_data6$cluster, summary)

# Capture the output of the cluster summaries
dend_data6 <- dendro_data(hc6)
ggdendrogram(dend_data6)
 dend6 <- as.dendrogram(hc6)
dend6 <- color_branches(dend6, k = 12)
plot(cut(dend6, h=30)$upper,
     main="Upper tree of cut at h=30")
require("igraph")
phylo6 <- fviz_dend(cut(dend6, h=30)$upper, k = 12, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggsave("phylo6.pdf", phylo6, dpi = 300, width = 10, height = 10)

grp6 <- cutree(hc6, k = 12)
table(grp6)
# Create a data frame by adding the cluster assignments to your original data
final_data6 <- cbind(share_r6s, cluster = factor(grp6))

cluster_summaries6 <- by(final_data6, final_data6$cluster, summary)
cluster_sizes6 <- as.data.frame(table(grp6))
names(cluster_sizes6) <- c("cluster", "size")
print(cluster_sizes6)
sink("cluster_sizes612.tex")
 stargazer(cluster_sizes6, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()
capture.output(cluster_summaries, file = "cluster_summaries_w1.txt")
capture.output(cluster_summaries6, file = "cluster_summaries_w6.txt")


#------------- HRS ----------------
#cross-sectional HRS wave 7
hrs_r7 <- hrs_w7[, c("r7iearn_constant", "low_educ", "secondary_educ", "tertiary_educ","r7bmi",
                      "r7_risky", "r7vgactx", "r7agey_e", "r7_morbi_2", "r7_morbi_3_plus", "r7_healthy",
                      "r7adlfive_0", "r7adlfive_1_2", "r7adlfive_3_plus", "r7_iadl_0", "r7_iadl_1_2", "r7_iadl_3_plus", "r7_retired")]
set.seed(1900)  # for reproducibility
hrs_r7s <- na.omit(hrs_r7)
hrs_r7_scaled <- scale(hrs_r7s)
# Dissimilarity matrix
d_w7 <- dist(hrs_r7_scaled, method = "euclidean")
as.matrix(d_w7)[1:6, 1:6]

# Hierarchical clustering using Ward Linkage
hc7 <- hclust(d_w7, method = "ward.D2")
# Plot the obtained dendrogram
#fviz_dend(hc1, cex = 0.5)

# Compute cophentic distance
res.coph7 <- cophenetic(hc7)
# Correlation between cophenetic distance and # the original distance
cor(d_w7, res.coph7)
#ward.D2: 0.7109887
set.seed(1900)  # for reproducibility
# Sample 20% of the data
hrs_r7sampled <- as.data.frame(hrs_r7_scaled) %>%
   dplyr::sample_frac(.20)

#
# hclusCut <- function(x, k, d.meth = "euclidean", ...) {
#   list(cluster = cutree(hclust(dist(x, method = d.meth), method = "ward.D2"), k = k))
# }
#
# # Calculate the gap statistic
# set.seed(1900)  # For reproducibility
# gap_stat <- clusGap(hrs_r7sampled, FUN = hclusCut, K.max = 20, B = 150)
#
# # Print the gap statistic results
# print(gap_stat)
# # Plot the gap statistic
# gapplot <- plot(gap_stat, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#
#
#
#---------------------------

# # Set up parallel backend
# library(cluster)
# library(doParallel)
# library(foreach)
#
# # Define the hierarchical clustering function
# hclusCut <- function(x, k, d.meth = "euclidean", ...) {
#   list(cluster = cutree(hclust(dist(x, method = d.meth), method = "ward.D2"), k = k))
# }
#
# # Set up parallel backend
# numCores <- detectCores() - 10  # Use one less than the number of available cores
# cl <- makeCluster(numCores)
# registerDoParallel(cl)
#
# # Define a function to compute the gap statistic in parallel
# parallelClusGap <- function(data, FUN, K.max, B, d.meth = "euclidean") {
#   foreach(b = 1:B, .combine = rbind, .packages = 'cluster') %dopar% {
#     set.seed(1900 + b)  # Ensure reproducibility within parallel execution
#     cluster_result <- FUN(data, k = K.max, d.meth = d.meth)
#     gap_result <- clusGap(data, FUN = FUN, K.max = K.max, B = 1)
#     gap_result$Tab
#   }
# }
#
# # Record the computation time
# start_time <- Sys.time()
#
# # Run the parallel computation
# set.seed(1900)  # For reproducibility
# gap_stat_parallel <- parallelClusGap(hrs_r7sampled, FUN = hclusCut, K.max = 20, B = 150)
#
# # Combine the results and compute the final gap statistic
# gap_stat <- list(Tab = apply(gap_stat_parallel, 2, mean))
#
# # Stop the parallel cluster
# stopCluster(cl)
#
# end_time <- Sys.time()
#
# # Print the time taken for the computation
# print(end_time - start_time)
#
# # Print the gap statistic results
# print(gap_stat)
# # Plot the gap statistic
# gapplot <- plot(gap_stat, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#
# # Plot the gap statistic
# gapplot <- plot(gap_stat, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#---------------------------

grp7 <- cutree(hc7, k = 15)
table(grp7)
# Create a data frame by adding the cluster assignments to your original data
final_data7 <- cbind(hrs_r7s, cluster = factor(grp7))
cluster_summaries7 <- by(final_data7, final_data7$cluster, summary)

# Capture the output of the cluster summaries
dend_data7 <- dendro_data(hc7)
ggdendrogram(dend_data7)
 dend7 <- as.dendrogram(hc7)
dend7 <- color_branches(dend7, k = 15)
plot(cut(dend7, h=30)$upper,
     main="Upper tree of cut at h=30")
require("igraph")
phylo7 <- fviz_dend(cut(dend7, h=30)$upper, k = 15, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggsave("phylo7.pdf", phylo7, dpi = 300, width = 10, height = 10)

grp7 <- cutree(hc7, k = 15)
table(grp7)

#cross-sectional HRS wave 11
hrs_r11 <- hrs_w11[, c("r11iearn_constant", "low_educ", "secondary_educ", "tertiary_educ","r11bmi",
                      "r11_risky", "r11vgactx", "r11agey_e", "r11_morbi_2", "r11_morbi_3_plus", "r11_healthy",
                      "r11adlfive_0", "r11adlfive_1_2", "r11adlfive_3_plus", "r11_iadl_0", "r11_iadl_1_2", "r11_iadl_3_plus", "r11_retired")]
set.seed(1900)  # for reproducibility
hrs_r11s <- na.omit(hrs_r11)
hrs_r11_scaled <- scale(hrs_r11s)
# Dissimilarity matrix
d_w11 <- dist(hrs_r11_scaled, method = "euclidean")
as.matrix(d_w11)[1:6, 1:6]

# Hierarchical clustering using Ward Linkage
hc11 <- hclust(d_w11, method = "ward.D2")
# Plot the obtained dendrogram
#fviz_dend(hc1, cex = 0.5)

# Compute cophentic distance
res.coph11 <- cophenetic(hc11)
# Correlation between cophenetic distance and # the original distance
cor(d_w11, res.coph11)
#ward.D2: 0.111098811
set.seed(1900)  # for reproducibility
# Sample 20% of the data
hrs_r11sampled <- as.data.frame(hrs_r11_scaled) %>%
   dplyr::sample_frac(.20)

#
# hclusCut <- function(x, k, d.meth = "euclidean", ...) {
#   list(cluster = cutree(hclust(dist(x, method = d.meth), method = "ward.D2"), k = k))
# }
#
# # Calculate the gap statistic
# set.seed(1900)  # For reproducibility
# gap_stat <- clusGap(hrs_r11sampled, FUN = hclusCut, K.max = 20, B = 150)
#
# # Print the gap statistic results
# print(gap_stat)
# # Plot the gap statistic
# gapplot <- plot(gap_stat, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#
#
#
#---------------------------

# # Set up parallel backend
# library(cluster)
# library(doParallel)
# library(foreach)
#
# # Define the hierarchical clustering function
# hclusCut <- function(x, k, d.meth = "euclidean", ...) {
#   list(cluster = cutree(hclust(dist(x, method = d.meth), method = "ward.D2"), k = k))
# }
#
# # Set up parallel backend
# numCores <- detectCores() - 10  # Use one less than the number of available cores
# cl <- makeCluster(numCores)
# registerDoParallel(cl)
#
# # Define a function to compute the gap statistic in parallel
# parallelClusGap <- function(data, FUN, K.max, B, d.meth = "euclidean") {
#   foreach(b = 1:B, .combine = rbind, .packages = 'cluster') %dopar% {
#     set.seed(1900 + b)  # Ensure reproducibility within parallel execution
#     cluster_result <- FUN(data, k = K.max, d.meth = d.meth)
#     gap_result <- clusGap(data, FUN = FUN, K.max = K.max, B = 1)
#     gap_result$Tab
#   }
# }
#
# # Record the computation time
# start_time <- Sys.time()
#
# # Run the parallel computation
# set.seed(1900)  # For reproducibility
# gap_stat_parallel <- parallelClusGap(hrs_r11sampled, FUN = hclusCut, K.max = 20, B = 150)
#
# # Combine the results and compute the final gap statistic
# gap_stat <- list(Tab = apply(gap_stat_parallel, 2, mean))
#
# # Stop the parallel cluster
# stopCluster(cl)
#
# end_time <- Sys.time()
#
# # Print the time taken for the computation
# print(end_time - start_time)
#
# # Print the gap statistic results
# print(gap_stat)
# # Plot the gap statistic
# gapplot <- plot(gap_stat, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#
# # Plot the gap statistic
# gapplot <- plot(gap_stat, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#---------------------------

grp11 <- cutree(hc11, k = 15)
table(grp11)
# Create a data frame by adding the cluster assignments to your original data
final_data11 <- cbind(hrs_r11s, cluster = factor(grp11))
cluster_summaries11 <- by(final_data11, final_data11$cluster, summary)

# Capture the output of the cluster summaries
dend_data11 <- dendro_data(hc11)
ggdendrogram(dend_data11)
 dend11 <- as.dendrogram(hc11)
dend11 <- color_branches(dend11, k = 15)
plot(cut(dend11, h=30)$upper,
     main="Upper tree of cut at h=30")
require("igraph")
phylo11 <- fviz_dend(cut(dend11, h=30)$upper, k = 15, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggsave("phylo11.pdf", phylo11, dpi = 300, width = 10, height = 10)

grp11 <- cutree(hc11, k = 15)
table(grp11)



cluster_sizes7 <- as.data.frame(table(grp7))
names(cluster_sizes7) <- c("cluster", "size")
print(cluster_sizes7)
sink("cluster_sizes712.tex")
 stargazer(cluster_sizes7, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()
cluster_sizes11 <- as.data.frame(table(grp11))
names(cluster_sizes11) <- c("cluster", "size")
print(cluster_sizes11)
sink("cluster_sizes1112.tex")
 stargazer(cluster_sizes11, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()
# Capture the output of the cluster summaries
capture.output(cluster_summaries7, file = "cluster_summaries_w7.txt")
capture.output(cluster_summaries11, file = "cluster_summaries_w11.txt")

