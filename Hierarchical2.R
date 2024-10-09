remove(list = ls())

# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(stargazer)
library(reshape2)
library(tabulator)
library(ineq)
library(treeio)
library(ggtree)
library(gridExtra)
library(factoextra)
library(dendextend)
library(cluster)
library(sharp)
#MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA")
#WIN HOME
#setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")
#SERVER
setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")
load("HClust.RData")
#read datasets
hrs <- read_dta("HRS/output/hrs_small.dta")
share <- read_dta("SHARE/output/share_small.dta")
share_w1 <- read_dta("SHARE/output/share_w1.dta")
share_w6 <- read_dta("SHARE/output/share_w6.dta")
hrs_w7 <- read_dta("HRS/output/hrs_w7.dta")
hrs_w11 <- read_dta("HRS/output/hrs_w11.dta")


#-------------- SHARE ----------------
#------------- Wave 1 ----------------
share_r1 <- share_w1[, c("h1itot_constant", "low_educ", "secondary_educ", "tertiary_educ","r1bmi",
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


grp <- cutree(hc1, k = 8)
table(grp)
# Create a data frame by adding the cluster assignments to your original data
final_data <- cbind(share_r1s, cluster = factor(grp))
cluster_summaries <- by(final_data, final_data$cluster, summary)

# Capture the output of the cluster summaries

dend <- as.dendrogram(hc1)

jco_colors <- c("#EFC000FF", "#0073C2FF", "#868686FF", "#CD534CFF", 
                "#7AA6DCFF", "#003C67FF", "#8F7700FF", "#A73030FF")

dend <- color_branches(dend, k = 8, col = jco_colors)

t1 <- plot(cut(dend, h=30)$upper,
     main="Upper tree of cut at h=30")
ggsave("treew1.pdf", t1, dpi = 300, width = 10, height = 10)

# Calculate the size of each cluster (number of observations per branch)
cluster_sizes <- as.data.frame(table(grp))
names(cluster_sizes) <- c("cluster", "size")
print(cluster_sizes)
sink("cluster_sizes16.tex")
clsize1 <- stargazer(cluster_sizes, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()

require("igraph")
prova <- fviz_dend(cut(dend, h=30)$upper, k = 6, k_colors = "jco",
                   type = "phylogenic", repel = TRUE,
                   color_labels_by_k = TRUE, rect = TRUE)

ggsave("phylo16.pdf", prova, dpi = 300, width = 10, height = 10)

# Cut the dendrogram at height h=30
cut_height <- 30
clusters_at_cut <- cutree(hc1, h = cut_height)

# Create a data frame with the cluster assignments
cluster_assignments <- data.frame(cluster = clusters_at_cut)

# Calculate the size of each cluster
cluster_sizes_at_cut <- as.data.frame(table(cluster_assignments$cluster))
names(cluster_sizes_at_cut) <- c("cluster", "size")

# Print the cluster sizes
print(cluster_sizes_at_cut)

# Save the cluster sizes to a LaTeX file using stargazer without using sink
latex_content <- stargazer(cluster_sizes_at_cut, type = "latex", title = "Cluster Sizes at h=30", summary = FALSE)
cat(latex_content, file = "nodes16.tex")
#------------- Wave 6 ----------------

share_r6 <- share_w6[, c("h6ittot", "low_educ", "secondary_educ", "tertiary_educ","r6bmi",
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
# gap_stat2 <- clusGap(share_r6_scaled, FUN = hclusCut, K.max = 6, B = 60)
#
# # Print the gap statistic results
# print(gap_stat2)
# # Plot the gap statistic
# plot(gap_stat2, main = "Gap Statistic for Hierarchical Clustering")
# abline(v = maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), col = "red", lty = 2)
#


grp6 <- cutree(hc6, k = 6)
table(grp6)
# Create a data frame by adding the cluster assignments to your original data
final_data6 <- cbind(share_r6s, cluster = factor(grp6))
cluster_summaries6 <- by(final_data6, final_data6$cluster, summary)

# Capture the output of the cluster summaries
#dend_data6 <- dendro_data(hc6)
#ggdendrogram(dend_data6)
dend6 <- as.dendrogram(hc6)
dend6 <- color_branches(dend6, k = 6, col = jco_colors)

t6 <- plot(cut(dend6, h=30)$upper,
     main="Upper tree of cut at h=30")
ggsave("treew6.pdf", t6, dpi = 300, width = 10, height = 10)
require("igraph")
phylo6 <- fviz_dend(cut(dend6, h=30)$upper, k = 6, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggsave("phylo66.pdf", phylo6, dpi = 300, width = 10, height = 10)

grp6 <- cutree(hc6, k = 6)
table(grp6)
# Create a data frame by adding the cluster assignments to your original data
final_data6 <- cbind(share_r6s, cluster = factor(grp6))

cluster_summaries6 <- by(final_data6, final_data6$cluster, summary)

cluster_sizes6 <- as.data.frame(table(grp6))
names(cluster_sizes6) <- c("cluster", "size")
print(cluster_sizes6)
sink("cluster_sizes66.tex")
 stargazer(cluster_sizes6, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()
capture.output(cluster_summaries, file = "cluster_summaries_w16.txt")
capture.output(cluster_summaries6, file = "cluster_summaries_w66.txt")

clusters_at_cut6 <- cutree(hc6, h = cut_height)

# Create a data frame with the cluster assignments
cluster_assignments6 <- data.frame(cluster = clusters_at_cut6)

# Calculate the size of each cluster
cluster_sizes_at_cut6 <- as.data.frame(table(cluster_assignments6$cluster))
names(cluster_sizes_at_cut6) <- c("node", "size")

# Print the cluster sizes
print(cluster_sizes_at_cut6)

# Save the cluster sizes to a LaTeX file using stargazer without using sink
latex_content6 <- stargazer(cluster_sizes_at_cut6, type = "latex", title = "Cluster Sizes at h=30", summary = FALSE)
cat(latex_content6, file = "nodes66.tex")

#-------------- HRS -------------------
#------------- Wave 7 -----------------
hrs_r7 <- hrs_w7[, c("r7iearn_constant", "low_educ", "secondary_educ", "tertiary_educ","r7bmi",
                      "r7_risky","r7no_phys_act", "r7low_phys_act", "r7good_phys_act", "r7agey_e", "r7_morbi_2", "r7_morbi_3_plus", "r7_healthy",
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
# gap_stat <- clusGap(hrs_r7sampled, FUN = hclusCut, K.max = 20, B = 60)
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
# gap_stat_parallel <- parallelClusGap(hrs_r7sampled, FUN = hclusCut, K.max = 20, B = 60)
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

grp7 <- cutree(hc7, k = 6)
table(grp7)
# Create a data frame by adding the cluster assignments to your original data
final_data7 <- cbind(hrs_r7s, cluster = factor(grp7))
cluster_summaries7 <- by(final_data7, final_data7$cluster, summary)

# Capture the output of the cluster summaries
#dend_data7 <- dendro_data(hc7)
#ggdendrogram(dend_data7)
 dend7 <- as.dendrogram(hc7)
dend7 <- color_branches(dend7, k = 6)
plot(cut(dend7, h=30)$upper,
     main="Upper tree of cut at h=30")
require("igraph")
phylo7 <- fviz_dend(cut(dend7, h=30)$upper, k = 6, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggsave("phylo76.pdf", phylo7, dpi = 300, width = 10, height = 10)

grp7 <- cutree(hc7, k = 6)
table(grp7)

clusters_at_cut7 <- cutree(hc7, h = cut_height)

# Create a data frame with the cluster assignments
cluster_assignments7 <- data.frame(cluster = clusters_at_cut7)

# Calculate the size of each cluster
cluster_sizes_at_cut7 <- as.data.frame(table(cluster_assignments7$cluster))
names(cluster_sizes_at_cut7) <- c("node", "size")

# Print the cluster sizes
print(cluster_sizes_at_cut7)

# Save the cluster sizes to a LaTeX file using stargazer without using sink
latex_content7 <- stargazer(cluster_sizes_at_cut7, type = "latex", title = "Cluster Sizes at h=30", summary = FALSE)
cat(latex_content6, file = "nodes76.tex")
#------------- Wave 11 ----------------

hrs_r11 <- hrs_w11[, c("r11iearn_constant", "low_educ", "secondary_educ", "tertiary_educ","r11bmi",
                      "r11_risky", "r11no_phys_act", "r11low_phys_act", "r11good_phys_act", "r11agey_e", "r11_morbi_2", "r11_morbi_3_plus", "r11_healthy",
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
# gap_stat <- clusGap(hrs_r11sampled, FUN = hclusCut, K.max = 20, B = 60)
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
# gap_stat_parallel <- parallelClusGap(hrs_r11sampled, FUN = hclusCut, K.max = 20, B = 60)
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

grp11 <- cutree(hc11, k = 6)
table(grp11)
# Create a data frame by adding the cluster assignments to your original data
final_data11 <- cbind(hrs_r11s, cluster = factor(grp11))
cluster_summaries11 <- by(final_data11, final_data11$cluster, summary)

# Capture the output of the cluster summaries
#dend_data11 <- dendro_data(hc11)
#ggdendrogram(dend_data11)
 dend11 <- as.dendrogram(hc11)
dend11 <- color_branches(dend11, k = 6)
plot(cut(dend11, h=30)$upper,
     main="Upper tree of cut at h=30")
require("igraph")
phylo11 <- fviz_dend(cut(dend11, h=30)$upper, k = 6, k_colors = "jco",
          type = "phylogenic", repel = TRUE)
ggsave("phylo116.pdf", phylo11, dpi = 300, width = 10, height = 10)

grp11 <- cutree(hc11, k = 6)
table(grp11)
capture.output(cluster_summaries7, file = "cluster_summaries_w76.txt")
capture.output(cluster_summaries11, file = "cluster_summaries_w116.txt")


cluster_sizes7 <- as.data.frame(table(grp7))
names(cluster_sizes7) <- c("cluster", "size")
print(cluster_sizes7)
sink("cluster_sizes76.tex")
 stargazer(cluster_sizes7, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()
cluster_sizes11 <- as.data.frame(table(grp11))
names(cluster_sizes11) <- c("cluster", "size")
print(cluster_sizes11)
sink("cluster_sizes116.tex")
 stargazer(cluster_sizes11, type = "latex", title = "Cluster Sizes", summary = FALSE)
sink()


clusters_at_cut11 <- cutree(hc11, h = cut_height)

# Create a data frame with the cluster assignments
cluster_assignments11 <- data.frame(cluster = clusters_at_cut11)

# Calculate the size of each cluster
cluster_sizes_at_cut11 <- as.data.frame(table(cluster_assignments11$cluster))
names(cluster_sizes_at_cut11) <- c("node", "size")

# Print the cluster sizes
print(cluster_sizes_at_cut11)

# Save the cluster sizes to a LaTeX file using stargazer without using sink
latex_content11 <- stargazer(cluster_sizes_at_cut11, type = "latex", title = "Cluster Sizes at h=30", summary = FALSE)
cat(latex_content6, file = "nodes116.tex")

#--------- cluster analyisis ----------

# Calculate the correlation matrix
cor_matrix1 <- cor(share_r1s, use = "complete.obs")
cor_matrix6 <- cor(share_r6s, use = "complete.obs")
cor_matrix7 <- cor(hrs_r7s, use = "complete.obs")
cor_matrix11 <- cor(hrs_r11s, use = "complete.obs")
# Ensuring to handle NA values appropriately
# Convert correlation matrices to data frames in long format
cor_data1 <- as.data.frame(as.table(cor_matrix1))
cor_data6 <- as.data.frame(as.table(cor_matrix6))
cor_data7 <- as.data.frame(as.table(cor_matrix7))
cor_data11 <- as.data.frame(as.table(cor_matrix11))

# Function to plot a correlation matrix
plot_cor_matrix <- function(cor_data, title) {
  ggplot(cor_data, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile() +  # Use geom_tile for heatmap squares
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank()) +
    ggtitle(title)  # Add a title for clarity
}

# Plot each correlation matrix
plot_cor_matrix(cor_data1, "Correlation Matrix 1")
plot_cor_matrix(cor_data6, "Correlation Matrix 6")
plot_cor_matrix(cor_data7, "Correlation Matrix 7")
plot_cor_matrix(cor_data11, "Correlation Matrix 11")

# Save the plots to files
ggsave("cor_matrix1.pdf", plot_cor_matrix(cor_data1, "Correlation Matrix 1"), width = 10, height = 10, dpi = 300)
ggsave("cor_matrix6.pdf", plot_cor_matrix(cor_data6, "Correlation Matrix 6"), width = 10, height = 10, dpi = 300)
ggsave("cor_matrix7.pdf", plot_cor_matrix(cor_data7, "Correlation Matrix 7"), width = 10, height = 10, dpi = 300)
ggsave("cor_matrix11.pdf", plot_cor_matrix(cor_data11, "Correlation Matrix 11"), width = 10, height = 10, dpi = 300)


#density plots for earnings and bmi Wave 1
# Create a list to store plots
plots <- list()

# Generate a density plot for earnings in each cluster
for (k in unique(final_data$cluster)) {
  p <- ggplot(final_data %>% filter(cluster == k), aes(x = r1earnings_constant)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("Earnings Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("earndens1.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Generate a density plot for earnings in each cluster
for (k in unique(final_data$cluster)) {
  p <- ggplot(final_data %>% filter(cluster == k), aes(x = r1bmi)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("BMI Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("bmidens1.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Calculate Gini index for each cluster
gini_indices <- final_data %>%
  group_by(cluster) %>%
  summarise(Gini_Index = ineq(r1earnings_constant, type = "Gini"),
            .groups = 'drop')  # drop the grouping

# Print the Gini indices
print(gini_indices)

# Save the Gini indices to a LaTeX file using stargazer without using sink
latex_content <- stargazer(gini_indices, type = "latex", title = "Gini Indices", summary = FALSE)


#density plots for earnings and bmi Wave 1
# Create a list to store plots
plots <- list()

# Generate a density plot for earnings in each cluster
for (k in unique(final_data6$cluster)) {
  p <- ggplot(final_data6 %>% filter(cluster == k), aes(x = r6earnings_constant)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("Earnings Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("earndens6.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Generate a density plot for earnings in each cluster
for (k in unique(final_data6$cluster)) {
  p <- ggplot(final_data6 %>% filter(cluster == k), aes(x = r6bmi)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("BMI Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("bmidens6.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Calculate Gini index for each cluster
gini_indices6 <- final_data6 %>%
  group_by(cluster) %>%
  summarise(Gini_Index = ineq(r6earnings_constant, type = "Gini"),
            .groups = 'drop')  # drop the grouping

# Print the Gini indices
print(gini_indices6)

# Save the Gini indices to a LaTeX file using stargazer without using sink
latex_content <- stargazer(gini_indices6, type = "latex", title = "Gini Indices", summary = FALSE)

#density plots for earnings and bmi Wave 7
# Create a list to store plots
plots <- list()

# Generate a density plot for earnings in each cluster
for (k in unique(final_data7$cluster)) {
  p <- ggplot(final_data7 %>% filter(cluster == k), aes(x = r7iearn_constant)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("Earnings Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("earndens7.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Generate a density plot for earnings in each cluster
for (k in unique(final_data7$cluster)) {
  p <- ggplot(final_data7 %>% filter(cluster == k), aes(x = r7bmi)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("BMI Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("bmidens7.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Calculate Gini index for each cluster
gini_indices7 <- final_data7 %>%
  group_by(cluster) %>%
  summarise(Gini_Index = ineq(r7iearn_constant, type = "Gini"),
            .groups = 'drop')  # drop the grouping

# Print the Gini indices
print(gini_indices7)

# Save the Gini indices to a LaTeX file using stargazer without using sink
latex_content <- stargazer(gini_indices7, type = "latex", title = "Gini Indices", summary = FALSE)

#density plots for earnings and bmi Wave 11
# Create a list to store plots
plots <- list()

# Generate a density plot for earnings in each cluster
for (k in unique(final_data11$cluster)) {
  p <- ggplot(final_data11 %>% filter(cluster == k), aes(x = r11iearn_constant)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("Earnings Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("earndens11.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Generate a density plot for earnings in each cluster
for (k in unique(final_data11$cluster)) {
  p <- ggplot(final_data11 %>% filter(cluster == k), aes(x = r11bmi)) +
    geom_density(fill = "#0073C2FF", alpha = 0.5) +
    labs(title = paste("BMI Density Plot - Cluster", k),
         x = "Earnings (scaled)",
         y = "Density") +
    theme_minimal()
  plots[[k]] <- p
}

# Arrange plots in a grid
plot_grid <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the plot grid to a file
ggsave("bmidens11.pdf", plot_grid, width = 20, height = 15, dpi = 300)

# Calculate Gini index for each cluster
gini_indices11 <- final_data11 %>%
  group_by(cluster) %>%
  summarise(Gini_Index = ineq(r11iearn_constant, type = "Gini"),
            .groups = 'drop')  # drop the grouping

# Print the Gini indices
print(gini_indices11)

# Save the Gini indices to a LaTeX file using stargazer without using sink
latex_content <- stargazer(gini_indices11, type = "latex", title = "Gini Indices", summary = FALSE)



dm <- dist(share_r1sm, method = "euclidean")


as.matrix(dm)[1:6, 1:6]

# Hierarchical clustering using Ward Linkage
hc1m <- hclust(dm, method = "ward.D2")
tree <- as.phylo(hc1m)
ggplot(tree, aes(x, y)) + geom_tree() + theme_tree()
ggtree(tree, layout="daylight", branch.length = 'none')




dend_cut <- cut(dend, h=30)$upper
dend6_cut <- cut(dend6, h=30)$upper

# Convert to dendrogram objects if they are not already
dend_cut <- as.dendrogram(dend_cut)
dend6_cut <- as.dendrogram(dend6_cut)

# Ensure labels are consistent across both dendrograms for proper matching in the tanglegram
labels_dend1 <- labels(dend_cut)
labels_dend6 <- labels(dend6_cut)
labels_dend <- labels_dend6[match(labels_dend1, labels_dend6)]
labels_dend 
dend_cut <- prune(dend, labels_dend)
dend6_cut <- prune(dend6, labels_dend)
# Create the tanglegram
tanglegram(dend_cut, dend6_cut, 
           main = "Tanglegram of Wave 1 and Wave 6",
           margin_inner = 10)  # Adjust margin_inner as necessary for label visibility
