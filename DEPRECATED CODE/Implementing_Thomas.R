remove(list = ls())
# Set working directory WINHOME
# setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE")

# Set working directory MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/SHARE")
# Set working directory
setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")


# load libraries
library(haven)
library(data.table)
library(sharp)
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
library(ClustMAPDP);
library(expm);
library(matrixStats);
# read and manipulate dataset
df <- read_dta("SHARE/output/share_small.dta")
# set.seed(1900)  # for reproducibility
# df_sample <- df %>%
#   dplyr::sample_frac(.10)


#-------------------------------------------------------------------------------
#                             KMeans clustering
#-------------------------------------------------------------------------------
r1_df_small <- df[, c(
    "r1vgactx", "r1hibpe", "r1lunge",
    "r1diabe", "r1stroke", "r1bmi", "r1hearte", "r1smokev","r1_drink_NO","r1_drink_MDW", "r1_drink_EDW", "r1iearn_constant")]
r1_df_small <- na.omit(r1_df_small)
r1_df_scaled <- scale(r1_df_small)
r1_df_scaled <- as.data.frame(r1_df_scaled)
nb <- NbClust(r1_df_scaled, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
pca <- prcomp(r1_df_small, center = TRUE, scale. = TRUE)
df_pca <- pca$x[, 1:10] # Use the first 10 principal components
plot(df_pca)
#problema, gap mi dice k = 10
fviz_nbclust(r1_df_scaled, kmeans,
             method = "gap_stat")

# K-means clustering with K = 2
km.res <- eclust(r1_df_scaled, "kmeans", k = 6,
                 nstart = 25, graph = TRUE)
# k-means group number of each observation
km.res$cluster

# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", frame.type = "norm")


# # K-means clustering with K = 10
# km.res <- eclust(r1_df_scaled, "kmeans", k = 10,
#                  nstart = 25, graph = TRUE)
# # k-means group number of each observation
# km.res$cluster
# 
# # Visualize k-means clusters
# fviz_cluster(km.res, geom = "point", frame.type = "norm")
#################################
#better results with k = 2
sil <- silhouette(km.res$cluster, dist(r1_df_scaled))


# Summary of silhouette analysis
si.sum <- summary(sil)
# Average silhouette width of each cluster
si.sum$clus.avg.widths

# The total average (mean of all individual silhouette widths)
si.sum$avg.width
# The size of each clusters
si.sum$clus.sizes
# Default plot
fviz_silhouette(km.res)

# An average silhouette width of 0.13 suggests that, on average, the clusters
# in the dataset are somewhat distinguishable, but there may be some overlap or
# ambiguity in cluster assignments. It's not particularly high, but it's also not 
# too low, indicating a moderate level of cluster separation.


# Compute pairwise-distance matrices
dd <- dist(r1_df_scaled, method ="euclidean")
# Statistics for k-means clustering
km_stats <- cluster.stats(dd,  km.res$cluster)
# (k-means) within clusters sum of squares
km_stats$within.cluster.ss

# Display all statistics
km_stats




#-------------------------------------------------------------------------------
#                             Map-DP clustering
#-------------------------------------------------------------------------------
r1_alt <- df[, c(
  "r1cancre", "r1hibpe", "r1lunge",
  "r1diabe", "r1stroke", "r1bmi", "r1hearte", "raeducl", "r1iearn_constant",
  "r1hownrnt", "r1agey", "r1gripsum", "r1smoken", "r1vgactx"
)]
r1_alt <- na.omit(r1_alt)
r1_alt <- scale(r1_alt)
r1_alts <- as.data.frame(r1_alt)
pca <- prcomp(r1_alts, center = TRUE, scale. = TRUE)
df_pca <- pca$x[, 1:10] # Use the first 10 principal components
plot(df_pca)

# Create X matrix with specified variables as a matrix
X <- t(as.matrix(r1_alts[, c("r1cancre", "r1hibpe", "r1lunge", "r1diabe", "r1stroke", "r1bmi", "r1hearte", "r1iearn_constant",
                             "r1hownrnt", "r1agey", "r1gripsum")]))

# Create Z matrix with the specified variable as a matrix
Z <- as.matrix(r1_alts["raeducl"])
Z <- as.array(Z)
N <- 5542

# Set up Normal-Wishart MAP-DP prior parameters
N0 <- 1;                           # Prior count (concentration parameter)
m0 <- rowMeans(X);                     # Normal-Wishart prior mean
a0 <- 10;                            # Normal-Wishart prior scale
c0 <- 10/N;                          # Normal-Wishart prior degrees of freedom
B0 <- diag(1./(0.05*rowVars(X)));    # Normal-Wishart prior precision
start.time <- Sys.time()
Rprof(tmp <- tempfile())
r <- clustMapDP(X,N0,m0,a0,c0,B0);
Rprof()
summaryRprof(tmp)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

par(mfrow=c(1,3));
plot(X[1,],X[2,],col=Z, main = "Input")
plot(X[1,],X[2,],col=r$z, main = paste("MAP-DP (",r$K,")"))
km <- kmeans(x=t(X),c=r$K,iter.max = 20)
plot(X[1,],X[2,],col=km$cluster, main = "kMeans")





















# Set up Normal-Wishart MAP-DP prior parameters
N0 <- 1;                           # Prior count (concentration parameter)
m0 <- rowMeans(r1_m);                     # Normal-Wishart prior mean
a0 <- 10;                            # Normal-Wishart prior scale
c0 <- 10/N;                          # Normal-Wishart prior degrees of freedom
B0 <- diag(1./(0.05*rowVars(r1_m)));    # Normal-Wishart prior precision
start.time <- Sys.time()
Rprof(tmp <- tempfile())
r <- clustMapDP(r1_m,N0,m0,a0,c0,B0);
Rprof()
summaryRprof(tmp)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

par(mfrow=c(1,3));
plot(r1_m[1,],r1_m[2,],col=Z, main = "Input")
plot(r1_m[1,],r1_m[2,],col=r$z, main = paste("MAP-DP (",r$K,")"))
km <- kmeans(x=t(r1_m),c=r$K,iter.max = 20)
plot(r1_m[1,],r1_m[2,],col=km$cluster, main = "kMeans")








X <- t(as.matrix(r1_df_scaled))

# Set up Normal-Wishart MAP-DP prior parameters
N0 <- 1                           # Prior count (concentration parameter)
m0 <- colMeans(X)     # Normal-Wishart prior mean
a0 <- 10                          # Normal-Wishart prior scale
c0 <- 10 / N    # Normal-Wishart prior degrees of freedom
B0 <- solve(diag(1 / (0.05 * colVars(X))))  # Normal-Wishart prior precision

# Call clustMapDP function
start.time <- Sys.time()
Rprof(tmp <- tempfile())

# Call the clustMapDP function
r <- clustMapDP(X, N0, m0, a0, c0, B0)

Rprof()
summaryRprof(tmp)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# Plot the results
par(mfrow = c(1, 3))
plot(X[1, ], X[2, ], col = r$z, main = paste("MAP-DP (", r$K, ")"))

# Perform kMeans for comparison
km <- kmeans(X, centers = r$K, iter.max = 20)
plot(X[1, ], X[2, ], col = km$cluster, main = "kMeans")







