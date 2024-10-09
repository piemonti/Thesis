remove(list=ls())
# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(sharp)
library(tabulator)
library(cluster)

# Set working directory
setwd("C:/Users/stud1/Documents/SHARE")



#-------------------------------------------------------------------------------
#                                 Clustering
#-------------------------------------------------------------------------------
w1_small <- read.csv("output/small_r1.csv")


set.seed(1900)  # for reproducibility
w1_small_sample <- w1_small %>%
  dplyr::sample_frac(.10)

w1_small_dat <- as.data.frame(w1_small_sample)
# Select the variables you want to cluster by
w1_smaller <- w1_small_dat[, c("r1morbi", "raeducl", "r1iearn_constant", "r1hownrnt", "r1agey_class" )]

# Remove NA values
w1_smaller <- na.omit(w1_smaller)
w1_smaller <- as.data.frame(w1_smaller)
#Perform consensus clustering
stab_clust <- Clustering(xdata = w1_smaller)
Clusters(stab_clust)

ggplot(w1_smaller, aes(x = w1morbi, y = w1agey_classlab, color = as.factor(Clusters(stab_clust)))) + 
  geom_point() +
  labs(color = "Cluster")

cluster_means <- aggregate(w1_smaller, list(Clusters(stab_clust)), mean)

cluster_means

sil_score <- silhouette(Clusters(stab_clust), dist(w1_smaller))
mean(sil_score[, 3])

stab_reg <- VariableSelection(xdata = w1_smaller, ydata = w1morbi)
SelectedVariables(stab_reg)





stab_ggm <- GraphicalModel(xdata = w1_smaller)
Adjacency(stab_ggm)
CalibrationPlot(stab_ggm)
plot(stab_ggm)

# 
# w1morbiomit <- df_cluster$w1morbi
# 
# stab_reg1 <- VariableSelection(xdata = df_cluster, ydata = df_cluster$w1morbi)
# SelectedVariables(stab_reg1)
# CalibrationPlot(stab_reg1)
# plot(stab_reg1)
# 
# stab_reg2 <- VariableSelection(xdata = health_int, ydata = db$r6agey)
# SelectedVariables(stab_reg2)
# CalibrationPlot(stab_reg2)
# plot(stab_reg2)
# 
# 
# summary(stab_ggm)
# summary(stab_reg1)
# summary(stab_reg2)