remove(list=ls())
# Load required libraries
library(dplyr)
library(haven)
library(sharp)
library(psych)
library(tidyr)
library(dbscan)
library(mice)
library(ggplot2)
library(factoextra)
library(cluster)


#
# # # Define the country codes to retain
# desired_countries <- c("IT", "FR", "ES", "DE", "SE", "DK", "FI")
# # Filter the dataset based on the MERGEID variable and desired country codes
#  sharedata_filtered <- sharedata %>%
#    filter(substr(sharedata$mergeid, 1, 2) %in% desired_countries)
#
# # View the filtered dataset
#  head(sharedata_filtered)
#
# # Extract respondent IDs
# respondent_ids <- unique(substring(names(your_data), 1, 2))
#
# sharedata_filtered$isalive <- rowSums(sharedata_filtered[, c("r1iwstat", "r6iwstat")], na.rm = TRUE)
#
# # Subset the data for individuals with isalive = 2
# db <- sharedata_filtered[sharedata_filtered$isalive == 2, ]
#
# # Print or use the subset_data with age_clusters
# write.csv("db.csv")

# -Filtered dataset, db has data for all individuals that were alive in the first and the sixth waves ----------

# Read the CSV file
db <- read.csv("db.csv")
# Define the vector of variable names
varNames <- c("r1arthre", "radiagarthr", "r1cancre", "radiagcancr", "r1diabe", "r1rxdiab", "radiagdiab", "r1hearte", "r1rxheart", "radiagheart", "r1hibpe", "r1rxhibp", "radiaghibp", "r1hchole", "r1rxhchol", "r1lunge", "r1rxlung", "radiaglung", "r1parkine", "radiagparkin", "r1stroke", "radiagstrok",
"r6arthre", "radiagarthr", "r6cancre", "r6diabe", "r6rxdiab", "r6hearte", "r6rxheart", "r6hibpe", "r6rxhibp", "r6hchole", "r6rxhchol", "r6lunge", "r6rxlung", "r6parkine", "r6stroke")

# Check if the variables exist in the data frame
varNames %in% names(db)
health_vars <- db[, c( "r1cancre", "r1diabe", "r1rxdiab", "r1hearte", "r1rxheart", "r1hibpe", "r1rxhibp", "r1lunge", "r1rxlung", "r1stroke",  "r6cancre", "r6diabe", "r6rxdiab", "r6hearte", "r6rxheart", "r6hibpe", "r6rxhibp", "r6lunge", "r6rxlung", "r6stroke")]
#age_vars <- db[,c("radiagarthr","radiagcancr", "radiagarthr", "radiagdiab", "radiagheart", "radiaghibp", "radiaglung", "radiagparkin", "radiagstrok", "radiagarthr")]
age_vars <- db[,c("r1agey", "r6agey")]

# Calculate the total number of elements in the dataset
total_elements <- prod(dim(health_vars))

# Calculate the number of missing values
num_missing_values <- sum(is.na(health_vars))

# Calculate the proportion of missing values
missing_proportion <- num_missing_values / total_elements
# Check the proportion of missing values in the dataframe, roughly 24%
# Print the proportion
print(missing_proportion)

# -DBSCAN with imputation
health_new <- health_vars

# Loop through each column in the data frame
for(col in colnames(health_new)){
  # Convert the column to a factor
  health_new[[col]] <- as.factor(health_new[[col]])
}

# Perform the imputation
tempData <- mice(health_new, method='logreg', m=5)

# Save the completed data
completedhealth <- complete(tempData)
health_int <- completedhealth

# Loop through each column in the data frame
for(col in colnames(health_int)){
  # Convert the column to an integer
  health_int[[col]] <- as.integer(as.character(health_int[[col]]))
}
# Perfrorm DBSCAN clustering
dbscan_result <- dbscan(health_int, eps = 0.5, minPts = 5)

plot(health_int[,1], age_vars[,2],
     col=dbscan_result$cluster + 1L, # Adding 1 to make noise points black
     main="DBSCAN Clustering Results",
     xlab="health variables", ylab="age variables")

# Highlighting the noise points in red
points(health_int[dbscan_result$cluster == 0, 1], health_int[dbscan_result$cluster == 0, 2],
       col='red', pch = 3)

#-kmeans clustering-
# Perform the clustering
clust_result <- kmeans(health_int, centers = 5)

# Print the result
print(clust_result)
summary(clust_result)
PAC <- function(cm, lower, upper) {
  ambiguous <- cm[cm > lower & cm < upper]
  pac_score <- length(ambiguous) / length(cm)
  return(pac_score)
}

# Convert cluster assignment to binary format
binary_matrix <- table(clust_result$cluster, 1:nrow(health_int))

# Compute consensus matrix
cm <- binary_matrix %*% t(binary_matrix) / ncol(binary_matrix)

# Define bounds for "ambiguous clustering"
lower_bound <- 0.1
upper_bound <- 0.9

# Compute the PAC score
pac_score <- PAC(cm, lower = lower_bound, upper = upper_bound)

# Print the PAC score
print(pac_score)
# # Use PCA for dimensionality reduction
pca_result <- prcomp(health_int)
#
# Extract the first two principal components
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]

# Combine the principal components with the cluster assignments
clustered_data <- data.frame(PC1 = pc1, PC2 = pc2, Cluster = as.factor(clust_result$cluster))

# Plot the clusters
ggplot(clustered_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +  # Reduce the size of dots and set transparency
  scale_color_brewer(palette = "Spectral") +  # Use a color map
  ggtitle("K-means Clustering Results") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal()


#https://mlforanalytics.com/2020/05/23/pam-clustering-using-r/
multi.hist(health_int,nrow = 3, ncol=2,density=TRUE,freq=FALSE,bcol="lightblue",
      dcol= c("red","blue"),dlty=c("solid", "dotted"),
      main=colnames(health_int))
#all variables are bimodal
scaleddata <- scale(health_int)
fviz_nbclust(scaleddata, pam, method ="silhouette")+theme_minimal()
pamResult <-pam(scaleddata, k = 2)
pamResult
health_intcluster <- pamResult$cluster
head(health_intcluster)
pamResult$medoids
pamResult$clustering
fviz_cluster(pamResult,
             palette = c("#007892","#D9455F"),
             ellipse.type = "confidence",
             repel = TRUE,
             pointsize = 1.5,
             shape = 19,
             geom = "point",
             ggtheme = theme_minimal())
fviz_cluster(pamResult,
             palette = c("#007892","#D9455F"),
             ellipse.type = "convex",
             repel = TRUE,
             pointsize = 1.5,
             shape = 19,
             geom = "point",
             ggtheme = theme_minimal())

pamResult <-pam(scaleddata, k = 10)
pamResult
health_intcluster <- pamResult$cluster
head(health_int)
pamResult$medoids
pamResult$clustering
fviz_cluster(pamResult,
             palette = c("#007892","#D9455F", "#3E4F91", "#A67C52", "#D13F31", "#7D8F0A", "#1E90FF", "#FF4500", "#2E8B57", "#ADFF2F"),
             ellipse.type = "confidence",
             repel = TRUE,
             pointsize = 1.5,
             shape = 19,
             geom = "point",
             ggtheme = theme_minimal())
fviz_cluster(pamResult,
             palette = c("#007892","#D9455F", "#3E4F91", "#A67C52", "#D13F31", "#7D8F0A", "#1E90FF", "#FF4500", "#2E8B57", "#ADFF2F"),
             ellipse.type = "convex",
             repel = TRUE,
             pointsize = 1.5,
             shape = 19,
             geom = "point",
             ggtheme = theme_minimal())
# preparing age_vars for sharp
total_elements <- prod(dim(age_vars))


# Calculate the number of missing values
num_missing_values <- sum(is.na(age_vars))

# Calculate the proportion of missing values
missing_proportion <- num_missing_values / total_elements

print(missing_proportion)

# Loop through each column in the data frame
for(col in colnames(age_vars)){
  # Convert the column to an integer
  age_vars[[col]] <- as.integer(as.character(age_vars[[col]]))
}
#Error (SHARE.R#120): vector memory exhausted (limit reached?)
# stab_clust <- Clustering(xdata = health_int)
# Clusters(stab_clust)

stab_ggm <- GraphicalModel(xdata = health_int)
Adjacency(stab_ggm)
CalibrationPlot(stab_ggm)
plot(stab_ggm)

stab_reg1 <- VariableSelection(xdata = health_int, ydata = db$r1agey)
SelectedVariables(stab_reg1)
CalibrationPlot(stab_reg1)
plot(stab_reg1)

stab_reg2 <- VariableSelection(xdata = health_int, ydata = db$r6agey)
SelectedVariables(stab_reg2)
CalibrationPlot(stab_reg2)
plot(stab_reg2)


summary(stab_ggm)
summary(stab_reg1)
summary(stab_reg2)