remove(list = ls())
# Set working directory WINHOME
# setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE")

# Set working directory MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/SHARE")
# Set working directory
setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")

library(haven)
library(factoextra)
library(cluster)
library(FactoMineR)
library(corrplot)
share <- read_dta("SHARE/output/share_cross_section.dta")

share_r1 <- share[, c("r1_low_income", "r1_middle_income", "raeducl", "r1bmi",
                      "r1diabe", "r1vgactx")]


share_r1 <- na.omit(share_r1)
share_r1_scaled <- scale(share_r1)
res.pca <- PCA(share_r1, scale.unit = TRUE, ncp = 5, graph = TRUE)
eig.val <- get_eigenvalue(res.pca)
eig.val
# Eigenvalues can be used to determine the number of principal components to retain after PCA (Kaiser 1961):
#   
# An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables in standardized data. This is commonly used as a cutoff point for which PCs are retained. This holds true only when the data are standardized.
# 
# You can also limit the number of component to that number that accounts for a certain fraction of the total variance. For example, if you are satisfied with 70% of the total variance explained then use the number of components to achieve that.

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30))
#From the plot above, we might want to stop at the fifth principal component. 72% of the information (variances) contained in the data are retained by the first five principal components. 

var <- get_pca_var(res.pca)
var
fviz_pca_var(res.pca, col.var = "aquamarine3")

#The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates) . You can access to the cos2 as follow:
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)
#r1bmi and r1diabe are not well represented, small cos2
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#77B0AA", "#135D66", "#003C43"), 
             repel = TRUE # Avoid text overlapping
)


corrplot(var$contrib, is.corr=FALSE)   
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#77B0AA", "#135D66", "#003C43")
)


# fviz_nbclust(share_r1, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res <- kmeans(share_r1, 4, nstart = 25)

print(km.res)
aggregate(share_r1, by=list(cluster=km.res$cluster), mean)
fviz_cluster(km.res, data = share_r1,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), ellipse.type = "euclid", # Concentration ellipse star.plot = TRUE, # Add segments from centroids to items repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)







#wave 6

share_r6 <- share[, c("r6_low_income", "r6_middle_income", "raeducl", "r6bmi",
                      "r6diabe", "r6vgactx")]


share_r6 <- na.omit(share_r6)
share_r6_scaled <- scale(share_r6)
res.pca <- PCA(share_r6, scale.unit = TRUE, ncp = 5, graph = TRUE)
eig.val <- get_eigenvalue(res.pca)
eig.val
# Eigenvalues can be used to determine the number of principal components to retain after PCA (Kaiser 1961):
#   
# An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables in standardized data. This is commonly used as a cutoff point for which PCs are retained. This holds true only when the data are standardized.
# 
# You can also limit the number of component to that number that accounts for a certain fraction of the total variance. For example, if you are satisfied with 70% of the total variance explained then use the number of components to achieve that.

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30))
#From the plot above, we might want to stop at the fifth principal component. 72% of the information (variances) contained in the data are retained by the first five principal components. 

var <- get_pca_var(res.pca)
var
fviz_pca_var(res.pca, col.var = "aquamarine3")

#The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates) . You can access to the cos2 as follow:
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)
#r1bmi and r1diabe are not well represented, small cos2
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#77B0AA", "#135D66", "#003C43"), 
             repel = TRUE # Avoid text overlapping
)


corrplot(var$contrib, is.corr=FALSE)   
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#77B0AA", "#135D66", "#003C43")
)


# fviz_nbclust(share_r6, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res <- kmeans(share_r6, 4, nstart = 25)

print(km.res)
aggregate(share_r6, by=list(cluster=km.res$cluster), mean)
fviz_cluster(km.res, data = share_r6,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), ellipse.type = "euclid", # Concentration ellipse star.plot = TRUE, # Add segments from centroids to items repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



#share with multiple waves

share_waves <- read_dta("SHARE/output/share_small.dta")

share_waves <- share[, c("r1_low_income", "r1_middle_income", "raeducl", "r1bmi",
                      "r1diabe", "r1vgactx", "r6_low_income", "r6_middle_income", "r6bmi",
                      "r6diabe", "r6vgactx")]


share_waves <- na.omit(share_waves)
share_waves_scaled <- scale(share_waves)
res.pca <- PCA(share_waves, scale.unit = TRUE, ncp = 5, graph = TRUE)
eig.val <- get_eigenvalue(res.pca)
eig.val
# Eigenvalues can be used to determine the number of principal components to retain after PCA (Kaiser 1961):
#   
# An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables in standardized data. This is commonly used as a cutoff point for which PCs are retained. This holds true only when the data are standardized.
# 
# You can also limit the number of component to that number that accounts for a certain fraction of the total variance. For example, if you are satisfied with 70% of the total variance explained then use the number of components to achieve that.

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30))
#From the plot above, we might want to stop at the fifth principal component. 72% of the information (variances) contained in the data are retained by the first five principal components. 

var <- get_pca_var(res.pca)
var
fviz_pca_var(res.pca, col.var = "aquamarine3")

#The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates) . You can access to the cos2 as follow:
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)
#r1bmi and r1diabe are not well represented, small cos2
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#77B0AA", "#135D66", "#003C43"), 
             repel = TRUE # Avoid text overlapping
)


corrplot(var$contrib, is.corr=FALSE)   
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#77B0AA", "#135D66", "#003C43")
)


#fviz_nbclust(share_waves, kmeans, method = "wss") +
  #geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res <- kmeans(share_waves, 4, nstart = 25)

print(km.res)
aggregate(share_waves, by=list(cluster=km.res$cluster), mean)
fviz_cluster(km.res, data = share_waves,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), ellipse.type = "euclid", # Concentration ellipse star.plot = TRUE, # Add segments from centroids to items repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



