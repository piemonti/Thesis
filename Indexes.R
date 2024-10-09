remove(list = ls())
gc()

library(fmsb)
library(haven)
library(tidyr)
library(reshape2)
library(ggplot2)
library(kableExtra)
library(stargazer)
library(dplyr)
library(RColorBrewer)
library(ggridges)
library(paletteer)

# Function to detect operating system and adjust file paths accordingly
get_os <- function() {
  if (Sys.info()[['sysname']] == 'Windows') {
    base_path <- "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/"
  } else if (Sys.info()[['sysname']] == 'Darwin') {  # 'Darwin' is the sysname for macOS
    base_path <- "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/"
  } else {
    stop("Unsupported operating system")
  }
  return(base_path)
}

# Set base path
base_path <- get_os()
index_data<- read.csv(paste0(base_path, "Clustering/Analysis/indexdata.csv"))

colors <- paletteer_c("grDevices::SunsetDark", 12)
point_shapes <- c(16, 17, 18, 19, 15, 7, 8, 3, 4, 5, 6, 9)  # Different point shapes


# Reshape data for CH Index plot
ch_data <- index_data[, c("Dataset", "CH_Index_K_8", "CH_Index_K_50", "CH_Index_K_100", "CH_Index_K_150")]
ch_data_melted <- melt(ch_data, id.vars = "Dataset", 
                       variable.name = "K", value.name = "CH_Index")

# Reshape K column to show numeric values
ch_data_melted$K <- as.numeric(gsub("CH_Index_K_", "", ch_data_melted$K))


# Reshape data for Silhouette Index plot
sil_data <- index_data[, c("Dataset", "Silhouette_K_8", "Silhouette_K_50", "Silhouette_K_100", "Silhouette_K_150")]
sil_data_melted <- melt(sil_data, id.vars = "Dataset", 
                        variable.name = "K", value.name = "Silhouette_Index")

# Reshape K column to show numeric values
sil_data_melted$K <- as.numeric(gsub("Silhouette_K_", "", sil_data_melted$K))


# Set the output path for the analysis folder
output_path <- paste0(base_path, "Clustering/Analysis/")

# Plot for CH Index with color palette and point shapes
ch_index_plot <- ggplot(ch_data_melted, aes(x = K, y = CH_Index, color = Dataset, shape = Dataset)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +  # Adjust point size if needed
  scale_color_manual(values = colors) +
  scale_shape_manual(values = point_shapes) +
  labs(title = "CH Index by Dataset for different K values", x = "K", y = "CH Index") +
  theme_minimal()

# Save the CH Index plot
ggsave(paste0(output_path, "CH_Index_plot.pdf"), plot = ch_index_plot, width = 8, height = 6)

# Plot for Silhouette Index with color palette and point shapes
silhouette_index_plot <- ggplot(sil_data_melted, aes(x = K, y = Silhouette_Index, color = Dataset, shape = Dataset)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) +  # Adjust point size if needed
  scale_color_manual(values = colors) +
  scale_shape_manual(values = point_shapes) +
  labs(title = "Silhouette Index by Dataset for different K values", x = "K", y = "Silhouette Index") +
  theme_minimal()

# Save the Silhouette Index plot
ggsave(paste0(output_path, "Silhouette_Index_plot.pdf"), plot = silhouette_index_plot, width = 8, height = 6)
 