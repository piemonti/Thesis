  remove(list = ls())
  gc()
  
  library(fmsb)
  library(haven)
  library(tidyr)
  library(ggplot2)
  library(fastDummies)
  library(reshape2)
  library(kableExtra)
  library(ggalluvial)
  library(dplyr)
  library(treemap)
  library(UpSetR)
  library(RColorBrewer)
  library(ggridges)
  library(grateful)
  library(grid)
  library(gridExtra)
  
  # Function to detect operating system and adjust file paths accordingly
  get_os <- function() {
    if (Sys.info()[['sysname']] == 'Windows') {
      base_path <- "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/"
    } else if (Sys.info()[['sysname']] == 'Darwin') {  # 'Darwin' is the sysname for macOS
      base_path <- "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/"
    } else {
      stop("Unsupported operating system")
    }
    return(base_path)
  }
  
  # Read datasets
  base_path <- get_os()
  
  share_w1 <- read_dta(paste0(base_path, "SHARE/output/merged_data1.dta"))
  share_w6 <- read_dta(paste0(base_path, "SHARE/output/merged_data6.dta"))
  hrs_w7 <- read_dta(paste0(base_path, "HRS/output/cluster_assignments/merged_data7.dta"))
  hrs_w12 <- read_dta(paste0(base_path, "HRS/output/cluster_assignments/merged_data12.dta"))
  elsa_w2 <- read_dta(paste0(base_path, "ELSA/UKDA-5050-stata/output/cluster_assignments/merged_data2.dta"))
  elsa_w8 <- read_dta(paste0(base_path, "ELSA/UKDA-5050-stata/output/cluster_assignments/merged_data8.dta"))
  mhas_w2 <- read_dta(paste0(base_path, "MHAS/output/cluster_assignments/merged_data2.dta"))
  mhas_w4 <- read_dta(paste0(base_path, "MHAS/output/cluster_assignments/merged_data4.dta"))
  klosa_w3 <- read_dta(paste0(base_path, "KLoSA/output/cluster_assignments/merged_data3.dta"))
  klosa_w8 <- read_dta(paste0(base_path, "KLoSA/output/cluster_assignments/merged_data8.dta"))
  charls_w1 <- read_dta(paste0(base_path, "CHARLS/output/cluster_assignments/merged_data1.dta"))
  charls_w4 <- read_dta(paste0(base_path, "CHARLS/output/cluster_assignments/merged_data4.dta"))
  
  share_w1_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/share_w1_healthmean.dta"))
  share_w6_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/share_w6_healthmean.dta"))
  hrs_w7_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/hrs_w7_healthmean.dta"))
  hrs_w12_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/hrs_w12_healthmean.dta"))
  elsa_w2_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/elsa_w2_healthmean.dta"))
  elsa_w8_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/elsa_w8_healthmean.dta"))
  klosa_w3_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/klosa_w3_healthmean.dta"))
  klosa_w8_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/klosa_w8_healthmean.dta"))
  charls_w1_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/charls_w1_healthmean.dta"))
  charls_w4_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/charls_w4_healthmean.dta"))
  mhas_w2_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/mhas_w2_healthmean.dta"))
  mhas_w4_healthmean <- read_dta(paste0(base_path, "Clustering/Chapter3/mhas_w4_healthmean.dta"))

  # Merge SHARE wave 1 with its health mean dataset
  share_w1_merged <- merge(share_w1, share_w1_healthmean, by = "cluster")
  
  # Merge SHARE wave 6 with its health mean dataset
  share_w6_merged <- merge(share_w6, share_w6_healthmean, by = "cluster")
  
  # Merge HRS wave 7 with its health mean dataset
  hrs_w7_merged <- merge(hrs_w7, hrs_w7_healthmean, by = "cluster")
  
  # Merge HRS wave 12 with its health mean dataset
  hrs_w12_merged <- merge(hrs_w12, hrs_w12_healthmean, by = "cluster")
  
  # Merge ELSA wave 2 with its health mean dataset
  elsa_w2_merged <- merge(elsa_w2, elsa_w2_healthmean, by = "cluster")
  
  # Merge ELSA wave 8 with its health mean dataset
  elsa_w8_merged <- merge(elsa_w8, elsa_w8_healthmean, by = "cluster")
  
  # Merge KLoSA wave 3 with its health mean dataset
  klosa_w3_merged <- merge(klosa_w3, klosa_w3_healthmean, by = "cluster")
  
  # Merge KLoSA wave 8 with its health mean dataset
  klosa_w8_merged <- merge(klosa_w8, klosa_w8_healthmean, by = "cluster")
  
  # Merge CHARLS wave 1 with its health mean dataset
  charls_w1_merged <- merge(charls_w1, charls_w1_healthmean, by = "cluster")
  
  # Merge CHARLS wave 4 with its health mean dataset
  charls_w4_merged <- merge(charls_w4, charls_w4_healthmean, by = "cluster")
  
  # Merge MHAS wave 2 with its health mean dataset
  mhas_w2_merged <- merge(mhas_w2, mhas_w2_healthmean, by = "cluster")
  
  # Merge MHAS wave 4 with its health mean dataset
  mhas_w4_merged <- merge(mhas_w4, mhas_w4_healthmean, by = "cluster")
  
  

  # Select morbidity columns (r1cancre, r1diabe, r1hearte, etc.) and the category
  morbidity_columns <- c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke")
  selected_data <- filtered_data %>% select(all_of(c("category", morbidity_columns)))
  
  # Reshape data to long format to count each morbidity type
  morbidity_long <- selected_data %>%
    pivot_longer(cols = morbidity_columns, names_to = "morbidity", values_to = "presence") %>%
    filter(presence == 1)  # Keep only records where morbidity is present
  
  # Summarize the count of each morbidity type by category
  morbidity_summary <- morbidity_long %>%
    group_by(category, morbidity) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Create the treemap
  treemap(
    morbidity_summary,
    index = c("category", "morbidity"),  # Hierarchical grouping: category first, then morbidity
    vSize = "count",                     # Size of the boxes represents the count
    title = "Morbidity Prevalence in Moderately Ill and Severely Ill Clusters",
    palette = "Set3",                    # Color palette
    border.col = "white"                 # Border color for better visual separation
  )
  
  
  
  # Load necessary libraries
  library(treemap)
  library(dplyr)
  
  # Filter data for "Moderately ill" and "Severely ill" categories
  filtered_data <- share_w1_merged %>% filter(category %in% c("Moderately Ill", "Severely Ill"))
  
  # Select relevant columns: cluster, category, and morbidity variables
  morbidity_columns <- c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke")
  selected_data <- filtered_data %>% select(cluster, category, all_of(morbidity_columns))
  
  # Reshape data to long format for counting each morbidity type within clusters
  morbidity_long <- selected_data %>%
    pivot_longer(cols = morbidity_columns, names_to = "morbidity", values_to = "presence") %>%
    filter(presence == 1)  # Keep only records where morbidity is present
  
  # Summarize the count of each morbidity type by cluster and category
  morbidity_summary <- morbidity_long %>%
    group_by(category, cluster, morbidity) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Create the treemap
  treemap(
    morbidity_summary,
    index = c("category", "cluster", "morbidity"),  # Hierarchical grouping: category, then cluster, then morbidity
    vSize = "count",                               # Size of the boxes represents the count
    title = "Morbidity Prevalence by Cluster in Moderately Ill and Severely Ill Categories",
    palette = "Set3",                              # Color palette
    border.col = "white"                           # Border color for better visual separation
  )
  
  
# Drop observations with NA in 'cluster' for both datasets
grouped_data <- share_w1 %>%
  drop_na(cluster) %>%
  group_by(cluster)

grouped_data6 <- share_w6 %>%
  drop_na(cluster) %>%
  group_by(cluster)

# Function to calculate prevalence
calculate_prevalence <- function(data, variables) {
  data %>%
    summarise(across(all_of(variables), 
                     ~mean(. == 1, na.rm = TRUE), 
                     .names = "prev_{.col}")) %>%
    mutate(across(starts_with("prev_"), ~round(. * 100, 2)))
}

# Variables for each wave
w1_vars <- c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke")
w6_vars <- c("r6cancre", "r6diabe", "r6hearte", "r6hibpe", "r6lunge", "r6stroke")

# Calculate prevalence for wave 1 and wave 6
w1_prevalence <- grouped_data %>%
  calculate_prevalence(w1_vars) %>%
  mutate(wave = "Wave 1")

w6_prevalence <- grouped_data6 %>%
  calculate_prevalence(w6_vars) %>%
  mutate(wave = "Wave 6")

# Combine prevalence data from both waves
combined_prevalence <- bind_rows(w1_prevalence, w6_prevalence)

# Reshape the data for plotting
long_data <- combined_prevalence %>%
  pivot_longer(cols = starts_with("prev_"), 
               names_to = "morbidity", 
               values_to = "prevalence") %>%
  mutate(morbidity = recode(morbidity, 
                            prev_r1cancre = "Cancer",
                            prev_r1diabe = "Diabetes",
                            prev_r1hearte = "Heart Disease",
                            prev_r1hibpe = "High Blood Pressure",
                            prev_r1lunge = "Lung Disease",
                            prev_r1stroke = "Stroke",
                            prev_r6cancre = "Cancer",
                            prev_r6diabe = "Diabetes",
                            prev_r6hearte = "Heart Disease",
                            prev_r6hibpe = "High Blood Pressure",
                            prev_r6lunge = "Lung Disease",
                            prev_r6stroke = "Stroke"))

setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/Morbishare")

# Set up the PDF device for the bar plot
pdf("bar_plot.pdf", width = 8, height = 6)

# Create bar plot
ggplot(long_data, aes(x = cluster, y = prevalence, fill = morbidity)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~wave) +
  scale_fill_brewer(palette = "RdPu") +
  labs(title = "Evolution of Morbidities by Cluster",
       x = "Cluster",
       y = "Prevalence (%)",
       fill = "Morbidity") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Close the PDF device
dev.off()


# Set up the PDF device for the treemap
pdf("treemap.pdf", width = 10, height = 10)

# Create treemap
treemap(long_data,
        index = c("wave", "cluster", "morbidity"),
        vSize = "prevalence",
        vColor = "prevalence",
        draw.labels = TRUE,
        fontsize.labels = c(12, 10, 8),
        title = "Treemap of Morbidity Prevalence by Cluster and Wave",
        palette = "RdPu",
        border.col = c("darkgrey", "black", "white"),
        border.lwds = c(3, 2, 1),
        border.lw = 1)

# Close the PDF device
dev.off()

# Set up the PDF device for the bubble chart
pdf("bubble_chart.pdf", width = 12, height = 6)

# Create bubble chart
ggplot(long_data, aes(x = cluster, y = morbidity, size = prevalence, color = morbidity)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~wave) +
  scale_size_continuous(range = c(2, 15)) +
  scale_color_brewer(palette = "RdPu") +
  labs(title = "Bubble Chart of Morbidity Prevalence by Cluster and Wave",
       x = "Cluster",
       y = "Morbidity",
       size = "Prevalence (%)",
       color = "Morbidity") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Close the PDF device
dev.off()