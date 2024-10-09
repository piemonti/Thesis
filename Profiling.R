remove(list = ls())
gc()

library(fmsb)
library(haven)
library(tidyr)
library(ggplot2)
library(fastDummies)
library(reshape2)
library(kableExtra)
library(stargazer)
library(dplyr)
library(RColorBrewer)
library(ggridges)
library(grateful)


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

share_w1 <- read_dta(paste0(base_path, "SHARE/output/cluster_assignments/share_w1.dta"))
share_w6 <- read_dta(paste0(base_path, "SHARE/output/cluster_assignments/share_w6.dta"))
hrs_w7 <- read_dta(paste0(base_path, "HRS/output/cluster_assignments/hrs_w7.dta"))
hrs_w12 <- read_dta(paste0(base_path, "HRS/output/cluster_assignments/hrs_w12.dta"))
elsa_w2 <- read_dta(paste0(base_path, "ELSA/UKDA-5050-stata/output/cluster_assignments/elsa_w2.dta"))
elsa_w8 <- read_dta(paste0(base_path, "ELSA/UKDA-5050-stata/output/cluster_assignments/elsa_w8.dta"))
mhas_w2 <- read_dta(paste0(base_path, "MHAS/output/cluster_assignments/mhas_w2.dta"))
mhas_w4 <- read_dta(paste0(base_path, "MHAS/output/cluster_assignments/mhas_w4.dta"))
klosa_w3 <- read_dta(paste0(base_path, "KLoSA/output/cluster_assignments/klosa_w3.dta"))
klosa_w8 <- read_dta(paste0(base_path, "KLoSA/output/cluster_assignments/klosa_w8.dta"))
charls_w1 <- read_dta(paste0(base_path, "CHARLS/output/cluster_assignments/charls_w1.dta"))
charls_w4 <- read_dta(paste0(base_path, "CHARLS/output/cluster_assignments/charls_w4.dta"))


# Set working directory
setwd(paste0(base_path, "Clustering/Analysis"))


jco_colors <- c("#2c7bb6", "#519fcb", "#81b5d3", "#c1cfb9", "#f6d1a5", "#f09068", "#f46d43", "#d73027")
#-------------------Health status-------------------
# Set output directory
output_path <- file.path(base_path, "Clustering/Analysis/Health status/")
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Function to save stargazer output to a LaTeX file
save_stargazer_output <- function(data, title, filename) {
  full_path <- paste0(output_path, filename, ".tex")
  sink(full_path)
  stargazer(data, title = title, summary = FALSE, header = FALSE, type = "latex", digits = 3)
  sink()
}

mean(share_w1$r1agey)
mean(share_w6$r6agey)
mean(hrs_w7$r7agey_e)
mean(hrs_w12$r12agey_e)
mean(elsa_w2$r2agey)
mean(elsa_w8$r8agey)
mean(mhas_w2$r2agey)
mean(mhas_w4$r4agey)
mean(klosa_w3$r3agey)
mean(klosa_w8$r8agey)
mean(charls_w1$r1agey)
mean(charls_w4$r4agey)

#--------------------------SHARE--------------------------
# Convert list to data frame
share_health_w1_df <- data.frame(share_w1$r1_morbi_2, share_w1$r1_morbi_3_plus, share_w1$r1_healthy, share_w1$cluster)

# Rename columns
names(share_health_w1_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
share_w1_healthmean <- share_health_w1_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

share_w1_healthmean
stargazer(share_w1_healthmean, title="SHARE Wave 1 Health status", summary = FALSE, header = FALSE, type = "latex", digits = 3)

# Convert list to data frame
share_health_w6_df <- data.frame(share_w6$r6_morbi_2, share_w6$r6_morbi_3_plus, share_w6$r6_healthy, share_w6$cluster)

# Rename columns
names(share_health_w6_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
share_w6_healthmean <- share_health_w6_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

share_w6_healthmean
stargazer(share_w6_healthmean, title="SHARE Wave 6 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

#--------------------------HRS--------------------------
# Convert list to data frame
hrs_health_w7_df <- data.frame(hrs_w7$r7_morbi_2, hrs_w7$r7_morbi_3_plus, hrs_w7$r7_healthy, hrs_w7$cluster)

# Rename columns
names(hrs_health_w7_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
hrs_w7_healthmean <- hrs_health_w7_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

hrs_w7_healthmean
stargazer(hrs_w7_healthmean, title="HRS Wave 7 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

# Convert list to data frame
hrs_health_w12_df <- data.frame(hrs_w12$r12_morbi_2, hrs_w12$r12_morbi_3_plus, hrs_w12$r12_healthy, hrs_w12$cluster)

# Rename columns
names(hrs_health_w12_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
hrs_w12_healthmean <- hrs_health_w12_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

hrs_w12_healthmean
stargazer(hrs_w12_healthmean, title="HRS Wave 12 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

#--------------------------ELSA--------------------------
# Convert list to data frame
elsa_health_w2_df <- data.frame(elsa_w2$r2_morbi_2, elsa_w2$r2_morbi_3_plus, elsa_w2$r2_healthy, elsa_w2$cluster)

# Rename columns
names(elsa_health_w2_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
elsa_w2_healthmean <- elsa_health_w2_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

elsa_w2_healthmean
stargazer(elsa_w2_healthmean, title="ELSA Wave 2 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

# Convert list to data frame
elsa_health_w8_df <- data.frame(elsa_w8$r8_morbi_2, elsa_w8$r8_morbi_3_plus, elsa_w8$r8_healthy, elsa_w8$cluster)

# Rename columns
names(elsa_health_w8_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
elsa_w8_healthmean <- elsa_health_w8_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

elsa_w8_healthmean
stargazer(elsa_w8_healthmean, title="ELSA Wave 8 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

#--------------------------MHAS--------------------------
# Convert list to data frame
mhas_health_w2_df <- data.frame(mhas_w2$r2_morbi_2, mhas_w2$r2_morbi_3_plus, mhas_w2$r2_healthy, mhas_w2$cluster)

# Rename columns
names(mhas_health_w2_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
mhas_w2_healthmean <- mhas_health_w2_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

mhas_w2_healthmean
stargazer(mhas_w2_healthmean, title="MHAS Wave 2 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

# Convert list to data frame
mhas_health_w4_df <- data.frame(mhas_w4$r4_morbi_2, mhas_w4$r4_morbi_3_plus, mhas_w4$r4_healthy, mhas_w4$cluster)

# Rename columns
names(mhas_health_w4_df) <- c("2 morbidities", "3more_morbidities","healthy", "cluster")

# Group by cluster and calculate mean
mhas_w4_healthmean <- mhas_health_w4_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

mhas_w4_healthmean
stargazer(mhas_w4_healthmean, title="MHAS Wave 4 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

#--------------------------KLoSA--------------------------
# Convert list to data frame
klosa_health_w3_df <- data.frame(klosa_w3$r3_morbi_2, klosa_w3$r3_morbi_3_plus, klosa_w3$r3_healthy, klosa_w3$cluster)

# Rename columns
names(klosa_health_w3_df) <- c("2 morbidities", "3more_morbidities","healthy", "cluster")

# Group by cluster and calculate mean
klosa_w3_healthmean <- klosa_health_w3_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

klosa_w3_healthmean
stargazer(klosa_w3_healthmean, title="KLoSA Wave 3 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

# Convert list to data frame
klosa_health_w8_df <- data.frame(klosa_w8$r8_morbi_2, klosa_w8$r8_morbi_3_plus, klosa_w8$r8_healthy, klosa_w8$cluster)

# Rename columns
names(klosa_health_w8_df) <- c("2 morbidities", "3more_morbidities","healthy", "cluster")

# Group by cluster and calculate mean
klosa_w8_healthmean <- klosa_health_w8_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

klosa_w8_healthmean
stargazer(klosa_w8_healthmean, title="KLoSA Wave 8 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

#--------------------------CHARLS--------------------------
# Convert list to data frame
charls_health_w1_df <- data.frame(charls_w1$r1_morbi_2, charls_w1$r1_morbi_3_plus, charls_w1$r1_healthy, charls_w1$cluster)

# Rename columns
names(charls_health_w1_df) <- c("2 morbidities", "3more_morbidities", "healthy", "cluster")

# Group by cluster and calculate mean
charls_w1_healthmean <- charls_health_w1_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

charls_w1_healthmean
stargazer(charls_w1_healthmean, title="CHARLS Wave 1 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

# Convert list to data frame
charls_health_w4_df <- data.frame(charls_w4$r4_morbi_2, charls_w4$r4_morbi_3_plus, charls_w4$r4_healthy, charls_w4$cluster)

# Rename columns
names(charls_health_w4_df) <- c("2 morbidities", "3more_morbidities","healthy", "cluster")

# Group by cluster and calculate mean
charls_w4_healthmean <- charls_health_w4_df %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

charls_w4_healthmean

stargazer(charls_w4_healthmean, title="CHARLS Wave 4 Health status", summary = FALSE, header = FALSE, type = "latex", digits=3)

# Save stargazer output to LaTeX files
save_stargazer_output(share_w1_healthmean, "SHARE Wave 1 Health status", "SHARE_W1_Health_Status")
save_stargazer_output(share_w6_healthmean, "SHARE Wave 6 Health status", "SHARE_W6_Health_Status")
save_stargazer_output(hrs_w7_healthmean, "HRS Wave 7 Health status", "HRS_W7_Health_Status")
save_stargazer_output(hrs_w12_healthmean, "HRS Wave 12 Health status", "HRS_W12_Health_Status")
save_stargazer_output(elsa_w2_healthmean, "ELSA Wave 2 Health status", "ELSA_W2_Health_Status")
save_stargazer_output(elsa_w8_healthmean, "ELSA Wave 8 Health status", "ELSA_W8_Health_Status")
save_stargazer_output(mhas_w2_healthmean, "MHAS Wave 2 Health status", "MHAS_W2_Health_Status")
save_stargazer_output(mhas_w4_healthmean, "MHAS Wave 4 Health status", "MHAS_W4_Health_Status")
save_stargazer_output(klosa_w3_healthmean, "KLoSA Wave 3 Health status", "KLoSA_W3_Health_Status")
save_stargazer_output(klosa_w8_healthmean, "KLoSA Wave 8 Health status", "KLoSA_W8_Health_Status")
save_stargazer_output(charls_w1_healthmean, "CHARLS Wave 1 Health status", "CHARLS_W1_Health_Status")
save_stargazer_output(charls_w4_healthmean, "CHARLS Wave 4 Health status", "CHARLS_W4_Health_Status")




#-------------------Health status PLOTS-------------------
data <- bind_rows(
  mutate(share_w1_healthmean, wave="SHARE Wave 1"),
  mutate(share_w6_healthmean, wave="SHARE Wave 6"),
  mutate(hrs_w7_healthmean, wave="HRS Wave 7"),
  mutate(hrs_w12_healthmean, wave="HRS Wave 12"),
  mutate(elsa_w2_healthmean, wave="ELSA Wave 2"),
  mutate(elsa_w8_healthmean, wave="ELSA Wave 8"),
  mutate(mhas_w2_healthmean, wave="MHAS Wave 2"),
  mutate(mhas_w4_healthmean, wave="MHAS Wave 4"),
  mutate(klosa_w3_healthmean, wave="KLoSA Wave 3"),
  mutate(klosa_w8_healthmean, wave="KLoSA Wave 8"),
  mutate(charls_w1_healthmean, wave="CHARLS Wave 1"),
  mutate(charls_w4_healthmean, wave="CHARLS Wave 4")
)

# Pivoting the data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(starts_with("2 morbidities"), starts_with("3more_morbidities"), starts_with("healthy")),
    names_to = "status",
    values_to = "mean"
  )

jco_colors <- c("#2c7bb6", "#519fcb", "#81b5d3", "#c1cfb9", "#f6d1a5", "#f09068", "#f46d43", "#d73027", "#d73027")

ggplot(data_long, aes(x=cluster, y=wave, fill=mean)) +
  geom_tile() +
  scale_fill_gradientn(colors = brewer.pal(9, "PuBu")) +
  labs(title="Heatmap of Health Status by Cluster and Wave", x="Cluster", y="Wave") +
  theme_minimal()

ggsave(paste0(output_path, "Health_Status_Heatmap.png"), width=10, height=10, dpi=300)

clusters <- unique(share_w1$cluster)  
cluster_dfs <- list()

#-------------------CUSTOM LABELS-------------------
# Define custom labels for radar chart variables
custom_labels <- c("man", "single", "low educ", "sec. educ", 
                   "ter. educ", "risky beh.", "no PA", "low PA", "good PA",
                   "2 morbi", "3+ morbi", "healthy", "no ADL", "1-2 ADL",
                   "3+ ADL", "no IADL", "1-2 IADL", "3+ IADL", "retired")
# Define custom labels for radar chart variables
custom_labels_mhas <- c("man", "single", "low educ", "sec. educ", 
                   "ter. educ", "risky beh.", "no PA", "good PA",
                   "2 morbi", "3+ morbi", "healthy", "no ADL", "1-2 ADL",
                   "3+ ADL", "no IADL", "1-2 IADL", "3+ IADL", "retired")

#-------------------CLUSTER MEANS-------------------

# Function to compute means by cluster, excluding specified variables
compute_means_by_cluster <- function(data, cluster_col, exclude_vars = NULL) {
  # If exclude_vars is not NULL, remove those columns from the data
  if (!is.null(exclude_vars)) {
    data <- data %>% select(-all_of(exclude_vars))
  }
  means_by_cluster <- data %>%
    group_by(across(all_of(cluster_col))) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')
  return(means_by_cluster)
}

# Variables to exclude
cluster_col_name <- "cluster" 
exclude_vars_sharew1 <- c("h1itot_constant", "r1bmi", "r1agey") # Variables to exclude
exclude_vars_sharew6 <- c("h6ittot_constant", "r6bmi", "r6agey") # Variables to exclude
exclude_vars_hrsw7 <- c("h7itot_constant", "r7bmi", "r7agey_e") # Variables to exclude
exclude_vars_hrsw12 <- c("h12itot_constant", "r12bmi", "r12agey_e") # Variables to exclude
exclude_vars_elsaw2 <- c("h2itot_constant", "r2mbmi", "r2agey") # Variables to exclude
exclude_vars_elsaw8 <- c("h8itot_constant", "r8mbmi", "r8agey") # Variables to exclude
exclude_vars_mhasw2 <- c("h2itot_constant", "r2bmi", "r2agey") # Variables to exclude
exclude_vars_mhasw4 <- c("h4itot_constant", "r4bmi", "r4agey") # Variables to exclude
exclude_vars_klosaw3 <- c("h3itot_constant", "r3bmi", "r3agey") # Variables to exclude
exclude_vars_klosaw8 <- c("h8itot_constant", "r8bmi", "r8agey") # Variables to exclude
exclude_vars_charlsw1 <- c("h1itot_constant", "r1mbmi","r1agey") # Variables to exclude
exclude_vars_charlsw4 <- c("h4itot_constant", "r4agey") # Variables to exclude


means_share_w1 <- compute_means_by_cluster(share_w1, cluster_col_name, exclude_vars_sharew1)
means_share_w6 <- compute_means_by_cluster(share_w6, cluster_col_name, exclude_vars_sharew6)
means_hrs_w7 <- compute_means_by_cluster(hrs_w7, cluster_col_name, exclude_vars_hrsw7)
means_hrs_w12 <- compute_means_by_cluster(hrs_w12, cluster_col_name, exclude_vars_hrsw12)
means_elsa_w2 <- compute_means_by_cluster(elsa_w2, cluster_col_name, exclude_vars_elsaw2)
means_elsa_w8 <- compute_means_by_cluster(elsa_w8, cluster_col_name, exclude_vars_elsaw8)
means_mhas_w2 <- compute_means_by_cluster(mhas_w2, cluster_col_name, exclude_vars_mhasw2)
means_mhas_w4 <- compute_means_by_cluster(mhas_w4, cluster_col_name, exclude_vars_mhasw4)
means_klosa_w3 <- compute_means_by_cluster(klosa_w3, cluster_col_name, exclude_vars_klosaw3)
means_klosa_w8 <- compute_means_by_cluster(klosa_w8, cluster_col_name, exclude_vars_klosaw8)
means_charls_w1 <- compute_means_by_cluster(charls_w1, cluster_col_name, exclude_vars_charlsw1)
means_charls_w4 <- compute_means_by_cluster(charls_w4, cluster_col_name, exclude_vars_charlsw4)


# Function to calculate means of excluded variables
compute_excluded_means <- function(data, cluster_col, excluded_vars) {
  excluded_means <- data %>%
    group_by(across(all_of(cluster_col))) %>%
    summarise(across(all_of(excluded_vars), mean, na.rm = TRUE), .groups = 'drop')
  return(excluded_means)
}

# Compute excluded means for both datasets
excluded_means_w1 <- compute_excluded_means(share_w1, cluster_col_name, exclude_vars_sharew1)
excluded_means_w6 <- compute_excluded_means(share_w6, cluster_col_name, exclude_vars_sharew6)
excluded_means_hrsw7 <- compute_excluded_means(hrs_w7, cluster_col_name, exclude_vars_hrsw7)
excluded_means_hrsw12 <- compute_excluded_means(hrs_w12, cluster_col_name, exclude_vars_hrsw12)
excluded_means_elsaw2 <- compute_excluded_means(elsa_w2, cluster_col_name, exclude_vars_elsaw2)
excluded_means_elsaw8 <- compute_excluded_means(elsa_w8, cluster_col_name, exclude_vars_elsaw8)
excluded_means_mhasw2 <- compute_excluded_means(mhas_w2, cluster_col_name, exclude_vars_mhasw2)
excluded_means_mhasw4 <- compute_excluded_means(mhas_w4, cluster_col_name, exclude_vars_mhasw4)
excluded_means_klosaw3 <- compute_excluded_means(klosa_w3, cluster_col_name, exclude_vars_klosaw3)
excluded_means_klosaw8 <- compute_excluded_means(klosa_w8, cluster_col_name, exclude_vars_klosaw8)
excluded_means_charlsw1 <- compute_excluded_means(charls_w1, cluster_col_name, exclude_vars_charlsw1)
excluded_means_charlsw4 <- compute_excluded_means(charls_w4, cluster_col_name, exclude_vars_charlsw4)

# List of dataframe names
df_names <- c("excluded_means_w1", "excluded_means_w6", "excluded_means_hrsw7", "excluded_means_hrsw12",
              "excluded_means_elsaw2", "excluded_means_elsaw8", "excluded_means_mhasw2", "excluded_means_mhasw4",
              "excluded_means_klosaw3", "excluded_means_klosaw8", "excluded_means_charlsw1", "excluded_means_charlsw4")

# Function to create a readable title
create_title <- function(df_name) {
  parts <- strsplit(df_name, "_")[[1]]
  study <- toupper(parts[2])
  wave <- paste("Wave", substr(parts[3], 1, nchar(parts[3])))
  return(paste(study, wave, "Means"))
}

# Function to capture dataframe output in LaTeX format
capture_df_output <- function(df, title) {
  latex_output <- kable(df, format = "latex", booktabs = TRUE, caption = title) %>%
    kable_styling(latex_options = c("striped", "hold_position"))
  
  return(latex_output)
}
# Define the folder where you want to save the LaTeX files
#output_folder <- "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/Clustering/Analysis/MEANS"

# # Ensure the folder exists
# if (!dir.exists(output_folder)) {
#   dir.create(output_folder, recursive = TRUE)
# }

# # Generate separate LaTeX output for each dataframe
# for (df_name in df_names) {
#   if (exists(df_name)) {
#     df <- get(df_name)
#     output_file <- file.path(output_folder, paste0(df_name, "_table.tex"))
#     
#     latex_table <- capture_df_output(df, create_title(df_name))
#     
#     # Save the LaTeX table to a file in the specified folder
#     cat(latex_table, file = output_file)
#     
#     cat("LaTeX output for", df_name, "has been saved to", output_file, "\n")
#   } else {
#     cat("Warning:", df_name, "not found in the environment.\n")
#   }
# }
create_radar_chart_with_means <- function(data, cluster_number, title, save_path, excluded_means, color) {
  # Ensure the directory exists
  dir_path <- dirname(save_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Filter data for the specific cluster
  cluster_data <- data %>% filter(cluster == cluster_number) %>% select(-cluster)
  
  # Add max and min rows
  cluster_data_radar <- rbind(rep(1, ncol(cluster_data)), rep(0, ncol(cluster_data)), cluster_data)
  
  # Set up PDF for saving the plot
  pdf(file = save_path, width = 10, height = 6) # Increased width for text
  
  # Set layout to have room for text on the right
  layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))  # 3:1 ratio for chart:text
  
  # Radar chart
  par(mar = c(3, 3, 3, 1))  # Adjust margins for radar chart
  radarchart(
    cluster_data_radar, 
    axistype = 1,
    pcol = color,
    pfcol = scales::alpha(color, 0.5),
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = seq(0, 1, 0.25),
    cglwd = 0.8,
    vlcex = 0.8,
    vlabels = custom_labels
  )
  
  # Add title
  title(main = title, cex.main = 1.2)
  
  # Excluded means text
  par(mar = c(3, 1, 3, 1))  # Adjust margins for text area
  plot.new()
  cluster_excluded_means <- excluded_means %>% filter(cluster == cluster_number)
  
  # Dynamically check and print the excluded means
  text(0.5, 0.8, paste("Mean of Excluded Variables:"), cex = 0.9, font = 2)
  if (ncol(cluster_excluded_means) >= 2) {
    text(0.5, 0.6, paste("Mean inc:", round(cluster_excluded_means[[2]], 2)), cex = 0.8)
  }
  if (ncol(cluster_excluded_means) >= 3) {
    text(0.5, 0.5, paste("Mean bmi:", round(cluster_excluded_means[[3]], 2)), cex = 0.8)
  }
  if (ncol(cluster_excluded_means) >= 4) {
    text(0.5, 0.4, paste("Mean age:", round(cluster_excluded_means[[4]], 2)), cex = 0.8)
  }
  
  # Close PDF device
  dev.off()
}


create_radar_chart_with_means_mhas <- function(data, cluster_number, title, save_path, excluded_means, color) {
  # Ensure the directory exists
  dir_path <- dirname(save_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Filter data for the specific cluster
  cluster_data <- data %>% filter(cluster == cluster_number) %>% select(-cluster)
  
  # Add max and min rows
  cluster_data_radar <- rbind(rep(1, ncol(cluster_data)), rep(0, ncol(cluster_data)), cluster_data)
  
  # Set up PDF for saving the plot
  pdf(file = save_path, width = 10, height = 6) # Increased width for text
  
  # Set layout to have room for text on the right
  layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))  # 3:1 ratio for chart:text
  
  # Radar chart
  par(mar = c(3, 3, 3, 1))  # Adjust margins for radar chart
  radarchart(
    cluster_data_radar, 
    axistype = 1,
    pcol = color,
    pfcol = scales::alpha(color, 0.5),
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = seq(0, 1, 0.25),
    cglwd = 0.8,
    vlcex = 0.8,
    vlabels = custom_labels_mhas
  )
  
  # Add title
  title(main = title, cex.main = 1.2)
  
  # Excluded means text
  par(mar = c(3, 1, 3, 1))  # Adjust margins for text area
  plot.new()
  cluster_excluded_means <- excluded_means %>% filter(cluster == cluster_number)
  
  # Dynamically check and print the excluded means
  text(0.5, 0.8, paste("Mean of Excluded Variables:"), cex = 0.9, font = 2)
  if (ncol(cluster_excluded_means) >= 2) {
    text(0.5, 0.6, paste("Mean inc:", round(cluster_excluded_means[[2]], 2)), cex = 0.8)
  }
  if (ncol(cluster_excluded_means) >= 3) {
    text(0.5, 0.5, paste("Mean bmi:", round(cluster_excluded_means[[3]], 2)), cex = 0.8)
  }
  if (ncol(cluster_excluded_means) >= 4) {
    text(0.5, 0.4, paste("Mean age:", round(cluster_excluded_means[[4]], 2)), cex = 0.8)
  }
  
  # Close PDF device
  dev.off()
}
# Define the folder path
sharew1_folder <- "SHARE/wave1"
sharew6_folder <- "SHARE/wave6"
hrsw7_folder <- "HRS/wave7"
hrsw12_folder <- "HRS/wave12"
elsaw2_folder <- "ELSA/wave2"
elsaw8_folder <- "ELSA/wave8"
mhasw2_folder <- "MHAS/wave2"
mhasw4_folder <- "MHAS/wave4"
klosaw3_folder <- "KLoSA/wave3"
klosaw8_folder <- "KLoSA/wave8"
charlsw1_folder <- "CHARLS/wave1"
charlsw4_folder <- "CHARLS/wave4"

# Create radar charts with means for each cluster in each dataset 
for (i in seq_along(unique(means_share_w1$cluster))) {
  cluster <- unique(means_share_w1$cluster)[i]
  save_path <- file.path(sharew1_folder, paste0("SHARE_W1_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_share_w1, cluster, 
    paste("Radar Chart - Share W1 - Cluster", cluster), 
    save_path, 
    excluded_means_w1,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_share_w6$cluster))) {
  cluster <- unique(means_share_w6$cluster)[i]
  save_path <- file.path(sharew6_folder, paste0("SHARE_W6_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_share_w6, cluster, 
    paste("Radar Chart - Share W6 - Cluster", cluster), 
    save_path, 
    excluded_means_w6,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_hrs_w7$cluster))) {
  cluster <- unique(means_hrs_w7$cluster)[i]
  save_path <- file.path(hrsw7_folder, paste0("HRS_W7_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_hrs_w7, cluster, 
    paste("Radar Chart - HRS W7 - Cluster", cluster), 
    save_path, 
    excluded_means_hrsw7,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_hrs_w12$cluster))) {
  cluster <- unique(means_hrs_w12$cluster)[i]
  save_path <- file.path(hrsw12_folder, paste0("HRS_W12_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_hrs_w12, cluster, 
    paste("Radar Chart - HRS W12 - Cluster", cluster), 
    save_path, 
    excluded_means_hrsw12,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_elsa_w2$cluster))) {
  cluster <- unique(means_elsa_w2$cluster)[i]
  save_path <- file.path(elsaw2_folder, paste0("ELSA_W2_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_elsa_w2, cluster, 
    paste("Radar Chart - ELSA W2 - Cluster", cluster), 
    save_path, 
    excluded_means_elsaw2,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_elsa_w8$cluster))) {
  cluster <- unique(means_elsa_w8$cluster)[i]
  save_path <- file.path(elsaw8_folder, paste0("ELSA_W8_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_elsa_w8, cluster, 
    paste("Radar Chart - ELSA W8 - Cluster", cluster), 
    save_path, 
    excluded_means_elsaw8,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_mhas_w2$cluster))) {
  cluster <- unique(means_mhas_w2$cluster)[i]
  save_path <- file.path(mhasw2_folder, paste0("MHAS_W2_cluster", cluster, ".pdf"))
  create_radar_chart_with_means_mhas(
    means_mhas_w2, cluster, 
    paste("Radar Chart - MHAS W2 - Cluster", cluster), 
    save_path, 
    excluded_means_mhasw2,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_mhas_w4$cluster))) {
  cluster <- unique(means_mhas_w4$cluster)[i]
  save_path <- file.path(mhasw4_folder, paste0("MHAS_W4_cluster", cluster, ".pdf"))
  create_radar_chart_with_means_mhas(
    means_mhas_w4, cluster, 
    paste("Radar Chart - MHAS W4 - Cluster", cluster), 
    save_path, 
    excluded_means_mhasw4,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_klosa_w3$cluster))) {
  cluster <- unique(means_klosa_w3$cluster)[i]
  save_path <- file.path(klosaw3_folder, paste0("KLoSA_W3_cluster", cluster, ".pdf"))
  create_radar_chart_with_means_mhas(
    means_klosa_w3, cluster, 
    paste("Radar Chart - KLoSA W3 - Cluster", cluster), 
    save_path, 
    excluded_means_klosaw3,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_klosa_w8$cluster))) {
  cluster <- unique(means_klosa_w8$cluster)[i]
  save_path <- file.path(klosaw8_folder, paste0("KLoSA_W8_cluster", cluster, ".pdf"))
  create_radar_chart_with_means_mhas(
    means_klosa_w8, cluster, 
    paste("Radar Chart - KLoSA W8 - Cluster", cluster), 
    save_path, 
    excluded_means_klosaw8,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_charls_w1$cluster))) {
  cluster <- unique(means_charls_w1$cluster)[i]
  save_path <- file.path(charlsw1_folder, paste0("CHARLS_W1_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_charls_w1, cluster, 
    paste("Radar Chart - CHARLS W1 - Cluster", cluster), 
    save_path, 
    excluded_means_charlsw1,
    jco_colors[i %% length(jco_colors) ]
  )
}

for (i in seq_along(unique(means_charls_w4$cluster))) {
  cluster <- unique(means_charls_w4$cluster)[i]
  save_path <- file.path(charlsw4_folder, paste0("CHARLS_W4_cluster", cluster, ".pdf"))
  create_radar_chart_with_means(
    means_charls_w4, cluster, 
    paste("Radar Chart - CHARLS W4 - Cluster", cluster), 
    save_path, 
    excluded_means_charlsw4,
    jco_colors[i %% length(jco_colors) ]
  )
}

#-------------------BOX PLOTS-------------------

# Filter data for specific clusters
specific_clusters <- hrs_w7 %>% filter(cluster %in% c (1,2,6,8))

# Filter data for specific clusters
# Apply log transformation to the data
specific_clusters$h7itot_constant <- log(specific_clusters$h7itot_constant)

# Plot density distribution for specific clusters with faceting and adjusted alpha
ggplot(specific_clusters, aes(x = h7itot_constant, fill = factor(cluster))) +
  geom_density(alpha = 0.3) +
  labs(x = "h7itot_constant (log scale)", y = "Density") +
  theme_minimal()

#-------------------RIDGE PLOTS-------------------

# Create the ridge plot
ggplot(hrs_w7, aes(x = h7itot_constant, y = as.factor(cluster), fill = as.factor(cluster))) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(quantile(hrs_w7$h7itot_constant, 0.01), quantile(hrs_w7$h7itot_constant, 0.99))) +
  scale_fill_manual(values = jco_colors) +
  labs(x = "h7itot_constant", y = "Cluster", fill = "Cluster") +
  theme_ridges() +
  theme(legend.position = "none")
alt_colors <- c("#2c7bb6", "#519fcb", "#81b5d3", "#c1cfb9", "#f6d1a5", "#f09068", "#f46d43", "#d73027")
# Function to create and save ridge plots
create_ridge_plot <- function(data, x_var, cluster_var, title) {
  p <- ggplot(data, aes_string(x = x_var, y = paste0("as.factor(", cluster_var, ")"), fill = paste0("as.factor(", cluster_var, ")"))) +
    geom_density_ridges(scale = 3, rel_min_height = 0.01) +
    scale_x_continuous(limits = c(quantile(data[[x_var]], 0.01), quantile(data[[x_var]], 0.99))) +
    scale_fill_manual(values = alt_colors) +
    labs(x = x_var, y = "Cluster", fill = "Cluster", title = title) +
    theme_ridges() +
    theme(legend.position = "none")

  # Save the plot as a .png file with a name based on the title
  ggsave(paste0(title, ".pdf"), plot = p, width = 10, height = 6, dpi = 300)
}

# Call the function for each dataset
create_ridge_plot(share_w1, "h1itot_constant", "cluster", "SHARE_Wave_1")
create_ridge_plot(share_w6, "h6ittot_constant", "cluster", "SHARE_Wave_6")
create_ridge_plot(hrs_w7, "h7itot_constant", "cluster", "HRS_Wave_7")
create_ridge_plot(hrs_w12, "h12itot_constant", "cluster", "HRS_Wave_12")
create_ridge_plot(elsa_w2, "h2itot_constant", "cluster", "ELSA_Wave_2")
create_ridge_plot(elsa_w8, "h8itot_constant", "cluster", "ELSA_Wave_8")
create_ridge_plot(mhas_w2, "h2itot_constant", "cluster", "MHAS_Wave_2")
create_ridge_plot(mhas_w4, "h4itot_constant", "cluster", "MHAS_Wave_4")
create_ridge_plot(klosa_w3, "h3itot_constant", "cluster", "KLoSA_Wave_3")
create_ridge_plot(klosa_w8, "h8itot_constant", "cluster", "KLoSA_Wave_8")
create_ridge_plot(charls_w1, "h1itot_constant", "cluster", "CHARLS_Wave_1")
create_ridge_plot(charls_w4, "h4itot_constant", "cluster", "CHARLS_Wave_4")



#-------------------OVER IMPOSED RADAR CHARTS-------------------
share_w1_healthmean
share_w6_healthmean
hrs_w7_healthmean
hrs_w12_healthmean
elsa_w2_healthmean
elsa_w8_healthmean
mhas_w2_healthmean
mhas_w4_healthmean
klosa_w3_healthmean
klosa_w8_healthmean
charls_w1_healthmean
charls_w4_healthmean

# Function to classify health status
classify_health_status <- function(df) {
  df$category <- with(df, ifelse(`healthy` > 0.5, "Overall Healthy",
                                 ifelse(`2 morbidities` + `3more_morbidities` > 0.3, 
                                        ifelse(`2 morbidities` + `3more_morbidities` > 0.5, "Severely Ill", "Moderately Ill"), 
                                        "Overall Healthy")))
  return(df)
}

# Apply classification to each dataset
share_w1_healthmean <- classify_health_status(share_w1_healthmean)
share_w6_healthmean <- classify_health_status(share_w6_healthmean)
hrs_w7_healthmean <- classify_health_status(hrs_w7_healthmean)
hrs_w12_healthmean <- classify_health_status(hrs_w12_healthmean)
elsa_w2_healthmean <- classify_health_status(elsa_w2_healthmean)
elsa_w8_healthmean <- classify_health_status(elsa_w8_healthmean)
mhas_w2_healthmean <- classify_health_status(mhas_w2_healthmean)
mhas_w4_healthmean <- classify_health_status(mhas_w4_healthmean)
klosa_w3_healthmean <- classify_health_status(klosa_w3_healthmean)
klosa_w8_healthmean <- classify_health_status(klosa_w8_healthmean)
charls_w1_healthmean <- classify_health_status(charls_w1_healthmean)
charls_w4_healthmean <- classify_health_status(charls_w4_healthmean)


share_w1_healthmean <- as.data.frame(share_w1_healthmean) 
share_w6_healthmean <- as.data.frame(share_w6_healthmean)
hrs_w7_healthmean <- as.data.frame(hrs_w7_healthmean)
hrs_w12_healthmean <- as.data.frame(hrs_w12_healthmean)
elsa_w2_healthmean <- as.data.frame(elsa_w2_healthmean)
elsa_w8_healthmean <- as.data.frame(elsa_w8_healthmean)
mhas_w2_healthmean <- as.data.frame(mhas_w2_healthmean)
mhas_w4_healthmean <- as.data.frame(mhas_w4_healthmean)
klosa_w3_healthmean <- as.data.frame(klosa_w3_healthmean)
klosa_w8_healthmean <- as.data.frame(klosa_w8_healthmean)
charls_w1_healthmean <- as.data.frame(charls_w1_healthmean)
charls_w4_healthmean <- as.data.frame(charls_w4_healthmean)

# Function to rename columns to be Stata compatible
rename_columns <- function(df) {
  colnames(df) <- gsub(" ", "_", colnames(df))  # Replace spaces with underscores
  colnames(df) <- gsub("2_morbidities", "morbidities_2", colnames(df))  # Move numbers to the end
  colnames(df) <- gsub("3more_morbidities", "morbidities_3plus", colnames(df))  # Rename column
  return(df)
}

# Apply renaming to each dataset
share_w1_healthmean <- rename_columns(share_w1_healthmean)
share_w6_healthmean <- rename_columns(share_w6_healthmean)
hrs_w7_healthmean <- rename_columns(hrs_w7_healthmean)
hrs_w12_healthmean <- rename_columns(hrs_w12_healthmean)
elsa_w2_healthmean <- rename_columns(elsa_w2_healthmean)
elsa_w8_healthmean <- rename_columns(elsa_w8_healthmean)
mhas_w2_healthmean <- rename_columns(mhas_w2_healthmean)
mhas_w4_healthmean <- rename_columns(mhas_w4_healthmean)
klosa_w3_healthmean <- rename_columns(klosa_w3_healthmean)
klosa_w8_healthmean <- rename_columns(klosa_w8_healthmean)
charls_w1_healthmean <- rename_columns(charls_w1_healthmean)
charls_w4_healthmean <- rename_columns(charls_w4_healthmean)

#save healthmean dataframes to dta in a specific folder
ch3folder <- paste0(base_path, "Clustering/Chapter3/")
write_dta(share_w1_healthmean, paste0(ch3folder, "share_w1_healthmean.dta"))
write_dta(share_w6_healthmean, paste0(ch3folder, "share_w6_healthmean.dta"))
write_dta(hrs_w7_healthmean, paste0(ch3folder, "hrs_w7_healthmean.dta"))
write_dta(hrs_w12_healthmean, paste0(ch3folder, "hrs_w12_healthmean.dta"))
write_dta(elsa_w2_healthmean, paste0(ch3folder, "elsa_w2_healthmean.dta"))
write_dta(elsa_w8_healthmean, paste0(ch3folder, "elsa_w8_healthmean.dta"))
write_dta(mhas_w2_healthmean, paste0(ch3folder, "mhas_w2_healthmean.dta"))
write_dta(mhas_w4_healthmean, paste0(ch3folder, "mhas_w4_healthmean.dta"))
write_dta(klosa_w3_healthmean, paste0(ch3folder, "klosa_w3_healthmean.dta"))
write_dta(klosa_w8_healthmean, paste0(ch3folder, "klosa_w8_healthmean.dta"))
write_dta(charls_w1_healthmean, paste0(ch3folder, "charls_w1_healthmean.dta"))
write_dta(charls_w4_healthmean, paste0(ch3folder, "charls_w4_healthmean.dta"))



# List of dataframe names
df_names <- c("share_w1_healthmean", "share_w6_healthmean",
              "hrs_w7_healthmean", "hrs_w12_healthmean",
              "elsa_w2_healthmean", "elsa_w8_healthmean",
              "mhas_w2_healthmean", "mhas_w4_healthmean",
              "klosa_w3_healthmean", "klosa_w8_healthmean",
              "charls_w1_healthmean", "charls_w4_healthmean")

# Function to create a readable title
create_title <- function(df_name) {
  parts <- strsplit(df_name, "_")[[1]]
  study <- toupper(parts[1])
  wave <- paste("Wave", substr(parts[2], 2, nchar(parts[2])))
  return(paste(study, wave, "Health Mean"))
}

# Function to capture dataframe output in LaTeX format
capture_df_output <- function(df, title) {
  latex_output <- kable(df, format = "latex", booktabs = TRUE, caption = title) %>%
    kable_styling(latex_options = c("striped", "hold_position"))
  
  return(latex_output)
}

# Generate separate LaTeX output for each dataframe
for (df_name in df_names) {
  if (exists(df_name)) {
    df <- get(df_name)
    output_file <- paste0(df_name, "_table.tex")
    
    latex_table <- capture_df_output(df, create_title(df_name))
    
    # Save the LaTeX table to a file
    cat(latex_table, file = output_file)
    
    cat("LaTeX output for", df_name, "has been saved to", output_file, "\n")
  } else {
    cat("Warning:", df_name, "not found in the environment.\n")
  }
}
create_category_specific_radar_charts <- function(means_data, healthmean_data, save_path_prefix, dataset_name, custom_labels) {
  # Ensure the directory exists
  dir_path <- dirname(save_path_prefix)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Merge means_data with healthmean_data to get categories
  merged_data <- merge(means_data, healthmean_data[, c("cluster", "category")], by = "cluster")
  
  # Define color palette
  jco_colors <- c("#2c7bb6", "#81b5d3", "#e8b987", "#f46d43", "#d73027")
  
  # Define line types
  line_types <- c(1, 2, 6, 4, 5, 6, 1, 2)  # Repeat if more than 8 clusters
  
  # Create a radar chart for each category
  for (category in unique(merged_data$category)) {
    # Filter data for the current category
    category_data <- merged_data %>% filter(category == !!category)
    
    # Prepare data for radar chart
    radar_data <- category_data %>%
      select(-cluster, -category) %>%
      as.data.frame()
    
    # Add max and min rows
    radar_data <- rbind(rep(1, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)
    
    # Set up PDF for saving the plot
    pdf_path <- paste0(save_path_prefix, "_", gsub(" ", "_", category), ".pdf")
    pdf(file = pdf_path, width = 10, height = 8)
    
    # Create radar chart
    radarchart(
      radar_data, 
      axistype = 1,
      pcol = jco_colors[1:nrow(category_data)],
      pfcol = scales::alpha(jco_colors[1:nrow(category_data)], 0.2),
      plwd = 2,
      plty = line_types[1:nrow(category_data)],  # Add this line
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 1, 0.25),
      cglwd = 0.8,
      vlcex = 0.8,
      vlabels = custom_labels
    )
    
    # Add title
    title(main = paste(dataset_name, "-", category), cex.main = 1.2)
    
    # Add legend
    legend("topright", 
           legend = paste("Cluster", category_data$cluster), 
           col = jco_colors[1:nrow(category_data)], 
           lty = line_types[1:nrow(category_data)],  # Update this line
           lwd = 2, 
           bty = "n",
           cex = 0.8)
    
    # Close PDF device
    dev.off()
  }
}

# Create category-specific radar charts for each dataset
create_category_specific_radar_charts(
  means_share_w1, share_w1_healthmean,
  "SHARE/category_charts/SHARE_W1", 
  "SHARE Wave 1",
  custom_labels
)

create_category_specific_radar_charts(
  means_share_w6, share_w6_healthmean,
  "SHARE/category_charts/SHARE_W6", 
  "SHARE Wave 6",
  custom_labels
)

create_category_specific_radar_charts(
  means_hrs_w7, hrs_w7_healthmean,
  "HRS/category_charts/HRS_W7", 
  "HRS Wave 7",
  custom_labels
)

create_category_specific_radar_charts(
  means_hrs_w12, hrs_w12_healthmean,
  "HRS/category_charts/HRS_W12", 
  "HRS Wave 12",
  custom_labels
)

create_category_specific_radar_charts(
  means_elsa_w2, elsa_w2_healthmean,
  "ELSA/category_charts/ELSA_W2", 
  "ELSA Wave 2",
  custom_labels
)

create_category_specific_radar_charts(
  means_elsa_w8, elsa_w8_healthmean,
  "ELSA/category_charts/ELSA_W8", 
  "ELSA Wave 8",
  custom_labels
)

create_category_specific_radar_charts(
  means_mhas_w2, mhas_w2_healthmean,
  "MHAS/category_charts/MHAS_W2", 
  "MHAS Wave 2",
  custom_labels_mhas
)

create_category_specific_radar_charts(
  means_mhas_w4, mhas_w4_healthmean,
  "MHAS/category_charts/MHAS_W4", 
  "MHAS Wave 4",
  custom_labels_mhas
)

create_category_specific_radar_charts(
  means_klosa_w3, klosa_w3_healthmean,
  "KLoSA/category_charts/KLoSA_W3", 
  "KLoSA Wave 3",
  custom_labels_mhas
)

create_category_specific_radar_charts(
  means_klosa_w8, klosa_w8_healthmean,
  "KLoSA/category_charts/KLoSA_W8", 
  "KLoSA Wave 8",
  custom_labels_mhas
)

create_category_specific_radar_charts(
  means_charls_w1, charls_w1_healthmean,
  "CHARLS/category_charts/CHARLS_W1", 
  "CHARLS Wave 1",
  custom_labels
)

create_category_specific_radar_charts(
  means_charls_w4, charls_w4_healthmean,
  "CHARLS/category_charts/CHARLS_W4", 
  "CHARLS Wave 4",
  custom_labels
)

#-------------------AGE RIDGE PLOTS-------------------
# Define the color palette
alt_colors <- c("#2c7bb6", "#519fcb", "#81b5d3", "#c1cfb9", "#f6d1a5", "#f09068", "#f46d43", "#d73027")
alt_colors <- c("#2c7bb6", "#519fcb", "#81b5d3", "#c1cfb9", "#f6d1a5", "#f09068", "#f46d43", "#d73027", 
                "#1b9e77", "#d95f02", "#7570b3", "#e7298a")
# Combine datasets into one data frame
combined_data <- bind_rows(
  share_w1 %>% mutate(Wave = "SHARE Wave 1", Age = r1agey),
  share_w6 %>% mutate(Wave = "SHARE Wave 6", Age = r6agey),
  hrs_w7 %>% mutate(Wave = "HRS Wave 7", Age = r7agey_e),
  hrs_w12 %>% mutate(Wave = "HRS Wave 12", Age = r12agey_e),
  elsa_w2 %>% mutate(Wave = "ELSA Wave 2", Age = r2agey),
  elsa_w8 %>% mutate(Wave = "ELSA Wave 8", Age = r8agey),
  mhas_w2 %>% mutate(Wave = "MHAS Wave 2", Age = r2agey),
  mhas_w4 %>% mutate(Wave = "MHAS Wave 4", Age = r4agey),
  klosa_w3 %>% mutate(Wave = "KLoSA Wave 3", Age = r3agey),
  klosa_w8 %>% mutate(Wave = "KLoSA Wave 8", Age = r8agey),
  charls_w1 %>% mutate(Wave = "CHARLS Wave 1", Age = r1agey),
  charls_w4 %>% mutate(Wave = "CHARLS Wave 4", Age = r4agey)
)

library(paletteer)
  
ggplot(combined_data, aes(x = Age, y = Wave, fill = Wave)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(quantile(combined_data$Age, 0.01), quantile(combined_data$Age, 0.99))) +
  scale_fill_manual(values = paletteer_c("grDevices::SunsetDark", 12)) +
  labs(x = "Age", y = "Dataset Wave", fill = "Wave", title = "Age Distribution Across Waves") +
  theme_ridges() +
  theme(legend.position = "none")

# Optionally save the plot
ggsave("combined_ridge_plot.pdf", width = 10, height = 6, dpi = 300)

#-------------------PREVALENCE OF ILL CLUSTERS-------------------
get_cluster_sizes <- function(dataset, dataset_name) {
  dataset %>%
    group_by(cluster) %>%
    summarise(size = n()) %>%
    ungroup()
}

# Calculate cluster sizes for each dataset and store them in separate data frames
share_w1_clusters <- get_cluster_sizes(share_w1, "share_w1")
share_w6_clusters <- get_cluster_sizes(share_w6, "share_w6")
hrs_w7_clusters <- get_cluster_sizes(hrs_w7, "hrs_w7")
hrs_w12_clusters <- get_cluster_sizes(hrs_w12, "hrs_w12")
elsa_w2_clusters <- get_cluster_sizes(elsa_w2, "elsa_w2")
elsa_w8_clusters <- get_cluster_sizes(elsa_w8, "elsa_w8")
mhas_w2_clusters <- get_cluster_sizes(mhas_w2, "mhas_w2")
mhas_w4_clusters <- get_cluster_sizes(mhas_w4, "mhas_w4")
klosa_w3_clusters <- get_cluster_sizes(klosa_w3, "klosa_w3")
klosa_w8_clusters <- get_cluster_sizes(klosa_w8, "klosa_w8")
charls_w1_clusters <- get_cluster_sizes(charls_w1, "charls_w1")
charls_w4_clusters <- get_cluster_sizes(charls_w4, "charls_w4")

share_w1_combined <- share_w1_healthmean %>%
  left_join(share_w1_clusters, by = "cluster")

share_w6_combined <- share_w6_healthmean %>%
  left_join(share_w6_clusters, by = "cluster")

hrs_w7_combined <- hrs_w7_healthmean %>%
  left_join(hrs_w7_clusters, by = "cluster")

hrs_w12_combined <- hrs_w12_healthmean %>%
  left_join(hrs_w12_clusters, by = "cluster")

elsa_w2_combined <- elsa_w2_healthmean %>%
  left_join(elsa_w2_clusters, by = "cluster")

elsa_w8_combined <- elsa_w8_healthmean %>%
  left_join(elsa_w8_clusters, by = "cluster")

mhas_w2_combined <- mhas_w2_healthmean %>%
  left_join(mhas_w2_clusters, by = "cluster")

mhas_w4_combined <- mhas_w4_healthmean %>%
  left_join(mhas_w4_clusters, by = "cluster")

klosa_w3_combined <- klosa_w3_healthmean %>%
  left_join(klosa_w3_clusters, by = "cluster")

klosa_w8_combined <- klosa_w8_healthmean %>%
  left_join(klosa_w8_clusters, by = "cluster")

charls_w1_combined <- charls_w1_healthmean %>%
  left_join(charls_w1_clusters, by = "cluster")

charls_w4_combined <- charls_w4_healthmean %>%
  left_join(charls_w4_clusters, by = "cluster")

share_w1_combined
share_w6_combined
hrs_w7_combined
hrs_w12_combined
elsa_w2_combined
elsa_w8_combined
mhas_w2_combined
mhas_w4_combined
klosa_w3_combined
klosa_w8_combined
charls_w1_combined
charls_w4_combined

dataset_pairs <- list(
  list(share_w1_combined, share_w6_combined),
  list(hrs_w7_combined, hrs_w12_combined),
  list(elsa_w2_combined, elsa_w8_combined),
  list(mhas_w2_combined, mhas_w4_combined),
  list(klosa_w3_combined, klosa_w8_combined),
  list(charls_w1_combined, charls_w4_combined)
)

# Define dataset names for labeling the pairs
pair_names <- c("SHARE", "HRS", "ELSA", 
                "MHAS", "KLoSA", "CHARLS")

# Initialize a data frame to store the results
prevalence_pairs <- data.frame(Pair = pair_names, Prevalence_Wave1 = numeric(length(pair_names)), 
                               Prevalence_Wave2 = numeric(length(pair_names)), Increase = numeric(length(pair_names)))

# Loop through each dataset pair and calculate the prevalence for each wave
for (i in 1:length(dataset_pairs)) {
  wave1 <- dataset_pairs[[i]][[1]]  # First wave dataset
  wave2 <- dataset_pairs[[i]][[2]]  # Second wave dataset
  
  # Check if both wave1 and wave2 are data frames
  if (is.data.frame(wave1) & is.data.frame(wave2)) {
    
    # Calculate prevalence for wave 1 (first dataset in the pair)
    ill_wave1 <- wave1[wave1$category %in% c("Severely Ill", "Moderately Ill"), ]
    total_ill_wave1 <- sum(ill_wave1$size)
    total_pop_wave1 <- sum(wave1$size)
    prevalence_wave1 <- total_ill_wave1 / total_pop_wave1
    
    # Calculate prevalence for wave 2 (second dataset in the pair)
    ill_wave2 <- wave2[wave2$category %in% c("Severely Ill", "Moderately Ill"), ]
    total_ill_wave2 <- sum(ill_wave2$size)
    total_pop_wave2 <- sum(wave2$size)
    prevalence_wave2 <- total_ill_wave2 / total_pop_wave2
    
    # Store the prevalence values
    prevalence_pairs$Prevalence_Wave1[i] <- prevalence_wave1
    prevalence_pairs$Prevalence_Wave2[i] <- prevalence_wave2
    
    # Calculate the fold increase between the two waves
    prevalence_pairs$Increase[i] <- prevalence_wave2 / prevalence_wave1
  } else {
    stop("One or both of the datasets are not data frames.")
  }
}

# Display the calculated prevalence and fold increase data
print(prevalence_pairs)

# Assuming prevalence_pairs contains the data, including the 'Pair' and 'Increase' columns
alt_colors <- c("#519fcb", "#81b5d3", "#c1cfb9", "#f6d1a5", "#f09068", "#f46d43")

# Create the bar chart
ggplot(prevalence_pairs, aes(x = Pair, y = Increase)) +
  geom_bar(stat = "identity", aes(fill = Pair)) +  # Map 'Pair' to 'fill'
  geom_text(aes(label = paste0(round(Increase, 2), "x")), 
            vjust = -0.5, size = 5.5) +
  labs(title = "Increase in Joint Prevalence of Ill Clusters Between Waves",
       y = "Fold Increase in Prevalence") +
  scale_fill_manual(values = alt_colors) +  # Use custom color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot to specified folder
ggsave("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/Clustering/Analysis/Increase_in_Joint_Prevalence.pdf",
       width = 10, height = 8)
#-------

# Initialize a data frame to store the contribution of 2 morbidities and 3+ morbidities for each dataset
contribution_data <- data.frame(Pair = pair_names, 
                                Contribution_2_Morbidities_Wave1 = numeric(length(pair_names)),
                                Contribution_3_More_Morbidities_Wave1 = numeric(length(pair_names)),
                                Contribution_2_Morbidities_Wave2 = numeric(length(pair_names)),
                                Contribution_3_More_Morbidities_Wave2 = numeric(length(pair_names)))

# Loop through each dataset pair and calculate the contribution of 2 morbidities and 3+ morbidities
for (i in 1:length(dataset_pairs)) {
  wave1 <- dataset_pairs[[i]][[1]]  # First wave dataset
  wave2 <- dataset_pairs[[i]][[2]]  # Second wave dataset
  
  # Check if both wave1 and wave2 are data frames
  if (is.data.frame(wave1) & is.data.frame(wave2)) {
    
    # Calculate contribution for wave 1 (first dataset in the pair)
    contrib_2_morbid_wave1 <- sum(wave1$`2 morbidities` * wave1$size) / sum(wave1$size)
    contrib_3_more_morbid_wave1 <- sum(wave1$`3more_morbidities` * wave1$size) / sum(wave1$size)
    
    # Calculate contribution for wave 2 (second dataset in the pair)
    contrib_2_morbid_wave2 <- sum(wave2$`2 morbidities` * wave2$size) / sum(wave2$size)
    contrib_3_more_morbid_wave2 <- sum(wave2$`3more_morbidities` * wave2$size) / sum(wave2$size)
    
    # Store the contributions
    contribution_data$Contribution_2_Morbidities_Wave1[i] <- contrib_2_morbid_wave1
    contribution_data$Contribution_3_More_Morbidities_Wave1[i] <- contrib_3_more_morbid_wave1
    contribution_data$Contribution_2_Morbidities_Wave2[i] <- contrib_2_morbid_wave2
    contribution_data$Contribution_3_More_Morbidities_Wave2[i] <- contrib_3_more_morbid_wave2
  } else {
    stop("One or both of the datasets are not data frames.")
  }
}

# Display the calculated contribution data
print(contribution_data)


# Add columns for fold change in contribution for each morbidity group
contribution_data$Fold_Change_2_Morbidities <- contribution_data$Contribution_2_Morbidities_Wave2 / contribution_data$Contribution_2_Morbidities_Wave1
contribution_data$Fold_Change_3_More_Morbidities <- contribution_data$Contribution_3_More_Morbidities_Wave2 / contribution_data$Contribution_3_More_Morbidities_Wave1

# Display the fold changes
print(contribution_data)

long_data <- melt(contribution_data[, c("Pair", 
                                        "Contribution_2_Morbidities_Wave1", 
                                        "Contribution_3_More_Morbidities_Wave1",
                                        "Contribution_2_Morbidities_Wave2", 
                                        "Contribution_3_More_Morbidities_Wave2")],
                  id.vars = "Pair")

# Rename the variables for better readability
long_data$variable <- factor(long_data$variable, levels = c("Contribution_2_Morbidities_Wave1", 
                                                            "Contribution_3_More_Morbidities_Wave1", 
                                                            "Contribution_2_Morbidities_Wave2", 
                                                            "Contribution_3_More_Morbidities_Wave2"),
                             labels = c("2 Morbidities (1st.Wave)", "3+ Morbidities (1st.Wave)",
                                        "2 Morbidities (Last Wave)", "3+ Morbidities (Last Wave)"))

ggplot(long_data, aes(x = Pair, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, 2)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#519fcb", "#81b5d3", "#c1cfb9", "#f6d1a5")) +  # Use your custom color palette
  labs(title = "Contribution of 2 Morbidities and 3+ Morbidities Across Waves",
       x = "Dataset Pair",
       y = "Contribution to Population (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/Clustering/Analysis/Contribution_of_Morbidities.pdf",
       width = 10, height = 8)


#Create a plot of the ratios of contribution between waves
# Calculate the ratios for 2 morbidities and 3+ morbidities
contribution_data$Ratio_2_Morbidities <- contribution_data$Contribution_2_Morbidities_Wave2 / contribution_data$Contribution_2_Morbidities_Wave1
contribution_data$Ratio_3_More_Morbidities <- contribution_data$Contribution_3_More_Morbidities_Wave2 / contribution_data$Contribution_3_More_Morbidities_Wave1

# Melt the data to long format for plotting
long_ratio_data <- melt(contribution_data[, c("Pair", "Ratio_2_Morbidities", "Ratio_3_More_Morbidities")],
                        id.vars = "Pair")

# Rename the variables for better readability
long_ratio_data$variable <- factor(long_ratio_data$variable, levels = c("Ratio_2_Morbidities", "Ratio_3_More_Morbidities"),
                                   labels = c("2 Morbidities (Ratio)", "3+ Morbidities (Ratio)"))

# Plotting the ratios using ggplot2
ggplot(long_ratio_data, aes(x = Pair, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, 2)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#81b5d3", "#c1cfb9")) +  # Use your custom color palette
  labs(title = "Ratio of Contribution of Morbidities Between Waves",
       x = "Dataset Pair",
       y = "Ratio of Contribution") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Define the color palette based on the uploaded image
pair_colors <- c(
  "CHARLS" = "#519fcb",  # Light blue
  "ELSA" = "#81b5d3",    # Sky blue
  "HRS" = "#c1cfb9",     # Light green
  "KLoSA" = "#f6d1a5",   # Peach
  "MHAS" = "#f09068",    # Light orange
  "SHARE" = "#f46d43"    # Orange
)

# Define lighter shades for "3+ Morbidities"
pair_lighter_colors <- c(
  "CHARLS" = "#a1c6e1",
  "ELSA" = "#bcd7e7",
  "HRS" = "#d9e4c8",
  "KLoSA" = "#fde1c4",
  "MHAS" = "#f6b794",
  "SHARE" = "#f89e75"
)

# Create a named vector to map colors to the pairs for the two ratios
color_mapping <- setNames(c(rbind(pair_colors, pair_lighter_colors)), 
                          c(rbind(paste0(names(pair_colors), "_2"), 
                                  paste0(names(pair_colors), "_3"))))

# Melt the data to long format for plotting
long_ratio_data <- melt(contribution_data[, c("Pair", "Ratio_2_Morbidities", "Ratio_3_More_Morbidities")],
                        id.vars = "Pair")

# Create a new variable to distinguish between 2 and 3+ morbidities for color mapping
long_ratio_data$fill_key <- ifelse(long_ratio_data$variable == "Ratio_2_Morbidities", 
                                   paste0(long_ratio_data$Pair, "_2"), 
                                   paste0(long_ratio_data$Pair, "_3"))

# Rename the variables for better readability
long_ratio_data$variable <- factor(long_ratio_data$variable, levels = c("Ratio_2_Morbidities", "Ratio_3_Morbidities"),
                                   labels = c("2 Morbidities (Ratio)", "3+ Morbidities (Ratio)"))

# Plotting the ratios using ggplot2
ggplot(long_ratio_data, aes(x = Pair, y = value, fill = fill_key)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, 2)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = color_mapping,
    name = "Morbidities",
    labels = c(
      "CHARLS_2" = "2 Morbi.",
      "CHARLS_3" = "3+ Morbi.",
      "ELSA_2" = "2 Morbi.",
      "ELSA_3" = "3+ Morbi.",
      "HRS_2" = "2 Morbi.",
      "HRS_3" = "3+ Morbi.",
      "KLoSA_2" = "2 Morbi.",
      "KLoSA_3" = "3+ Morbi.",
      "MHAS_2" = "2 Morbi.",
      "MHAS_3" = "3+ Morbi.",
      "SHARE_2" = "2 Morbi.",
      "SHARE_3" = "3+ Morbi."
    )
  ) + 
  labs(
    title = "Ratio of Contribution of Morbidities Between Waves",
    x = "Dataset Pair",
    y = "Ratio of Contribution"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",  # Move legend to the top
    legend.title = element_text(face = "bold"),  # Make legend title bold
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Save the plot
ggsave("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/Clustering/Analysis/Ratio_of_Contribution_of_Morbidities.pdf",
       width = 10, height = 8)

