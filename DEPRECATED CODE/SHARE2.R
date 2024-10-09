remove(list=ls())
# Load required libraries
library(dplyr)
library(haven)
library(tidyr)
library(psych)
library(ggplot2)
library(reshape2)
library(stringr)
library(tidyverse)
library(forcats)
library(writexl)
library(ggpubr)    
library(GGally) 
library(skimr)
setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/SHARE")

# Read SHARE dataset
sharedata <- read_dta("output/sampled_data.dta")

# Define the country codes to retain
desired_countries <- c("IT", "FR", "ES", "DE", "SE", "DK", "FI")
# Filter the dataset based on the MERGEID variable and desired country codes
 sharedata_filtered <- sharedata %>%
   filter(substr(sharedata$mergeid, 1, 2) %in% desired_countries)

 # Filter the dataset based on the conditions for r1iwstat and r6iwstat, rather than using isalive
share_final <- sharedata_filtered %>%
   filter(r1iwstat == 1 & (r6iwstat == 1 | r6iwstat == 4))
                                      
health_vars <- share_final[, c( "r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke", "r1bmi",  "r6cancre", "r6diabe", "r6hearte", "r6rxheart", "r6hibpe", "r6lunge", "r6stroke", "r6bmi")]
health_varsr1 <- c( "r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke", "r1bmi")

#-------------------------------------------------------------------------------
#                       Descriptive Stats by Age Classes
#-------------------------------------------------------------------------------

age_vars1and6 <- share_final[,c("r1agey", "r6agey")]
age_varsall <- share_final[,c("r1agey","r2agey", "r4agey", "r5agey", "r6agey")]
age_breaks <- seq(50, 100, by = 10)

# Loop through each wave and create decennial age classes
waves_to_include <- c("r1agey", "r2agey", "r4agey", "r5agey", "r6agey")
share_final[paste0(waves_to_include, "_class")] <- lapply(share_final[waves_to_include], function(x) cut(x, breaks = c(age_breaks, Inf), labels = FALSE, right = FALSE))
# Specify labels for age classes
age_class_labels <- c("50-59", "60-69", "70-79", "80-89", "90+")

# Adjust the breaks to have the same length as labels
age_breaks <- c(50, 60, 70, 80, 90, Inf)

# Loop through each wave and create decennial age classes with labels
share_final[paste0(waves_to_include, "_classlab")] <- lapply(share_final[waves_to_include], function(x) cut(x, breaks = age_breaks, labels = age_class_labels, right = FALSE))


output_folder <- "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/SHARE/output/age_vars_stats"
pdf(file.path(output_folder, "age_vars_stats.pdf"))
boxplot(age_vars1and6)
boxplot(age_varsall)
sink(file.path(output_folder, "summary_output.txt"))
summary(age_varsall)
summary(share_final$r1agey_classlab)
summary(share_final$r2agey_classlab)
summary(share_final$r4agey_classlab)
summary(share_final$r5agey_classlab)
summary(share_final$r6agey_classlab)
sink()
summary_output <- readLines(file.path(output_folder, "summary_output.txt"))
cat(paste(summary_output, collapse = "\n"), file = file.path(output_folder, "age_vars_stats.pdf"), append = TRUE)
dev.off()


# Add disease status variable for r1cancre
share_final$r1cancre_disease <- ifelse(share_final$r1cancre %in% c(1, 2), 1, 0)

# Loop through each wave and create decennial age classes
for (wave_var in waves_to_include) {
  unique_breaks <- unique(c(age_breaks, Inf))
  share_final[paste0(wave_var, "_class")] <- cut(share_final[[wave_var]], breaks = unique_breaks, labels = FALSE, right = FALSE)
}

# Specify labels for age classes
age_class_labels <- c("50-59", "60-69", "70-79", "80-89", "90+")

# Loop through each wave and create decennial age classes with labels
for (wave_var in waves_to_include) {
  unique_breaks <- unique(c(age_breaks, Inf))
  share_final[paste0(wave_var, "_classlab")] <- cut(share_final[[wave_var]], breaks = unique_breaks, labels = age_class_labels, right = FALSE)
}

# Calculate prevalence for r1cancre and age class
prevalence_r1cancre <- tapply(share_final$r1cancre_disease, share_final[[paste0(waves_to_include[1], "_class")]], mean)

# Print the results for r1cancre
prevalence_df_r1cancre <- data.frame(Age_Class = age_class_labels, Prevalence_r1cancre = prevalence_r1cancre)
print(prevalence_df_r1cancre)

health_vars_other_than_bmi <- c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke",
                               "r6cancre", "r6diabe", "r6hearte", "r6rxheart", "r6hibpe", "r6lunge", "r6stroke")


sink(file.path(output_folder, "prevalence_output.txt"))

# Loop through each health variable
for (disease_var in health_vars_other_than_bmi) {
  # Add disease status variable
  share_final[paste0(disease_var, "_disease")] <- ifelse(share_final[[disease_var]] %in% c(1, 2), 1, 0)
  
  # Loop through each wave and create decennial age classes
  for (wave_var in waves_to_include) {
    unique_breaks <- unique(c(age_breaks, Inf))
    share_final[paste0(wave_var, "_class")] <- cut(share_final[[wave_var]], breaks = unique_breaks, labels = FALSE, right = FALSE)
  }
  
  # Specify labels for age classes
  age_class_labels <- c("50-59", "60-69", "70-79", "80-89", "90+")
  
  # Loop through each wave and create decennial age classes with labels
  for (wave_var in waves_to_include) {
    unique_breaks <- unique(c(age_breaks, Inf))
    share_final[paste0(wave_var, "_classlab")] <- cut(share_final[[wave_var]], breaks = unique_breaks, labels = age_class_labels, right = FALSE)
  }
  
  # Calculate prevalence for each health variable and age class
  prevalence_results <- tapply(share_final[[paste0(disease_var, "_disease")]], share_final[[paste0(waves_to_include[1], "_class")]], mean)
  
  # Print the results for each health variable
  prevalence_df <- data.frame(Age_Class = age_class_labels, Prevalence = prevalence_results)
  cat(paste("Prevalence for", disease_var), "\n")
  print(prevalence_df)
  cat("\n")
}

# Close the text file
sink()

# Select the health variable of interest
disease_var <- "r1cancre"

# Combine disease prevalence data into a long format for ggplot
prevalence_long <- tidyr::gather(prevalence_df, key = "Disease", value = "Prevalence", -Age_Class)

# Loop through each disease variable
for (disease_var in health_vars_other_than_bmi) {
  
  # Create a bar plot for the prevalence of the current disease across age classes
  plot_data <- share_final %>%
    filter(!is.na(get(paste0(waves_to_include[1], "_classlab"))), !is.na(get(paste0(disease_var, "_disease")))) %>%
    ggplot(aes(x = as.factor(get(paste0(waves_to_include[1], "_classlab"))), fill = factor(get(paste0(disease_var, "_disease"))))) +
    geom_bar(position = "stack", stat = "count") +
    labs(title = paste("Prevalence of", disease_var, "across Age Classes"),
         x = "Age Class", y = "Count",
         fill = "Disease Status") +
    scale_fill_manual(values = c("0" = "aquamarine3", "1" = "lightblue3"), labels = c("No Disease", "Disease")) +
    theme_minimal()
  
  # Save each plot as a separate file
  ggsave(file.path(output_folder, paste0("plot_", disease_var, ".pdf")), plot_data)
}
pdf(file.path(output_folder, "bmi.pdf"))
ggplot(share_final, aes(x = "", y = r1bmi)) +
  geom_boxplot(fill = "aquamarine3") +
  labs(title = "Boxplot for r1bmi", y = "r1bmi") +
  theme_minimal()

ggplot(share_final, aes(x = "", y = r6bmi)) +
  geom_boxplot(fill = "aquamarine3") +
  labs(title = "Boxplot for r6bmi", y = "r6bmi") +
  theme_minimal()

sink(file.path(output_folder,"bmi.txt"))
summary(share_final$r1bmi, na.rm = TRUE)
summary(share_final$r6bmi, na.rm = TRUE)
sink()

dev.off()

cov_obs <- share_final %>%
  select(r1agey, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke, r1bmi, r6agey, r6cancre, r6diabe, r6hearte, r6hibpe, r6lunge, r6stroke, r6bmi) %>%
  na.omit() %>%
  cov()

print(cov_obs)

covarmat <- cov2cor(cov_obs)
covarmat
covarmatdat <- as.data.frame(covarmat)

output_path <- file.path(output_folder, "var_cov_matrix.xlsx")

# Write the data frame to the Excel file in the specified folder
write_xlsx(covarmatdat, output_path)

corrplot <- ggpairs(select(share_final,r1agey, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke, r1bmi, r6agey, r6cancre, r6diabe, r6hearte, r6hibpe, r6lunge, r6stroke, r6bmi ), lower = list(continuous = "smooth"))
ggsave(file.path(output_folder, "ggpairs_plot.png"), corrplot,  width = 16, height = 16)

#-------------------------------------------------------------------------------
#                       Descriptive Stats by Gender
#-------------------------------------------------------------------------------
sink(file.path(output_folder, "summary_gender.txt"))
summary_statistics <- share_final %>%
  group_by(ragender) %>%
  select(r1agey, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke, r1bmi, r6agey, r6cancre, r6diabe, r6hearte, r6hibpe, r6lunge, r6stroke, r6bmi) %>%
  na.omit() %>%
  skim()

print(summary_statistics)
sink()

# Create a bar plot for the prevalence of each disease variable across gender
for (disease_var in health_vars_other_than_bmi) {
  
  # Create a bar plot for the prevalence of the current disease across gender
  plot_data_gender <- share_final %>%
    filter(!is.na(ragender), !is.na(get(paste0(disease_var, "_disease")))) %>%
    ggplot(aes(x = as.factor(ragender), fill = factor(get(paste0(disease_var, "_disease"))))) +
    geom_bar(position = "stack", stat = "count") +
    labs(title = paste("Prevalence of", disease_var, "across Gender"),
         x = "Gender", y = "Count",
         fill = "Disease Status") +
    scale_fill_manual(values = c("0" = "aquamarine3", "1" = "lightblue3"), labels = c("No Disease", "Disease")) +
    theme_minimal()
  
  # Save each plot as a separate file
  ggsave(file.path(output_folder, paste0("plot_gender_", disease_var, ".pdf")), plot_data_gender)
}

# Print the summary statistics for each health variable by gender
summary_statistics_gender <- share_final %>%
  group_by(ragender) %>%
  select(r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke, r6cancre, r6diabe, r6hearte, r6rxheart, r6hibpe, r6lunge, r6stroke) %>%
  na.omit() %>%
  skim()

print(summary_statistics_gender)

# Create a boxplot for BMI by gender
pdf(file.path(output_folder, "bmi_gender.pdf"))
ggplot(share_final, aes(x = as.factor(ragender), y = r1bmi)) +
  geom_boxplot(fill = "aquamarine3") +
  labs(title = "Boxplot for r1bmi by Gender", y = "r1bmi") +
  theme_minimal()

ggplot(share_final, aes(x = as.factor(ragender), y = r6bmi)) +
  geom_boxplot(fill = "aquamarine3") +
  labs(title = "Boxplot for r6bmi by Gender", y = "r6bmi") +
  theme_minimal()
dev.off()

# Print summary statistics for BMI by gender
sink(file.path(output_folder, "bmi_gender.txt"))
summary_bmi_gender <- share_final %>%
  group_by(ragender) %>%
  select(r1bmi, r6bmi) %>%
  na.omit() %>%
  skim()

print(summary_bmi_gender)
sink()


# Covariance matrix by gender
cov_obs_gender <- share_final %>%
  group_by(ragender) %>%
  select(r1agey, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke, r1bmi, r6agey, r6cancre, r6diabe, r6hearte, r6hibpe, r6lunge, r6stroke, r6bmi) %>%
  na.omit() %>%
  cov()

# Print the covariance matrix by gender
print(cov_obs_gender)

# Convert covariance matrix to correlation matrix
corrmat_gender <- cov2cor(cov_obs_gender)
corrmat_genderdat <- as.data.frame(corrmat_gender)

output_path <- file.path(output_folder, "var_cov_matrix_gender.xlsx")
# Save the covariance matrix to an Excel file
write_xlsx(corrmat_genderdat, output_path)

# Save the correlation plot
corrplot_gender <- ggpairs(select(share_final, r1agey, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke, r1bmi, r6agey, r6cancre, r6diabe, r6hearte, r6hibpe, r6lunge, r6stroke, r6bmi, ragender), lower = list(continuous = "smooth"))
ggsave(file.path(output_folder, "ggpairs_plot_gender.png"), corrplot_gender, width = 16, height = 16)

