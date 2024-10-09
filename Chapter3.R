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
#----------------UPSET PLOTS----------------
  # Load necessary library
  library(UpSetR)
  
  # Function to generate and save UpSet plots
  create_upset_plot <- function(data, morbidities, col_names, plot_name, output_folder) {
    # Convert binary indicators to numeric
    morbidities_df <- as.data.frame(lapply(data[, morbidities], as.numeric))
    
    # Filter out rows where only one morbidity is present
    morbidities_df_filtered <- morbidities_df[rowSums(morbidities_df) > 1, ]
    
    # Rename columns to more readable names
    colnames(morbidities_df_filtered)[colnames(morbidities_df_filtered) %in% morbidities] <- col_names
    
    # Remove rows with NA values
    morbidities_df_filtered <- na.omit(morbidities_df_filtered)
    
    # Create the UpSet plot
    plot <- upset(morbidities_df_filtered, 
                  sets = col_names,
                  keep.order = TRUE, 
                  main.bar.color = "steelblue",
                  sets.bar.color = "darkred",
                  order.by = "freq",
                  sets.x.label = "Tot.Morbidity Count")
    
    # Create the output folder if it does not exist
    if (!dir.exists(output_folder)) {
      dir.create(output_folder)
    }
    
    # Define the file path for the plot
    output_file <- paste0(output_folder, "/", plot_name, ".pdf")
    
    # Save the plot as a PDF
    pdf(file = output_file, width = 11, height = 8.5) # Adjust the size as needed
    print(plot)
    dev.off()
    
    # Inform the user
    cat("Plot saved as:", output_file, "\n")
  }
  
  output_folder <- paste0(base_path, "/Clustering/Chapter3/")
  
  # Creating plots for all datasets
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
  
  # SHARE Wave 1
  create_upset_plot(share_w1, c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "SHARE_W1", output_folder)
  
  # SHARE Wave 6
  create_upset_plot(share_w6, c("r6cancre", "r6diabe", "r6hearte", "r6hibpe", "r6lunge", "r6stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "SHARE_W6", output_folder)
  
  # HRS Wave 7
  create_upset_plot(hrs_w7, c("r7cancre", "r7diabe", "r7hearte", "r7hibpe", "r7lunge", "r7stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "HRS_W7", output_folder)
  
  # HRS Wave 12
  create_upset_plot(hrs_w12, c("r12cancre", "r12diabe", "r12hearte", "r12hibpe", "r12lunge", "r12stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "HRS_W12", output_folder)
  
  # ELSA Wave 2
  create_upset_plot(elsa_w2, c("r2cancre", "r2diabe", "r2hearte", "r2hibpe", "r2lunge", "r2stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "ELSA_W2", output_folder)
  
  # ELSA Wave 8
  create_upset_plot(elsa_w8, c("r8cancre", "r8diabe", "r8hearte", "r8hibpe", "r8lunge", "r8stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "ELSA_W8", output_folder)
  
  # KLOSA Wave 3
  create_upset_plot(klosa_w3, c("r3cancre", "r3diabe", "r3hearte", "r3hibpe", "r3lunge", "r3stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "KLOSA_W3", output_folder)
  
  # KLOSA Wave 8
  create_upset_plot(klosa_w8, c("r8cancre", "r8diabe", "r8hearte", "r8hibpe", "r8lunge", "r8stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "KLOSA_W8", output_folder)
  
  # CHARLS Wave 1
  create_upset_plot(charls_w1, c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "CHARLS_W1", output_folder)
  
  # CHARLS Wave 4
  create_upset_plot(charls_w4, c("r4cancre", "r4diabe", "r4hearte", "r4hibpe", "r4lunge", "r4stroke"), 
                    c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                    "CHARLS_W4", output_folder)
  
  # MHAS Wave 2
  create_upset_plot(mhas_w2, c("r2cancre", "r2diabe", "r2respe", "r2hibpe", "r2stroke"), 
                    c("Cancer", "Diabetes", "Lung Disease", "High Blood Pressure", "Stroke"), 
                    "MHAS_W2", output_folder)
  
  # MHAS Wave 4
  create_upset_plot(mhas_w4, c("r4cancre", "r4diabe", "r4respe", "r4hibpe", "r4stroke"), 
                    c("Cancer", "Diabetes", "Lung Disease", "High Blood Pressure", "Stroke"), 
                    "MHAS_W4", output_folder)
  
  # Inform the user that all plots are saved
  cat("All plots have been saved as separate PDF files in the folder:", output_folder, "\n")
  

#----------------Weighted PLOTS----------------
wshare_w1 <- read_dta(paste0(base_path, "SHARE/output/weightedSHARE.dta"))
wshare_w6 <- read_dta(paste0(base_path, "SHARE/output/weightedSHAREW6.dta"))
whrs_w7 <- read_dta(paste0(base_path, "HRS/output/weightedHRSW7.dta"))
whrs_w12 <- read_dta(paste0(base_path, "HRS/output/weightedHRSW12.dta"))
welsa_w2 <- read_dta(paste0(base_path, "ELSA/UKDA-5050-stata/output/weightedELSAW2.dta"))
welsa_w8 <- read_dta(paste0(base_path, "ELSA/UKDA-5050-stata/output/weightedELSAW8.dta"))
wklosa_w3 <- read_dta(paste0(base_path, "KLOSA/output/weightedKLOSAW3.dta"))
wklosa_w8 <- read_dta(paste0(base_path, "KLOSA/output/weightedKLOSAW8.dta"))
wcharls_w1 <- read_dta(paste0(base_path, "CHARLS/output/weightedCHARLSW1.dta"))
wcharls_w4 <- read_dta(paste0(base_path, "CHARLS/output/weightedCHARLSW4.dta"))
wmhas_w2 <- read_dta(paste0(base_path, "MHAS/output/weightedMHASW2.dta"))
wmhas_w4 <- read_dta(paste0(base_path, "MHAS/output/weightedMHASW4.dta"))
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


# Function to generate and save UpSet plots with weighted data
create_weighted_upset_plot <- function(data, morbidities, col_names, weight_col, plot_name, output_folder) {
  # Convert binary indicators to numeric
  morbidities_df <- as.data.frame(lapply(data[, morbidities], as.numeric))
  
  # Add the weight column to the morbidity dataframe
  morbidities_df$weights <- data[[weight_col]]
  
  # Handle NA and negative weights
  morbidities_df$weights[is.na(morbidities_df$weights)] <- 1  # Replace NA weights with 1
  morbidities_df$weights[morbidities_df$weights < 0] <- 1      # Replace negative weights with 1
  
  # Round weights to the nearest integer and divide by 1000
  morbidities_df$weights <- round(morbidities_df$weights / 1000)
  
  # Filter out rows where only one morbidity is present
  morbidities_df_filtered <- morbidities_df[rowSums(morbidities_df[, morbidities]) > 1, ]
  
  # Rename columns to more readable names
  colnames(morbidities_df_filtered)[colnames(morbidities_df_filtered) %in% morbidities] <- col_names
  
  # Remove rows with NA values
  morbidities_df_filtered <- na.omit(morbidities_df_filtered)
  
  # Repeat rows based on weights to simulate weighted UpSet plot
  morbidities_df_weighted <- morbidities_df_filtered[rep(1:nrow(morbidities_df_filtered), times = morbidities_df_filtered$weights), ]
  
  # Remove the weight column as it is no longer needed
  morbidities_df_weighted$weights <- NULL
  
  # Create the UpSet plot
  plot <- upset(morbidities_df_weighted, 
                sets = col_names,
                keep.order = TRUE, 
                main.bar.color =  "#f46d43" ,
                sets.bar.color = "#f89e75",
                order.by = "freq",
                sets.x.label = "Total Morbidity Count (in thousands)")
  
  # Create the output folder if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Define the file path for the plot
  output_file <- paste0(output_folder, "/", plot_name, "_weighted.pdf")
  
  # Save the plot as a PDF
  pdf(file = output_file, width = 11, height = 8.5) # Adjust the size as needed
  print(plot)
  dev.off()
  
  # Inform the user
  cat("Weighted plot saved as:", output_file, "\n")
}


output_folder <- paste0(base_path, "/Clustering/Chapter3/Weighted_Plots/")

# Creating weighted plots for SHARE Wave 1 as an example
create_weighted_upset_plot(wshare_w1, c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke"), 
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                           "r1wtsamp", "SHARE_W1", output_folder)
create_weighted_upset_plot(wshare_w6, c("r6cancre", "r6diabe", "r6hearte", "r6hibpe", "r6lunge", "r6stroke"), 
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                           "r6wtsamp", "SHARE_W6", output_folder)

# Function to generate and save UpSet plots with weighted data
create_weighted_upset_plot <- function(data, morbidities, col_names, weight_col, plot_name, output_folder) {
  # Convert binary indicators to numeric
  morbidities_df <- as.data.frame(lapply(data[, morbidities], as.numeric))
  
  # Add the weight column to the morbidity dataframe
  morbidities_df$weights <- data[[weight_col]]
  
  # Handle NA and negative weights
  morbidities_df$weights[is.na(morbidities_df$weights)] <- 1  # Replace NA weights with 1
  morbidities_df$weights[morbidities_df$weights < 0] <- 1      # Replace negative weights with 1
  
  # Round weights to the nearest integer and divide by 1000
  morbidities_df$weights <- round(morbidities_df$weights / 1000)
  
  # Filter out rows where only one morbidity is present
  morbidities_df_filtered <- morbidities_df[rowSums(morbidities_df[, morbidities]) > 1, ]
  
  # Rename columns to more readable names
  colnames(morbidities_df_filtered)[colnames(morbidities_df_filtered) %in% morbidities] <- col_names
  
  # Remove rows with NA values
  morbidities_df_filtered <- na.omit(morbidities_df_filtered)
  
  # Repeat rows based on weights to simulate weighted UpSet plot
  morbidities_df_weighted <- morbidities_df_filtered[rep(1:nrow(morbidities_df_filtered), times = morbidities_df_filtered$weights), ]
  
  # Remove the weight column as it is no longer needed
  morbidities_df_weighted$weights <- NULL
  
  # Create the UpSet plot
  plot <- upset(morbidities_df_weighted, 
                sets = col_names,
                keep.order = TRUE, 
                main.bar.color =  "#c1cfb9" ,
                sets.bar.color = "#d9e4c8",
                order.by = "freq",
                sets.x.label = "Total Morbidity Count (in thousands)")
  
  # Create the output folder if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Define the file path for the plot
  output_file <- paste0(output_folder, "/", plot_name, "_weighted.pdf")
  
  # Save the plot as a PDF
  pdf(file = output_file, width = 11, height = 8.5) # Adjust the size as needed
  print(plot)
  dev.off()
  
  # Inform the user
  cat("Weighted plot saved as:", output_file, "\n")
}

create_weighted_upset_plot(whrs_w7, c("r7cancre", "r7diabe", "r7hearte", "r7hibpe", "r7lunge", "r7stroke"), 
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                           "r7wtresp", "HRS_W7", output_folder)
create_weighted_upset_plot(whrs_w12, c("r12cancre", "r12diabe", "r12hearte", "r12hibpe", "r12lunge", "r12stroke"), 
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"), 
                           "r12wtresp", "HRS_W12", output_folder)
# create_weighted_upset_plot(welsa_w2, c("r2cancre", "r2diabe", "r2hearte", "r2hibpe", "r2lunge", "r2stroke"),
#                            c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"),
#                            "r2cwtresp", "ELSA_W2", output_folder)
# create_weighted_upset_plot(welsa_w8, c("r8cancre", "r8diabe", "r8hearte", "r8hibpe", "r8lunge", "r8stroke"),
#                            c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"),
#                            "r8cwtresp", "ELSA_W8", output_folder)


# Function to generate and save UpSet plots with weighted data
create_weighted_upset_plot <- function(data, morbidities, col_names, weight_col, plot_name, output_folder) {
  # Convert binary indicators to numeric
  morbidities_df <- as.data.frame(lapply(data[, morbidities], as.numeric))
  
  # Add the weight column to the morbidity dataframe
  morbidities_df$weights <- data[[weight_col]]
  
  # Handle NA and negative weights
  morbidities_df$weights[is.na(morbidities_df$weights)] <- 1  # Replace NA weights with 1
  morbidities_df$weights[morbidities_df$weights < 0] <- 1      # Replace negative weights with 1
  
  # Round weights to the nearest integer and divide by 1000
  morbidities_df$weights <- round(morbidities_df$weights / 1000)
  
  # Filter out rows where only one morbidity is present
  morbidities_df_filtered <- morbidities_df[rowSums(morbidities_df[, morbidities]) > 1, ]
  
  # Rename columns to more readable names
  colnames(morbidities_df_filtered)[colnames(morbidities_df_filtered) %in% morbidities] <- col_names
  
  # Remove rows with NA values
  morbidities_df_filtered <- na.omit(morbidities_df_filtered)
  
  # Repeat rows based on weights to simulate weighted UpSet plot
  morbidities_df_weighted <- morbidities_df_filtered[rep(1:nrow(morbidities_df_filtered), times = morbidities_df_filtered$weights), ]
  
  # Remove the weight column as it is no longer needed
  morbidities_df_weighted$weights <- NULL
  
  # Create the UpSet plot
  plot <- upset(morbidities_df_weighted, 
                sets = col_names,
                keep.order = TRUE, 
                main.bar.color =  "#f6d1a5" ,
                sets.bar.color = "#fde1c4",
                order.by = "freq",
                sets.x.label = "Total Morbidity Count (in thousands)")
  
  # Create the output folder if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Define the file path for the plot
  output_file <- paste0(output_folder, "/", plot_name, "_weighted.pdf")
  
  # Save the plot as a PDF
  pdf(file = output_file, width = 11, height = 8.5) # Adjust the size as needed
  print(plot)
  dev.off()
  
  # Inform the user
  cat("Weighted plot saved as:", output_file, "\n")
}
create_weighted_upset_plot(wklosa_w3, c("r3cancre", "r3diabe", "r3hearte", "r3hibpe", "r3lunge", "r3stroke"),
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"),
                           "r3wtresp", "KLOSA_W3", output_folder)
create_weighted_upset_plot(wklosa_w8, c("r8cancre", "r8diabe", "r8hearte", "r8hibpe", "r8lunge", "r8stroke"),
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"),
                           "r8wtresp", "KLOSA_W8", output_folder)

# Function to generate and save UpSet plots with weighted data
create_weighted_upset_plot <- function(data, morbidities, col_names, weight_col, plot_name, output_folder) {
  # Convert binary indicators to numeric
  morbidities_df <- as.data.frame(lapply(data[, morbidities], as.numeric))
  
  # Add the weight column to the morbidity dataframe
  morbidities_df$weights <- data[[weight_col]]
  
  # Handle NA and negative weights
  morbidities_df$weights[is.na(morbidities_df$weights)] <- 1  # Replace NA weights with 1
  morbidities_df$weights[morbidities_df$weights < 0] <- 1      # Replace negative weights with 1
  
  # Round weights to the nearest integer and divide by 1000
  morbidities_df$weights <- round(morbidities_df$weights / 1000)
  
  # Filter out rows where only one morbidity is present
  morbidities_df_filtered <- morbidities_df[rowSums(morbidities_df[, morbidities]) > 1, ]
  
  # Rename columns to more readable names
  colnames(morbidities_df_filtered)[colnames(morbidities_df_filtered) %in% morbidities] <- col_names
  
  # Remove rows with NA values
  morbidities_df_filtered <- na.omit(morbidities_df_filtered)
  
  # Repeat rows based on weights to simulate weighted UpSet plot
  morbidities_df_weighted <- morbidities_df_filtered[rep(1:nrow(morbidities_df_filtered), times = morbidities_df_filtered$weights), ]
  
  # Remove the weight column as it is no longer needed
  morbidities_df_weighted$weights <- NULL
  
  # Create the UpSet plot
  plot <- upset(morbidities_df_weighted, 
                sets = col_names,
                keep.order = TRUE, 
                main.bar.color =  "#519fcb" ,
                sets.bar.color = "#a1c6e1",
                order.by = "freq",
                sets.x.label = "Total Morbidity Count (in thousands)")
  
  # Create the output folder if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Define the file path for the plot
  output_file <- paste0(output_folder, "/", plot_name, "_weighted.pdf")
  
  # Save the plot as a PDF
  pdf(file = output_file, width = 11, height = 8.5) # Adjust the size as needed
  print(plot)
  dev.off()
  
  # Inform the user
  cat("Weighted plot saved as:", output_file, "\n")
}
create_weighted_upset_plot(wcharls_w1, c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke"),
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"),
                           "r1wtresp", "CHARLS_W1", output_folder)
create_weighted_upset_plot(wcharls_w4, c("r4cancre", "r4diabe", "r4hearte", "r4hibpe", "r4lunge", "r4stroke"),
                           c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke"),
                           "r4wtresp", "CHARLS_W4", output_folder)
# Function to generate and save UpSet plots with weighted data
create_weighted_upset_plot <- function(data, morbidities, col_names, weight_col, plot_name, output_folder) {
  # Convert binary indicators to numeric
  morbidities_df <- as.data.frame(lapply(data[, morbidities], as.numeric))
  
  # Add the weight column to the morbidity dataframe
  morbidities_df$weights <- data[[weight_col]]
  
  # Handle NA and negative weights
  morbidities_df$weights[is.na(morbidities_df$weights)] <- 1  # Replace NA weights with 1
  morbidities_df$weights[morbidities_df$weights < 0] <- 1      # Replace negative weights with 1
  
  # Round weights to the nearest integer and divide by 1000
  morbidities_df$weights <- round(morbidities_df$weights / 1000)
  
  # Filter out rows where only one morbidity is present
  morbidities_df_filtered <- morbidities_df[rowSums(morbidities_df[, morbidities]) > 1, ]
  
  # Rename columns to more readable names
  colnames(morbidities_df_filtered)[colnames(morbidities_df_filtered) %in% morbidities] <- col_names
  
  # Remove rows with NA values
  morbidities_df_filtered <- na.omit(morbidities_df_filtered)
  
  # Repeat rows based on weights to simulate weighted UpSet plot
  morbidities_df_weighted <- morbidities_df_filtered[rep(1:nrow(morbidities_df_filtered), times = morbidities_df_filtered$weights), ]
  
  # Remove the weight column as it is no longer needed
  morbidities_df_weighted$weights <- NULL
  
  # Create the UpSet plot
  plot <- upset(morbidities_df_weighted, 
                sets = col_names,
                keep.order = TRUE, 
                main.bar.color =  "#f09068" ,
                sets.bar.color = "#f6b794",
                order.by = "freq",
                sets.x.label = "Total Morbidity Count (in thousands)")
  
  # Create the output folder if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Define the file path for the plot
  output_file <- paste0(output_folder, "/", plot_name, "_weighted.pdf")
  
  # Save the plot as a PDF
  pdf(file = output_file, width = 11, height = 8.5) # Adjust the size as needed
  print(plot)
  dev.off()
  
  # Inform the user
  cat("Weighted plot saved as:", output_file, "\n")
}
create_weighted_upset_plot(wmhas_w2, c("r2cancre", "r2diabe", "r2respe", "r2hibpe", "r2stroke"),
                           c("Cancer", "Diabetes", "Lung Disease", "High Blood Pressure", "Stroke"),
                           "r2wtresp", "MHAS_W2", output_folder)
create_weighted_upset_plot(wmhas_w4, c("r4cancre", "r4diabe", "r4respe", "r4hibpe", "r4stroke"),
                           c("Cancer", "Diabetes", "Lung Disease", "High Blood Pressure", "Stroke"),
                           "r4wtresp", "MHAS_W4", output_folder)

# Load necessary libraries
library(UpSetR)

# Step 1: Define variables
morbidities <- c("r2cancre", "r2diabe", "r2hearte", "r2hibpe", "r2lunge", "r2stroke")
col_names <- c("Cancer", "Diabetes", "Heart Disease", "High Blood Pressure", "Lung Disease", "Stroke")
weight_col <- "r2cwtresp"
plot_name <- "ELSA_W2"
output_folder <- paste0(base_path, "/Clustering/Chapter3/Weighted_Plots/")

# Step 2: Convert binary indicators to numeric
morbidities_df <- as.data.frame(lapply(welsa_w2[, morbidities], as.numeric))

# Step 3: Add the weight column to the morbidity dataframe
morbidities_df$weights <- welsa_w2[[weight_col]]

# Step 4: Handle NA and negative weights
morbidities_df$weights[is.na(morbidities_df$weights)] <- 1  # Replace NA weights with 1
morbidities_df$weights[morbidities_df$weights < 0] <- 1      # Replace negative weights with 1

# Step 5: Round weights to the nearest integer and divide by 1000
morbidities_df$weights <- round(morbidities_df$weights / 1000)

# Step 6: Filter out rows where only one morbidity is present
morbidities_df_filtered <- morbidities_df[rowSums(morbidities_df[, morbidities]) > 1, ]

# Step 7: Rename columns to more readable names
colnames(morbidities_df_filtered)[colnames(morbidities_df_filtered) %in% morbidities] <- col_names

# Step 8: Remove rows with NA values
morbidities_df_filtered <- na.omit(morbidities_df_filtered)

# Step 9: Repeat rows based on weights to simulate weighted UpSet plot
morbidities_df_weighted <- morbidities_df_filtered[rep(1:nrow(morbidities_df_filtered), times = morbidities_df_filtered$weights), ]

# Step 10: Remove the weight column as it is no longer needed
morbidities_df_weighted$weights <- NULL

# Step 11: Create the UpSet plot
plot <- upset(morbidities_df_weighted, 
              sets = col_names,
              keep.order = TRUE, 
              main.bar.color = "steelblue",
              sets.bar.color = "darkred",
              order.by = "freq",
              sets.x.label = "Total Morbidity Count (in thousands)")

# Step 12: Create the output folder if it does not exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Step 13: Define the file path for the plot
output_file <- paste0(output_folder, "/", plot_name, "_weighted.pdf")

# Step 14: Save the plot as a PDF
pdf(file = output_file, width = 11, height = 8.5) # Adjust the size as needed
print(plot)
dev.off()

# Inform the user
cat("Weighted plot saved as:", output_file, "\n")

