remove(list=ls())
gc()
# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(sharp)
library(tabulator)
library(openxlsx)
library(fastDummies)
#WINHOME
setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ScriptRA")
load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/MEMSHARECOMPLETE.RData")
#SERVER
#setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ScriptRA")
#load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/MEMSHARECOMPLETE.RData")
# # Read SHARE dataset
#sharedata <- read_dta("output/H_SHARE_f.dta")
#-------------------------------------------------------------------------------
#                               Data prep
#-------------------------------------------------------------------------------
# Specify new age breaks with 5-year span
age_breaks <- seq(28, 100, by = 5)

# Loop through each wave and create 5-year age classes
waves_to_include <- c("r1agey", "r2agey", "r4agey", "r5agey", "r6agey")
sharedata[paste0(waves_to_include, "_class")] <- lapply(sharedata[waves_to_include], function(x) cut(x, breaks = c(age_breaks, Inf), labels = FALSE, right = FALSE))

# Specify new labels for 5-year age classes
age_class_labels <- c("28-32", "33-37", "38-42", "43-47", "48-52", "53-57", "58-62", "63-67", "68-72", "73-77", "78-82", "83-87", "88-92", "93-97", "98-100")

# Adjust the breaks to have the same length as labels
age_breaks <- c(28, 33, 38, 43, 48, 53, 58, 63, 68, 73, 78, 83, 88, 93, 98, Inf)

# Loop through each wave and create 5-year age classes with labels
sharedata[paste0(waves_to_include, "_classlab")] <- lapply(sharedata[waves_to_include], function(x) cut(x, breaks = age_breaks, labels = age_class_labels, right = FALSE))

summary(sharedata$r1agey_classlab)
summary(sharedata$r2agey_classlab)
summary(sharedata$r4agey_classlab)
summary(sharedata$r5agey_classlab)
summary(sharedata$r6agey_classlab)

sharedata %>% tab(c2003cpindex)
sharedata %>% tab(r6hownrnt)


health_vars <- sharedata[, c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", 
                             "r1lunge", "r1stroke", "r1bmi", "r6bmi", 
                             "r6cancre", "r6diabe", "r6hearte", "r6hibpe",
                             "r6lunge", "r6stroke","r1gripsum", "r6gripsum")]
age_vars <- sharedata[, c("r1agey", "r6agey")]
behaviour_vars <- sharedata[,c( "r1vgactx", "r6vgactx", "r1mdactx", "r6mdactx", 
                                "r1smokev","r6smokev", "r1smoken", "r1drinkx",
                                "r6drinkb", "r6drinkn7")]
insurance_vars <- sharedata[, c("r1higov", "r1hiothp", "r1hident", "r1hidrug", "r1hiltc")]
educ_vars <- as.data.frame(sharedata$raeducl)
labor_vars <- sharedata[, c("r1slfemp", "r1jlasty", "r1jnace", "r1jisco", 
                            "r6slfemp",  "r6jlasty",  "r6jnace", "r6jisco", "r1lbrf_s"
                            ,"r6lbrf_s")]
wealth_vars <- sharedata[, c("r1hownrnt", "r6hownrnt") ]

missing_proportion_health <- colMeans(is.na(health_vars))

missing_proportion_age <- colMeans(is.na(age_vars))

missing_proportion_behaviour <- colMeans(is.na(behaviour_vars))

missing_proportion_insurance <- colMeans(is.na(insurance_vars))

missing_proportion_educ <- colMeans(is.na(educ_vars))

missing_proportion_labor <- colMeans(is.na(labor_vars))

missing_proportion_wealth <- colMeans(is.na(wealth_vars))

# Display the results
missing_proportion_health
missing_proportion_age
missing_proportion_behaviour
missing_proportion_insurance
missing_proportion_educ
missing_proportion_labor
missing_proportion_wealth

# Define the country codes to retain
desired_countries <- c("IT", "FR", "ES", "DE", "SE", "DK", "FI")
# Filter the dataset based on the MERGEID variable and desired country codes
sharedata_filtered <- sharedata %>%
  filter(substr(sharedata$mergeid, 1, 2) %in% desired_countries)

# Filter the dataset based on the conditions for r1iwstat and r6iwstat, rather than using isalive
share_final <- sharedata_filtered %>%
  filter(r1iwstat == 1 & (r6iwstat == 1 | r6iwstat == 4))

age_breaks <- seq(50, 100, by = 10)
# Loop through each wave and create 5-year age classes
waves_to_include <- c("r1agey", "r2agey", "r4agey", "r5agey", "r6agey")
share_final[paste0(waves_to_include, "_class")] <- lapply(share_final[waves_to_include],
                                                          function(x) cut(x, breaks = c(age_breaks, Inf), 
                                                                          labels = FALSE, right = FALSE))

# Specify new labels for 10-year age classes
age_class_labels <- c("50-59", "60-69", "70-79", "80-89", "90-99", "100+")

# Adjust the breaks to have the same length as labels
age_breaks <- c(50, 60, 70, 80, 90, 100, Inf)

# Loop through each wave and create 5-year age classes with labels
share_final[paste0(waves_to_include, "_classlab")] <- lapply(share_final[waves_to_include],
                                                             function(x) cut(x, breaks = age_breaks,
                                                                             labels = age_class_labels,
                                                                             right = FALSE))

share_small <- share_final %>%
  select(mergeid, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke,
         r6cancre, r6diabe, r6hearte, r6hibpe, r6lunge, r6stroke, r1agey,
         r1agey_classlab, r1agey_class, r6agey, r6agey_classlab, 
         r1higov, r1hiothp, r1hident, r1hidrug, r1hiltc, raeducl, r1slfemp,
         r1jlasty, r1jnace, r1jisco, r6slfemp, r6jlasty,  r6jnace, r6jisco, 
         r1hownrnt, r6hownrnt, r1iearn, r6itearn, c2003cpindex, c2012cpindex, 
         r1bmi, r6bmi, r1vgactx, r6vgactx, r1mdactx, r6mdactx, r1smokev,
         r6smokev, r1smoken, r6smoken, r1drinkx, r6drinkb, r6drinkn7, r1gripsum, 
         r6gripsum, r1lbrf_s, r6lbrf_s, h1rcany, h6rcany, r6rcany, r1adlfive, r1shopa,
         r1mealsa, r1medsa, r1moneya, r6adlfive, r6shopa, r6mealsa, r6medsa, 
         r6moneya, r1ipena, r6itpena, ragender, hh1hhres, hh6hhres, h1child, h6child, r1wtsamp, r6wtsamp )%>%
  filter(r1agey >= 50) %>%
  filter(r6agey >= 50)
 
health_vars <- share_small[, c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", 
                             "r1lunge", "r1stroke", "r1bmi", "r6bmi", 
                             "r6cancre", "r6diabe", "r6hearte", "r6hibpe",
                             "r6lunge", "r6stroke","r1gripsum", "r6gripsum")]
age_vars <- share_small[, c("r1agey", "r6agey")]
behaviour_vars <- share_small[,c( "r1vgactx", "r6vgactx", "r1mdactx", "r6mdactx", 
                                "r1smokev","r6smokev", "r1smoken", "r1drinkx",
                                "r6drinkb", "r6drinkn7")]
insurance_vars <- share_small[, c("r1higov", "r1hiothp", "r1hident", "r1hidrug", "r1hiltc")]
educ_vars <- as.data.frame(share_small$raeducl)
labor_vars <- share_small[, c("r1slfemp", "r1jlasty", "r1jnace", "r1jisco",  
                              "r6slfemp",  "r6jlasty",  "r6jnace", "r6jisco", "r1lbrf_s",
                              "r6lbrf_s")]
wealth_vars <- share_small[, c("r1hownrnt", "r6hownrnt") ]

missing_proportion_health <- colMeans(is.na(health_vars))

missing_proportion_age <- colMeans(is.na(age_vars))

missing_proportion_behaviour <- colMeans(is.na(behaviour_vars))

missing_proportion_insurance <- colMeans(is.na(insurance_vars))

missing_proportion_educ <- colMeans(is.na(educ_vars))

missing_proportion_labor <- colMeans(is.na(labor_vars))

missing_proportion_wealth <- colMeans(is.na(wealth_vars))

# Display the results
missing_proportion_health
missing_proportion_age
missing_proportion_behaviour
missing_proportion_insurance
missing_proportion_educ
missing_proportion_labor
missing_proportion_wealth

# Convert income for 2002 to constant prices
share_small$r1iearn_constant <- (share_small$r1iearn / share_small$c2003cpindex) * 100
share_small$r1ipena_constant <- (share_small$r1ipena / share_small$c2003cpindex) * 100
# Convert income for 2012 to constant prices
share_small$r6itearn_constant <- (share_small$r6itearn / share_small$c2003cpindex) * 100
share_small$r6itpena_constant <- (share_small$r6itpena / share_small$c2003cpindex) * 100


#Thank you for using fastDummies!
#To acknowledge our work, please cite the package:
#Kaplan, J. & Schlegel, B. (2023). fastDummies: Fast Creation of Dummy (Binary)
#Columns and Rows from Categorical Variables. Version 1.7.1. URL: 
#https://github.com/jacobkap/fastDummies, https://jacobkap.github.io/fastDummies/.

# Create dummy variables for education
share_small <- dummy_cols(share_small, select_columns = 'raeducl')

share_small <- rename(share_small,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                      tertiary_educ = raeducl_3)
#-----------------------------------
# income brackets
#-----------------------------------
# Create dummy variables for home ownership in wave 1
share_small <- dummy_cols(share_small, select_columns = 'r1hownrnt')
# Create dummy variables for home ownership in wave 6
share_small <- dummy_cols(share_small, select_columns = 'r6hownrnt')

# Create variable that captures employment earnings e pension earnings
share_small$r1_earnings <- share_small$r1iearn_constant + share_small$r1ipena_constant
share_small$r6_earnings <- share_small$r6itearn_constant + share_small$r6itpena_constant
# Define the income brackets based on the percentiles
share_small$r1_low_income <- ifelse(share_small$r1iearn_constant <= 18000, 1, 0)
share_small$r1_middle_income <- ifelse(share_small$r1iearn_constant > 18000 & 
                                         share_small$r1iearn_constant <= 35438.81, 1, 0)
share_small$r1_high_income <- ifelse(share_small$r1iearn_constant > 35438.81, 1, 0)
# Define the income brackets based on the percentiles
share_small$r6_low_income <- ifelse(share_small$r6itearn_constant <= 18000, 1, 0)
share_small$r6_middle_income <- ifelse(share_small$r6itearn_constant > 18000 
                                       & share_small$r6itearn_constant <= 35438.81, 1, 0)
share_small$r6_high_income <- ifelse(share_small$r6itearn_constant > 35438.81, 1, 0)

# Define the BMI categories based on the WHO classification for wave 1
share_small$r1_underweight <- ifelse(share_small$r1bmi < 18.5, 1, 0)
share_small$r1_normal_weight <- ifelse(share_small$r1bmi >= 18.5 & share_small$r1bmi < 25, 1, 0)
share_small$r1_overweight <- ifelse(share_small$r1bmi >= 25 & share_small$r1bmi < 30, 1, 0)
share_small$r1_obese <- ifelse(share_small$r1bmi >= 30, 1, 0)

# Define the BMI categories based on the WHO classification for wave 6 
share_small$r6_underweight <- ifelse(share_small$r6bmi < 18.5, 1, 0)
share_small$r6_normal_weight <- ifelse(share_small$r6bmi >= 18.5 & share_small$r6bmi < 25, 1, 0)
share_small$r6_overweight <- ifelse(share_small$r6bmi >= 25 & share_small$r6bmi < 30, 1, 0)
share_small$r6_obese <- ifelse(share_small$r6bmi >= 30, 1, 0)


share_small$r1morbi <- rowSums(share_small[,c("r1cancre", "r1diabe", "r1hearte", "r1hibpe", "r1lunge", "r1stroke")])

# Convert morbi to a binary variable where 0 indicates only one disease and 1 indicates more than one disease
share_small$r1morbi <- ifelse(share_small$r1morbi > 1, 1, 0)
share_small %>% tab(r1morbi)

share_small$r6morbi <- rowSums(share_small[,c("r6cancre", "r6diabe", "r6hearte", "r6hibpe", "r6lunge", "r6stroke")])

# Convert morbi to a binary variable where 0 indicates only one disease and 1 indicates more than one disease
share_small$r6morbi <- ifelse(share_small$r6morbi > 1, 1, 0)
share_small %>% tab(r6morbi)

# Create a bar plot for r1morbi
table_r1morbi <- table(share_small$r1morbi)
barplot(table_r1morbi, main="Bar Plot of r1morbi", xlab="Value", ylab="Frequency")

# Create a bar plot for r6morbi
table_r6morbi <- table(share_small$r6morbi)
barplot(table_r6morbi, main="Bar Plot of r6morbi", xlab="Value", ylab="Frequency")

# Create dummies for low/high skill, blue/white collar workers for wave 1

share_small$r1_high_white_collar <- ifelse(share_small$r1jisco %in% c(1,2,3), 1, 0)
share_small$r1_low_white_collar <- ifelse(share_small$r1jisco %in% c(4,5), 1, 0)
share_small$r1_high_blue_collar <- ifelse(share_small$r1jisco %in% c(6,7), 1, 0)
share_small$r1_low_blue_collar <- ifelse(share_small$r1jisco %in% c(8,9), 1, 0)
share_small$r61_retired <- ifelse(share_small$r1lbrf_s == 5, 1, 0)
# Create a new variable that captures if an individual is employed
share_small$r1_employed <- ifelse(share_small$r1lbrf_s == 1, 1, 0)
share_small %>% tab(r1_employed)
share_small$r1_retired <- ifelse(share_small$r1lbrf_s == 5, 1, 0)
share_small %>% tab(r1_retired)
share_small$r1_unemployed <- ifelse(share_small$r1lbrf_s == 3, 1, 0)
share_small %>% tab(r1_unemployed)


# Create new dummy variables
share_small$emp_r1_high_white_collar <- ifelse(share_small$r1_employed == 1 & share_small$r1_high_white_collar == 1, 1, 0)
share_small$emp_r1_low_white_collar <- ifelse(share_small$r1_employed == 1 & share_small$r1_low_white_collar == 1, 1, 0)
share_small$emp_r1_high_blue_collar <- ifelse(share_small$r1_employed == 1 & share_small$r1_high_blue_collar == 1, 1, 0)
share_small$emp_r1_low_blue_collar <- ifelse(share_small$r1_employed == 1 & share_small$r1_low_blue_collar == 1, 1, 0)


share_small %>% tab(emp_r1_high_white_collar)
share_small %>% tab(r1_high_white_collar)
share_small %>% tab(emp_r1_low_white_collar)
share_small %>% tab(r1_low_white_collar)
share_small %>% tab(emp_r1_high_blue_collar)
share_small %>% tab(r1_high_blue_collar)
share_small %>% tab(emp_r1_low_blue_collar)
share_small %>% tab(r1_low_blue_collar)

# Create dummies for low/high skill, blue/white collar workers for wave 6

share_small$r6_high_white_collar <- ifelse(share_small$r6jisco %in% c(1,2,3), 1, 0)
share_small$r6_low_white_collar <- ifelse(share_small$r6jisco %in% c(4,5), 1, 0)
share_small$r6_high_blue_collar <- ifelse(share_small$r6jisco %in% c(6,7), 1, 0)
share_small$r6_low_blue_collar <- ifelse(share_small$r6jisco %in% c(8,9), 1, 0)

# Create a new variable that captures if an individual is employed
share_small$r6_employed <- ifelse(share_small$r6lbrf_s == 1, 1, 0)
share_small$r6_retired <- ifelse(share_small$r6lbrf_s == 5, 1, 0)
share_small %>% tab(r6_retired)
share_small$r6_unemployed <- ifelse(share_small$r6lbrf_s == 3, 1, 0)
share_small %>% tab(r6_unemployed)


# Create new dummy variables
share_small$emp_r6_high_white_collar <- ifelse(share_small$r6_employed == 1 & share_small$r6_high_white_collar == 1, 1, 0)
share_small$emp_r6_low_white_collar <- ifelse(share_small$r6_employed == 1 & share_small$r6_low_white_collar == 1, 1, 0)
share_small$emp_r6_high_blue_collar <- ifelse(share_small$r6_employed == 1 & share_small$r6_high_blue_collar == 1, 1, 0)
share_small$emp_r6_low_blue_collar <- ifelse(share_small$r6_employed == 1 & share_small$r6_low_blue_collar == 1, 1, 0)
share_small$r6_retired <- ifelse(share_small$r6lbrf_s == 5, 1, 0)

share_small %>% tab(emp_r6_high_white_collar)
share_small %>% tab(r6_high_white_collar)
share_small %>% tab(emp_r6_low_white_collar)
share_small %>% tab(r6_low_white_collar)
share_small %>% tab(emp_r6_high_blue_collar)
share_small %>% tab(r6_high_blue_collar)
share_small %>% tab(emp_r6_low_blue_collar)
share_small %>% tab(r6_low_blue_collar)

#drink behaviors wave 1
share_small$r1_drink_NO <- ifelse(share_small$r1drinkx == 0, 1, 0) #does not drink
share_small$r1_drink_OTS <- ifelse(share_small$r1drinkx == 2, 1, 0) #drink one to several times a week
share_small$r1_drink_MDW <- ifelse(share_small$r1drinkx == 3, 1, 0) #drink most days of the week
share_small$r1_drink_EDW <- ifelse(share_small$r1drinkx == 4, 1, 0) #drink every day of the week

#drink behaviors wave 6
#does not drink
share_small$r6_drink_NO <- ifelse(share_small$r6drinkn7 == 0, 1, 0) 

#drink one to several times a week
share_small$r6_drink_OTS <- ifelse(share_small$r6drinkn7 >= 1 & share_small$r6drinkn7 <= 6, 1, 0) 

#drink most days of the week
share_small$r6_drink_MDW <- ifelse(share_small$r6drinkn7 >= 4 & share_small$r6drinkn7 <= 6, 1, 0) 

#drink every day of the week
share_small$r6_drink_EDW <- ifelse(share_small$r6drinkn7 >= 7, 1, 0) 

# Create dummy variables for each category of r1adlfive
share_small$r1adlfive_0 <- ifelse(share_small$r1adlfive == 0, 1, 0) # No impedings
share_small$r1adlfive_1_2 <- ifelse(share_small$r1adlfive == 1 & share_small$r1adlfive == 2, 1, 0) # 1 or 2 impedings
share_small$r1adlfive_3_4_5 <- ifelse(share_small$r1adlfive == 3 & share_small$r1adlfive == 4 & share_small$r1adlfive == 5, 1, 0) # 3+ impedings

# Create dummy variables for each category of r1adlfive
share_small$r6adlfive_0 <- ifelse(share_small$r6adlfive == 0, 1, 0) # No impedings
share_small$r6adlfive_1_2 <- ifelse(share_small$r6adlfive == 1 & share_small$r6adlfive == 2, 1, 0) # 1 or 2 impedings
share_small$r6adlfive_3_4_5 <- ifelse(share_small$r6adlfive == 3 & share_small$r6adlfive == 4 & share_small$r6adlfive == 5, 1, 0) # 3+ impedings


#summary of IADLs for wave 1
share_small$r1iadl_0 <- ifelse(share_small$r1shopa == 0 & share_small$r1mealsa == 0
                              & share_small$r1medsa == 0 & share_small$r1moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_small$r1_iadl_sum <- share_small$r1shopa + share_small$r1mealsa + share_small$r1medsa + share_small$r1moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_small$r1_iadl_1_2 <- ifelse(share_small$r1_iadl_sum >= 1 & share_small$r1_iadl_sum <= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_small$r1_iadl_3_plus <- ifelse(share_small$r1_iadl_sum >= 3, 1, 0)

#summary of IADLs for wave 6
share_small$r6iadl_0 <- ifelse(share_small$r6shopa == 0 & share_small$r6mealsa == 0
                              & share_small$r6medsa == 0 & share_small$r6moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_small$r6_iadl_sum <- share_small$r6shopa + share_small$r6mealsa + share_small$r6medsa + share_small$r6moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_small$r6_iadl_1_2 <- ifelse(share_small$r6_iadl_sum >= 1 & share_small$r6_iadl_sum <= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_small$r6_iadl_3_plus <- ifelse(share_small$r6_iadl_sum >= 3, 1, 0)


#Disease summary
share_small$r1_healthy <- ifelse(share_small$r1cancre == 0 & share_small$r1hibpe == 0
                              & share_small$r1diabe == 0 & share_small$r1hearte ==0
                              & share_small$r1lunge ==0 & share_small$r1stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_small$r1_dis_sum <- share_small$r1cancre + share_small$r1hearte + share_small$r1hibpe + share_small$r1diabe +
  share_small$r1lunge + share_small$r1stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_small$r1_morbi_2 <- ifelse(share_small$r1_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_small$r1_morbi_3_plus <- ifelse(share_small$r1_dis_sum >= 3, 1, 0)

#Disease summary
share_small$r6_healthy <- ifelse(share_small$r6cancre == 0 & share_small$r6hibpe == 0
                              & share_small$r6diabe == 0 & share_small$r6hearte ==0
                              & share_small$r6lunge ==0 & share_small$r6stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_small$r6_dis_sum <- share_small$r6cancre + share_small$r6hearte + share_small$r6hibpe + share_small$r6diabe +
  share_small$r6lunge + share_small$r6stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_small$r6_morbi_2 <- ifelse( share_small$r6_dis_sum >= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_small$r6_morbi_3_plus <- ifelse(share_small$r6_dis_sum >= 3, 1, 0)

#-----------------------------------
# risky behaviors
#-----------------------------------
share_small$r1_drink_EV <- ifelse(share_small$r1drinkx >= 1, 1, 0) #ever drinks

share_small$r6_drink_EV <- ifelse(share_small$r6drinkn7 >= 1, 1, 0) #ever drinks
# For wave 1
share_small$r1_risky <- ifelse(share_small$r1smokev >= 1 | share_small$r1_drink_EV >= 1, 1, 0)

# For wave 6
share_small$r6_risky <- ifelse(share_small$r6smokev >= 1 | share_small$r6_drink_EV >= 1, 1, 0)

# For wave 1
share_small$r1_risky <- ifelse(is.na(share_small$r1_risky), 0, share_small$r1_risky)

# For wave 6
share_small$r6_risky <- ifelse(is.na(share_small$r6_risky), 0, share_small$r6_risky)

#write.csv(share_small, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/share_small.csv")

write_dta(share_small, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/share_small.dta")
#write_dta(share_small, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/share_small.dta")
