remove(list=ls())
# Load required libraries
library("dplyr")
library("haven")
library("ggplot2")
library("sharp")
library("tabulator")
library("fastDummies")
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
# Specify new age breaks with 10-year span
age_breaks <- seq(50, 100, by = 10)

# Loop through each wave and create 5-year age classes
waves_to_include <- c("r1agey", "r2agey", "r4agey", "r5agey", "r6agey")
sharedata[paste0(waves_to_include, "_class")] <- lapply(sharedata[waves_to_include], function(x) cut(x, breaks = c(age_breaks, Inf), labels = FALSE, right = FALSE))

# Specify new labels for 5-year age classes
age_class_labels <- c("50-59", "60-69", "70-79", "80-89", "90-99", "100+")

# Adjust the breaks to have the same length as labels
age_breaks <- c(50, 60, 70, 80, 90, 100, Inf)

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
share_w1 <- sharedata_filtered %>%
  filter(r1iwstat == 1)
share_w6 <- sharedata_filtered %>%
  filter(r6iwstat == 1 | r6iwstat == 4)
# 


#age_breaks <- seq(50, 100, by = 10)
# Loop through each wave and create 5-year age classes
#waves_to_include <- c("r1agey", "r2agey", "r4agey", "r5agey", "r6agey")
#share_w1[paste0(waves_to_include, "_class")] <- lapply(share_final[waves_to_include],
#                                                          function(x) cut(x, breaks = c(age_breaks, Inf),
#                                                                          labels = FALSE, right = FALSE))

# Specify new labels for 10-year age classes
#age_class_labels <- c("50-59", "60-69", "70-79", "80-89", "90-99", "100+")

# Adjust the breaks to have the same length as labels
#age_breaks <- c(50, 60, 70, 80, 90, 100, Inf)

# Loop through each wave and create 5-year age classes with labels
#share_w1[paste0(waves_to_include, "_classlab")] <- lapply(share_final[waves_to_include],
#                                                             function(x) cut(x, breaks = age_breaks,
#                                                                             labels = age_class_labels,
#                                                                            right = FALSE))

# Loop through each wave and create 5-year age classes
#waves_to_include <- c("r1agey", "r2agey", "r4agey", "r5agey", "r6agey")
#share_w6[paste0(waves_to_include, "_class")] <- lapply(share_final[waves_to_include],
#                                                          function(x) cut(x, breaks = c(age_breaks, Inf),
#                                                                          labels = FALSE, right = FALSE))

# Loop through each wave and create 5-year age classes with labels
#share_w6[paste0(waves_to_include, "_classlab")] <- lapply(share_final[waves_to_include],
#                                                           function(x) cut(x, breaks = age_breaks,
#                                                                            labels = age_class_labels,
#                                                                             right = FALSE))


share_w1 <- share_w1 %>%
  select(mergeid, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke,
         r1agey, r1higov, r1hiothp, r1hident, r1hidrug, r1hiltc, raeducl, r1slfemp,
         r1jlasty, r1jnace, r1jisco, r1hownrnt, r1iearn, c2003cpindex, c2012cpindex, 
         r1bmi, r1vgactx, r1mdactx, r1smokev, r1smoken, r1drinkx, r1gripsum, 
         r1lbrf_s, h1rcany, r1adlfive, r1shopa, r1mealsa, r1medsa, r1moneya, r1ipena, h1itot, ragender, hh1hhres, r1wtsamp) %>%
  filter(r1agey >= 50)

share_w6 <- share_w6 %>%
  select(mergeid,r6cancre, r6diabe, r6hearte, r6hibpe, r6lunge, r6stroke,
         r6agey, raeducl, r6slfemp, r6jlasty,  r6jnace, r6jisco, 
         r6hownrnt, r6itearn, c2003cpindex, c2012cpindex,
         r6bmi, r6vgactx, r6mdactx, r6smokev, r6smoken, r6drinkb, r6drinkn7, 
         r6gripsum, r6lbrf_s, h6rcany, r6rcany,  r6adlfive, r6shopa, r6mealsa,
         r6medsa, r6moneya, r6itpena, r6itsemp, h6ittot, ragender, hh6hhres, r6wtsamp) %>%
  filter(r6agey >= 50)



# Convert income for 2002 to constant prices
share_w1$r1iearn_constant <- (share_w1$r1iearn / share_w1$c2003cpindex) * 100
share_w1$r1ipena_constant <- (share_w1$r1ipena / share_w1$c2003cpindex) * 100
#share_w1$r1isemp_constant <- (share_w1$r1isemp / share_w1$c2003cpindex) * 100
share_w1$r1earnings_constant <- share_w1$r1iearn_constant + share_w1$r1ipena_constant
share_w1$h1itot_constant <- (share_w1$h1itot / share_w1$c2003cpindex) * 100
# Convert income for 2012 to constant prices
share_w6$r6itearn_constant <- (share_w6$r6itearn / share_w6$c2003cpindex) * 100
share_w6$r6itpena_constant <- (share_w6$r6itpena / share_w6$c2003cpindex) * 100
#share_w6$r6itsemp_constant <- (share_w6$r6itsemp / share_w6$c2003cpindex) * 100
share_w6$r6earnings_constant <- share_w6$r6itearn_constant + share_w6$r6itpena_constant
share_w6$h6ittot_constant <- (share_w6$h6ittot / share_w6$c2003cpindex) * 100
#Thank you for using fastDummies!
#To acknowledge our work, please cite the package:
#Kaplan, J. & Schlegel, B. (2023). fastDummies: Fast Creation of Dummy (Binary)
#Columns and Rows from Categorical Variables. Version 1.7.1. URL: 
#https://github.com/jacobkap/fastDummies, https://jacobkap.github.io/fastDummies/.

# Create dummy variables for education
share_w1 <- dummy_cols(share_w1, select_columns = 'raeducl')

share_w1 <- rename(share_w1,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                   tertiary_educ = raeducl_3)
# Create dummy variables for education
share_w6 <- dummy_cols(share_w6, select_columns = 'raeducl')

share_w6 <- rename(share_w6,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                   tertiary_educ = raeducl_3)
# 
# 
# # Create dummy variables for home ownership in wave 1
# share_cross <- dummy_cols(share_cross, select_columns = 'r1hownrnt')
# # Create dummy variables for home ownership in wave 6
# share_cross <- dummy_cols(share_cross, select_columns = 'r6hownrnt')
# Define the income brackets based on the percentiles
share_w1$r1_low_income <- ifelse(share_w1$r1iearn_constant <= 18000, 1, 0)
share_w1$r1_middle_income <- ifelse(share_w1$r1iearn_constant > 18000 & 
                                      share_w1$r1iearn_constant <= 35438.81, 1, 0)
share_w1$r1_high_income <- ifelse(share_w1$r1iearn_constant > 35438.81, 1, 0)

# Define the income brackets based on the percentiles
share_w6$r6_low_income <- ifelse(share_w6$r6itearn_constant <= 18000, 1, 0)
share_w6$r6_middle_income <- ifelse(share_w6$r6itearn_constant > 18000 
                                    & share_w6$r6itearn_constant <= 35438.81, 1, 0)
share_w6$r6_high_income <- ifelse(share_w6$r6itearn_constant > 35438.81, 1, 0)

# Define the BMI categories based on the WHO classification for wave 1
share_w1$r1_underweight <- ifelse(share_w1$r1bmi < 18.5, 1, 0)
share_w1$r1_normal_weight <- ifelse(share_w1$r1bmi >= 18.5 & share_w1$r1bmi < 25, 1, 0)
share_w1$r1_overweight <- ifelse(share_w1$r1bmi >= 25 & share_w1$r1bmi < 30, 1, 0)
share_w1$r1_obese <- ifelse(share_w1$r1bmi >= 30, 1, 0)

# Define the BMI categories based on the WHO classification for wave 6 
share_w6$r6_underweight <- ifelse(share_w6$r6bmi < 18.5, 1, 0)
share_w6$r6_normal_weight <- ifelse(share_w6$r6bmi >= 18.5 & share_w6$r6bmi < 25, 1, 0)
share_w6$r6_overweight <- ifelse(share_w6$r6bmi >= 25 & share_w6$r6bmi < 30, 1, 0)
share_w6$r6_obese <- ifelse(share_w6$r6bmi >= 30, 1, 0)


# Create dummies for low/high skill, blue/white collar workers for wave 1

share_w1$r1_high_white_collar <- ifelse(share_w1$r1jisco %in% c(1,2,3), 1, 0)
share_w1$r1_low_white_collar <- ifelse(share_w1$r1jisco %in% c(4,5), 1, 0)
share_w1$r1_high_blue_collar <- ifelse(share_w1$r1jisco %in% c(6,7), 1, 0)
share_w1$r1_low_blue_collar <- ifelse(share_w1$r1jisco %in% c(8,9), 1, 0)

# Create a new variable that captures if an individual is employed
unique(share_w1$r1lbrf_s)
share_w1$r1_employed <- ifelse(share_w1$r1lbrf_s == 1, 1, 0)
share_w1 %>% tab(r1_employed)
share_w1$r1_retired <- ifelse(share_w1$r1lbrf_s == 5, 1, 0)
share_w1 %>% tab(r1_retired)
share_w1$r1_unemployed <- ifelse(share_w1$r1lbrf_s == 3, 1, 0)
share_w1 %>% tab(r1_unemployed)


# Create new dummy variables
share_w1$emp_r1_high_white_collar <- ifelse(share_w1$r1_employed == 1 & share_w1$r1_high_white_collar == 1, 1, 0)
share_w1$emp_r1_low_white_collar <- ifelse(share_w1$r1_employed == 1 & share_w1$r1_low_white_collar == 1, 1, 0)
share_w1$emp_r1_high_blue_collar <- ifelse(share_w1$r1_employed == 1 & share_w1$r1_high_blue_collar == 1, 1, 0)
share_w1$emp_r1_low_blue_collar <- ifelse(share_w1$r1_employed == 1 & share_w1$r1_low_blue_collar == 1, 1, 0)


share_w1 %>% tab(emp_r1_high_white_collar)
share_w1 %>% tab(r1_high_white_collar)
share_w1 %>% tab(emp_r1_low_white_collar)
share_w1 %>% tab(r1_low_white_collar)
share_w1 %>% tab(emp_r1_high_blue_collar)
share_w1 %>% tab(r1_high_blue_collar)
share_w1 %>% tab(emp_r1_low_blue_collar)
share_w1 %>% tab(r1_low_blue_collar)

# Create dummies for low/high skill, blue/white collar workers for wave 6

share_w6$r6_high_white_collar <- ifelse(share_w6$r6jisco %in% c(1,2,3), 1, 0)
share_w6$r6_low_white_collar <- ifelse(share_w6$r6jisco %in% c(4,5), 1, 0)
share_w6$r6_high_blue_collar <- ifelse(share_w6$r6jisco %in% c(6,7), 1, 0)
share_w6$r6_low_blue_collar <- ifelse(share_w6$r6jisco %in% c(8,9), 1, 0)

# Create a new variable that captures if an individual is employed
share_w6$r6_employed <- ifelse(share_w6$r6lbrf_s == 1, 1, 0)
share_w6$r6_retired <- ifelse(share_w6$r6lbrf_s == 5, 1, 0)
share_w6 %>% tab(r6_retired)
share_w6$r6_unemployed <- ifelse(share_w6$r6lbrf_s == 3, 1, 0)
share_w6 %>% tab(r6_unemployed)


# Create new dummy variables
share_w6$emp_r6_high_white_collar <- ifelse(share_w6$r6_employed == 1 & share_w6$r6_high_white_collar == 1, 1, 0)
share_w6$emp_r6_low_white_collar <- ifelse(share_w6$r6_employed == 1 & share_w6$r6_low_white_collar == 1, 1, 0)
share_w6$emp_r6_high_blue_collar <- ifelse(share_w6$r6_employed == 1 & share_w6$r6_high_blue_collar == 1, 1, 0)
share_w6$emp_r6_low_blue_collar <- ifelse(share_w6$r6_employed == 1 & share_w6$r6_low_blue_collar == 1, 1, 0)


share_w6 %>% tab(emp_r6_high_white_collar)
share_w6 %>% tab(r6_high_white_collar)
share_w6 %>% tab(emp_r6_low_white_collar)
share_w6 %>% tab(r6_low_white_collar)
share_w6 %>% tab(emp_r6_high_blue_collar)
share_w6 %>% tab(r6_high_blue_collar)
share_w6 %>% tab(emp_r6_low_blue_collar)
share_w6 %>% tab(r6_low_blue_collar)

#drink behaviors wave 1
share_w1$r1_drink_NO <- ifelse(share_w1$r1drinkx == 0, 1, 0) #does not drink
share_w1$r1_drink_OTS <- ifelse(share_w1$r1drinkx == 2, 1, 0) #drink one to several times a week
share_w1$r1_drink_MDW <- ifelse(share_w1$r1drinkx == 3, 1, 0) #drink most days of the week
share_w1$r1_drink_EDW <- ifelse(share_w1$r1drinkx == 4, 1, 0) #drink every day of the week
share_w1$r1_drink_EV <- ifelse(share_w1$r1drinkx >= 1, 1, 0) #ever drinks

#drink behaviors wave 6
#does not drink
share_w6$r6_drink_NO <- ifelse(share_w6$r6drinkn7 == 0, 1, 0) 

#drink one to several times a week
share_w6$r6_drink_OTS <- ifelse(share_w6$r6drinkn7 >= 1 & share_w6$r6drinkn7 <= 6, 1, 0) 

#drink most days of the week
share_w6$r6_drink_MDW <- ifelse(share_w6$r6drinkn7 >= 4 & share_w6$r6drinkn7 <= 6, 1, 0) 

#drink every day of the week
share_w6$r6_drink_EDW <- ifelse(share_w6$r6drinkn7 >= 7, 1, 0) 

share_w6$r6_drink_EV <- ifelse(share_w6$r6drinkn7 >= 1, 1, 0) #ever drinks
#---------------------
#     ADLs & IADLs
#---------------------
# Create dummy variables for each category of ADLs for wave 1
share_w1$r1adlfive_0 <- ifelse(share_w1$r1adlfive == 0, 1, 0) # No impedings
share_w1$r1adlfive_1_2 <- ifelse(share_w1$r1adlfive == 1 | share_w1$r1adlfive == 2, 1, 0) # 1 or 2 impedings
share_w1$r1adlfive_3_plus <- ifelse(share_w1$r1adlfive >= 3, 1, 0) # 3+ impedings

# Create dummy variables for each category of ADLs for wave 6
share_w6$r6adlfive_0 <- ifelse(share_w6$r6adlfive == 0, 1, 0) # No impedings
share_w6$r6adlfive_1_2 <- ifelse(share_w6$r6adlfive == 1 | share_w6$r6adlfive == 2, 1, 0) # 1 or 2 impedings
share_w6$r6adlfive_3_plus <- ifelse(share_w6$r6adlfive >= 3, 1, 0) # 3+ impedings

#summary of IADLs for wave 1
share_w1$r1iadl_0 <- ifelse(share_w1$r1shopa == 0 & share_w1$r1mealsa == 0
                            & share_w1$r1medsa == 0 & share_w1$r1moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_w1$r1_iadl_sum <- share_w1$r1shopa + share_w1$r1mealsa + share_w1$r1medsa + share_w1$r1moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_w1$r1_iadl_1_2 <- ifelse(share_w1$r1_iadl_sum == 1 | share_w1$r1_iadl_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_w1$r1_iadl_3_plus <- ifelse(share_w1$r1_iadl_sum >= 3, 1, 0)

#summary of IADLs for wave 6
share_w6$r6iadl_0 <- ifelse(share_w6$r6shopa == 0 & share_w6$r6mealsa == 0
                            & share_w6$r6medsa == 0 & share_w6$r6moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_w6$r6_iadl_sum <- share_w6$r6shopa + share_w6$r6mealsa + share_w6$r6medsa + share_w6$r6moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_w6$r6_iadl_1_2 <- ifelse(share_w6$r6_iadl_sum >= 1 & share_w6$r6_iadl_sum <= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_w6$r6_iadl_3_plus <- ifelse(share_w6$r6_iadl_sum >= 3, 1, 0)


#Disease summary
share_w1$r1_healthy <- ifelse(share_w1$r1cancre == 0 & share_w1$r1hibpe == 0
                              & share_w1$r1diabe == 0 & share_w1$r1hearte ==0
                              & share_w1$r1lunge ==0 & share_w1$r1stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_w1$r1_dis_sum <- share_w1$r1cancre + share_w1$r1hearte + share_w1$r1hibpe + share_w1$r1diabe +
  share_w1$r1lunge + share_w1$r1stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_w1$r1_morbi_2 <- ifelse(share_w1$r1_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_w1$r1_morbi_3_plus <- ifelse(share_w1$r1_dis_sum >= 3, 1, 0)

#Disease summary
share_w6$r6_healthy <- ifelse(share_w6$r6cancre == 0 & share_w6$r6hibpe == 0
                              & share_w6$r6diabe == 0 & share_w6$r6hearte ==0
                              & share_w6$r6lunge ==0 & share_w6$r6stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
share_w6$r6_dis_sum <- share_w6$r6cancre + share_w6$r6hearte + share_w6$r6hibpe + share_w6$r6diabe +
  share_w6$r6lunge + share_w6$r6stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
share_w6$r6_morbi_2 <- ifelse(share_w6$r6_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
share_w6$r6_morbi_3_plus <- ifelse(share_w6$r6_dis_sum >= 3, 1, 0)

#----------------------------------------------------
# level of functioning in social and daily activities
#----------------------------------------------------
share_w1$r1_independent <- ifelse(share_w1$r1adlfive == 0 | share_w1$r1_iadl_sum == 0, 1, 0)
share_w6$r6_independent <- ifelse(share_w6$r6adlfive == 0 | share_w6$r6_iadl_sum == 0, 1, 0)

share_w1$r1_semifunctional <- ifelse(share_w1$r1adlfive == 1 | share_w1$r1_iadl_sum == 1 | share_w1$r1adlfive == 2 | share_w1$r1_iadl_sum == 2, 1, 0)
share_w6$r6_semifunctional <- ifelse(share_w6$r6adlfive == 1 | share_w6$r6_iadl_sum == 1 | share_w6$r6adlfive == 2 | share_w6$r6_iadl_sum == 2, 1, 0)

share_w1$r1_notfunctional <- ifelse(share_w1$r1adlfive >= 3 | share_w1$r1_iadl_sum >= 3, 1, 0)
share_w6$r6_notfunctional <- ifelse(share_w6$r6adlfive >= 3 | share_w6$r6_iadl_sum >= 3, 1, 0)
#-----------------------------------
# risky behaviors
#-----------------------------------

# For wave 1
share_w1$r1_risky <- ifelse(share_w1$r1smokev >= 1 | share_w1$r1_drink_EV >= 1, 1, 0)

# For wave 6
share_w6$r6_risky <- ifelse(share_w6$r6smokev >= 1 | share_w6$r6_drink_EV >= 1, 1, 0)

# For wave 1
share_w1$r1_risky <- ifelse(is.na(share_w1$r1_risky), 0, share_w1$r1_risky)

# For wave 6
share_w6$r6_risky <- ifelse(is.na(share_w6$r6_risky), 0, share_w6$r6_risky)

#-----------------------------------
# physical activity behaviors
#-----------------------------------
share_w1$r1no_phys_act <- ifelse(share_w1$r1vgactx == 5 | share_w1$r1mdactx == 5, 1, 0)
share_w1 %>% tab(r1no_phys_act)
share_w1$r1low_phys_act <- ifelse(share_w1$r1vgactx == 4 | share_w1$r1mdactx == 4, 1, 0)
share_w1 %>% tab(r1low_phys_act)
share_w1$r1good_phys_act <- ifelse(share_w1$r1vgactx == 3 | share_w1$r1mdactx == 3 |share_w1$r1vgactx == 2 | share_w1$r1mdactx == 2 , 1, 0)
share_w1 %>% tab(r1good_phys_act)
#share_w1$r1vgood_phys_act <- ifelse(share_w1$r1vgactx == 2 | share_w1$r1mdactx == 2, 1, 0)
#share_w1 %>% tab(r1vgood_phys_act)

# For wave 6
share_w6$r6no_phys_act <- ifelse(share_w6$r6vgactx == 5 | share_w6$r6mdactx == 5, 1, 0)
share_w6$r6low_phys_act <- ifelse(share_w6$r6vgactx == 4 | share_w6$r6mdactx == 4, 1, 0)
share_w6$r6good_phys_act <- ifelse(share_w6$r6vgactx == 3 | share_w6$r6mdactx == 3 | share_w6$r6vgactx == 2 | share_w6$r6mdactx == 2 , 1, 0)
#share_w6$r6vgood_phys_act <- ifelse(share_w6$r6vgactx == 2 | share_w6$r6mdactx == 2, 1, 0)
share_w6 %>% tab(r6no_phys_act)
share_w6 %>% tab(r6low_phys_act)
share_w6 %>% tab(r6good_phys_act)
#share_w6 %>% tab(r6vgood_phys_act)

share_w1$man <- ifelse(share_w1$ragender == 1, 1, 0)
share_w1$woman <- ifelse(share_w1$ragender == 2, 1, 0)

share_w6$man <- ifelse(share_w6$ragender == 1, 1, 0)
share_w6$woman <- ifelse(share_w6$ragender == 2, 1, 0)

#-----------------------------------
#              family 
#-----------------------------------

share_w1$r1_single <- ifelse(share_w1$hh1hhres == 1, 1, 0)
share_w1$r1_notsingle <- ifelse(share_w1$hh1hhres >= 2, 1, 0)
share_w1 %>% tab(r1_single)
share_w1 %>% tab(r1_notsingle)

share_w6$r6_single <- ifelse(share_w6$hh6hhres == 1, 1, 0)
share_w6$r6_notsingle <- ifelse(share_w6$hh6hhres >= 2, 1, 0)
share_w6 %>% tab(r6_single)
share_w6 %>% tab(r6_notsingle)
#-----------------------------------

#write.csv(share_w1, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/share_w1.csv")
#write.csv(share_w6, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/share_w6.csv")
write_dta(share_w1, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/share_w1.dta")
write_dta(share_w6, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/share_w6.dta")
#write_dta(share_w1, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA//SHARE/output/share_w1.dta")
#write_dta(share_w6, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA//SHARE/output/share_w6.dta")
# share_cross %>% tab(r1diabe)
# share_cross %>% tab(r1hearte)
# share_cross %>% tab(r1hibpe)
# share_cross %>% tab(r1lunge)
# share_cross %>% tab(r1stroke)
# share_cross %>% tab(r1bmi)
# 
# share_cross %>% tab(r6diabe)
# share_cross %>% tab(r6hearte)
# share_cross %>% tab(r6hibpe)
# share_cross %>% tab(r6lunge)
# share_cross %>% tab(r6bmi)

#--dataset divided by gender
male_sharew1 <- share_w1 %>%
  filter(man==1)
count(male_sharew1)

female_sharew1 <- share_w1 %>%
  filter(woman==1)
count(female_sharew1)

male_sharew6 <- share_w6 %>%
  filter(man==1)
count(male_sharew6)

female_sharew6 <- share_w6 %>%
  filter(woman==1)
count(female_sharew6)

write_dta(male_sharew1, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/male_sharew1.dta")
write_dta(female_sharew1, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/female_sharew1.dta")

write_dta(male_sharew6, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/male_sharew6.dta")
write_dta(female_sharew6, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/SHARE/output/female_sharew6.dta")

