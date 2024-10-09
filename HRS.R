remove(list=ls())
# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(sharp)
library(tabulator)
library(openxlsx)
library(fastDummies)
#WINHOME
setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS")
load("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/MEMHRS_definitive.RData")
#SERVER
#setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS")
#load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/MEMHRS_definitive.RData")
#-------FORMATTING CPI DATA-------
cpi_data <- read.csv("CPI_data/Cpi.csv")

cpi_data<- cpi_data %>%
  select("X2002..YR2002.", "X2003..YR2003.", "X2004..YR2004.", "X2005..YR2005.", "X2006..YR2006.", "X2007..YR2007.",
         "X2008..YR2008.", "X2009..YR2009.", "X2010..YR2010.", "X2011..YR2011.", "X2012..YR2012.")
cpi_data <- cpi_data %>%
  rename(c2002cpindex = "X2002..YR2002.",
         c2003cpindex = "X2003..YR2003.",
         c2004cpindex = "X2004..YR2004.",
         c2005cpindex = "X2005..YR2005.",
         c2006cpindex = "X2006..YR2006.",
         c2007cpindex = "X2007..YR2007.",
         c2008cpindex = "X2008..YR2008.",
         c2009cpindex = "X2009..YR2009.",
         c2010cpindex = "X2010..YR2010.",
         c2011cpindex = "X2011..YR2011.",
         c2012cpindex = "X2012..YR2012.")

cpi_data <- na.omit(cpi_data)


# Specify new age breaks with 5-year span
age_breaks <- seq(50, 100, by = 10)

# Loop through each wave and create 5-year age classes
waves_to_include <- c("r7agey_e", "r8agey_e", "r9agey_e", "r10agey_e", "r11agey_e", "r12agey_e")
hrsdata[paste0(waves_to_include, "_class")] <- lapply(hrsdata[waves_to_include], function(x) cut(x, breaks = c(age_breaks, Inf), labels = FALSE, right = FALSE))

# Specify new labels for 10-year age classes
age_class_labels <- c("50-59", "60-69", "70-79", "80-89", "90-99", "100+")

# Adjust the breaks to have the same length as labels
age_breaks <- c(50, 60, 70, 80, 90, 100, Inf)

# Loop through each wave and create 5-year age classes with labels
hrsdata[paste0(waves_to_include, "_classlab")] <- lapply(hrsdata[waves_to_include], function(x) cut(x, breaks = age_breaks, labels = age_class_labels, right = FALSE))


health_vars <- hrsdata[, c("r7cancre", "r7diabe", "r7hearte", "r7hibpe",
                             "r7lunge", "r7stroke", "r7bmi", "r12bmi",
                             "r12cancre", "r12diabe", "r12hearte", "r12hibpe",
                             "r12lunge", "r12stroke","r7gripsum", "r12gripsum")]
age_vars <- hrsdata[, c("r7agey_e", "r12agey_e")]
behaviour_vars <- hrsdata[,c( "r7vgactx", "r12vgactx", "r7mdactx", "r12mdactx",
                                "r7smokev","r12smokev", "r7smoken", "r7drinkd","r7drinkn",
                                "r12drinkn", "r12drinkd")]

educ_vars <- as.data.frame(hrsdata$raeducl)
labor_vars <- hrsdata[, c("r7slfemp", "r7jlasty", "r12slfemp",  "r12jlasty", "r7lbrf"
                            ,"r12lbrf", "r7jcoccb", "r12jcoccb" )]
#wealth_vars <- hrsdata[, c("r7hownrnt", "r12hownrnt") ]

missing_proportion_health <- colMeans(is.na(health_vars))

missing_proportion_age <- colMeans(is.na(age_vars))

missing_proportion_behaviour <- colMeans(is.na(behaviour_vars))

missing_proportion_educ <- colMeans(is.na(educ_vars))

missing_proportion_labor <- colMeans(is.na(labor_vars))

#missing_proportion_wealth <- colMeans(is.na(wealth_vars))

# Display the results
missing_proportion_health
missing_proportion_age
missing_proportion_behaviour
missing_proportion_educ
missing_proportion_labor
#missing_proportion_wealth

#
# Filter the dataset based on the conditions for r1iwstat and r6iwstat, rather than using isalive
hrs_final <- hrsdata%>%
  filter(r7iwstat == 1 & (r12iwstat == 1 | r12iwstat == 4))

age_breaks <- seq(28, 100, by = 5)
# Loop through each wave and create 5-year age classes
waves_to_include <- c("r7agey_e", "r8agey_e", "r9agey_e", "r10agey_e", "r11agey_e", "r12agey_e")
hrs_final[paste0(waves_to_include, "_class")] <- lapply(hrs_final[waves_to_include],
                                                          function(x) cut(x, breaks = c(age_breaks, Inf),
                                                                          labels = FALSE, right = FALSE))

# Specify new labels for 5-year age classes
age_class_labels <- c("28-32", "33-37", "38-42", "43-47", "48-52", "53-57",
                      "58-62", "63-67", "68-72", "73-77", "78-82", "83-87",
                      "88-92", "93-97", "98-100")

# Adjust the breaks to have the same length as labels
age_breaks <- c(28, 33, 38, 43, 48, 53, 58, 63, 68, 73, 78, 83, 88, 93, 98, Inf)

# Loop through each wave and create 5-year age classes with labels
hrs_final[paste0(waves_to_include, "_classlab")] <- lapply(hrs_final[waves_to_include],
                                                             function(x) cut(x, breaks = age_breaks,
                                                                             labels = age_class_labels,
                                                                             right = FALSE))

hrs_small <- hrs_final %>%
  select(hhidpn, r7cancre, r7diabe, r7hearte, r7hibpe, r7lunge, r7stroke, r7bmi,
         r12bmi, r12cancre, r12diabe, r12hearte, r12hibpe,r12lunge, r12stroke,
         r7gripsum, r12gripsum,r7agey_e, r12agey_e, r7vgactx, r12vgactx,
         r7mdactx, r12mdactx, r7smokev,r12smokev, r7smoken, r7drinkd,r7drinkn,
         r12drinkn, r12drinkd, raeducl, r7slfemp, r7jlasty, r12slfemp,
         r12jlasty, r7lbrf,r12lbrf, r7jcoccb, r12jcoccb, r7iearn, r12iearn, r7rcany,
         r12rcany) %>%
  filter(r7agey_e >= 50) %>%
  filter(r12agey_e >= 50)

summary(age_vars)

health_vars <- hrs_small[, c("r7cancre", "r7diabe", "r7hearte", "r7hibpe",
                           "r7lunge", "r7stroke", "r7bmi", "r12bmi",
                           "r12cancre", "r12diabe", "r12hearte", "r12hibpe",
                           "r12lunge", "r12stroke","r7gripsum", "r12gripsum")]
age_vars <- hrs_small[, c("r7agey_e", "r12agey_e")]
behaviour_vars <- hrs_small[,c( "r7vgactx", "r12vgactx", "r7mdactx", "r12mdactx",
                              "r7smokev","r12smokev", "r7smoken", "r7drinkd","r7drinkn",
                              "r12drinkn", "r12drinkd")]

educ_vars <- as.data.frame(hrs_small$raeducl)
labor_vars <- hrs_small[, c("r7slfemp", "r7jlasty", "r12slfemp",  "r12jlasty", "r7lbrf"
                          ,"r12lbrf", "r7jcoccb", "r12jcoccb" )]
# Convert income for 2002 to constant prices
hrs_small$r7iearn_constant <- (hrs_small$r7iearn / cpi_data$c2003cpindex) * 100

# Convert income for 2012 to constant prices
hrs_small$r12iearn_constant <- (hrs_small$r12iearn / cpi_data$c2003cpindex) * 100

#wealth_vars <- hrsdata[, c("r7hownrnt", "r12hownrnt") ]

missing_proportion_health <- colMeans(is.na(health_vars))

missing_proportion_age <- colMeans(is.na(age_vars))

missing_proportion_behaviour <- colMeans(is.na(behaviour_vars))

missing_proportion_educ <- colMeans(is.na(educ_vars))

missing_proportion_labor <- colMeans(is.na(labor_vars))

#missing_proportion_wealth <- colMeans(is.na(wealth_vars))

# Display the results
missing_proportion_health
missing_proportion_age
missing_proportion_behaviour
missing_proportion_educ
missing_proportion_labor


# Create dummy variables for education
hrs_small <- dummy_cols(hrs_small, select_columns = 'raeducl')

hrs_small <- rename(hrs_small,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                      tertiary_educ = raeducl_3)
# Create dummy variables for home ownership in wave 1
#hrs_small <- dummy_cols(hrs_small, select_columns = 'r1hownrnt')
# Create dummy variables for home ownership in wave 6
#hrs_small <- dummy_cols(hrs_small, select_columns = 'r6hownrnt')
# Define the income brackets based on the percentiles
hrs_small$r7_low_income <- ifelse(hrs_small$r7iearn_constant <= 18000, 1, 0)
hrs_small$r7_middle_income <- ifelse(hrs_small$r7iearn_constant > 18000 & 
                                         hrs_small$r7iearn_constant <= 35438.81, 1, 0)
hrs_small$r7_high_income <- ifelse(hrs_small$r7iearn_constant > 35438.81, 1, 0)
# Define the income brackets based on the percentiles
hrs_small$r12_low_income <- ifelse(hrs_small$r12iearn_constant <= 18000, 1, 0)
hrs_small$r12_middle_income <- ifelse(hrs_small$r12iearn_constant > 18000
                                       & hrs_small$r12iearn_constant <= 35438.81, 1, 0)
hrs_small$r12_high_income <- ifelse(hrs_small$r12iearn_constant > 35438.81, 1, 0)

# Define the BMI categories based on the WHO classification for wave 1
hrs_small$r7_underweight <- ifelse(hrs_small$r7bmi < 18.5, 1, 0)
hrs_small$r7_normal_weight <- ifelse(hrs_small$r7bmi >= 18.5 & hrs_small$r7bmi < 25, 1, 0)
hrs_small$r7_overweight <- ifelse(hrs_small$r7bmi >= 25 & hrs_small$r7bmi < 30, 1, 0)
hrs_small$r7_obese <- ifelse(hrs_small$r7bmi >= 30, 1, 0)

# Define the BMI categories based on the WHO classification for wave 6 
hrs_small$r12_underweight <- ifelse(hrs_small$r12bmi < 18.5, 1, 0)
hrs_small$r12_normal_weight <- ifelse(hrs_small$r12bmi >= 18.5 & hrs_small$r12bmi < 25, 1, 0)
hrs_small$r12_overweight <- ifelse(hrs_small$r12bmi >= 25 & hrs_small$r12bmi < 30, 1, 0)
hrs_small$r12_obese <- ifelse(hrs_small$r12bmi >= 30, 1, 0)


hrs_small$r7morbi <- rowSums(hrs_small[,c("r7cancre", "r7diabe", "r7hearte", "r7hibpe", "r7lunge", "r7stroke")])

# Convert morbi to a binary variable where 0 indicates only one disease and 1 indicates more than one disease
hrs_small$r7morbi <- ifelse(hrs_small$r7morbi > 1, 1, 0)
hrs_small %>% tab(r7morbi)

hrs_small$r12morbi <- rowSums(hrs_small[,c("r12cancre", "r12diabe", "r12hearte", "r12hibpe", "r12lunge", "r12stroke")])

# Convert morbi to a binary variable where 0 indicates only one disease and 1 indicates more than one disease
hrs_small$r12morbi <- ifelse(hrs_small$r12morbi > 1, 1, 0)
hrs_small %>% tab(r12morbi)


# Create dummies for low/high skill, blue/white collar workers for wave 7
unique(hrs_small$r7jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
hrs_small$r7_high_white_collar <- ifelse(hrs_small$r7jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
hrs_small$r7_low_white_collar <- ifelse(hrs_small$r7jcoccb %in% c(17, 18), 1, 0)
hrs_small$r7_high_blue_collar <- ifelse(hrs_small$r7jcoccb %in% c(20, 21, 22, 23), 1, 0)
hrs_small$r7_low_blue_collar <- ifelse(hrs_small$r7jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(hrs_small$r7lbrf)

hrs_small$r7_employed <- ifelse(hrs_small$r7lbrf == 1 & 2, 1, 0)
hrs_small %>% tab(r7_employed)
hrs_small$r7_retired <- ifelse(hrs_small$r7lbrf == 4 & 5, 1, 0)
hrs_small %>% tab(r7_retired)
hrs_small$r7_unemployed <- ifelse(hrs_small$r7lbrf == 3, 1, 0)
hrs_small %>% tab(r7_unemployed)



# Create dummies for low/high skill, blue/white collar workers for wave 11
unique(hrs_small$r12jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
hrs_small$r12_high_white_collar <- ifelse(hrs_small$r12jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
hrs_small$r12_low_white_collar <- ifelse(hrs_small$r12jcoccb %in% c(17, 18), 1, 0)
hrs_small$r12_high_blue_collar <- ifelse(hrs_small$r12jcoccb %in% c(20, 21, 22, 23), 1, 0)
hrs_small$r12_low_blue_collar <- ifelse(hrs_small$r12jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(hrs_small$r12lbrf)

hrs_small$r12_employed <- ifelse(hrs_small$r12lbrf == 1 & 2, 1, 0)
hrs_small %>% tab(r12_employed)
hrs_small$r12_retired <- ifelse(hrs_small$r12lbrf == 4 & 5, 1, 0)
hrs_small %>% tab(r12_retired)
hrs_small$r12_unemployed <- ifelse(hrs_small$r12lbrf == 3, 1, 0)
hrs_small %>% tab(r12_unemployed)


# Employed workers low/high skill white/blue collar
hrs_small$emp_r7_high_white_collar <- ifelse(hrs_small$r7_employed == 1 &
                                                         hrs_small$r7_high_white_collar == 1, 1, 0)
hrs_small$emp_r7_low_white_collar <- ifelse(hrs_small$r7_employed == 1 &
                                                        hrs_small$r7_low_white_collar == 1, 1, 0)
hrs_small$emp_r7_high_blue_collar <- ifelse(hrs_small$r7_employed == 1 &
                                                        hrs_small$r7_high_blue_collar == 1, 1, 0)
hrs_small$emp_r7_low_blue_collar <- ifelse(hrs_small$r7_employed == 1 &
                                                       hrs_small$r7_low_blue_collar == 1, 1, 0)


hrs_small %>% tab(emp_r7_high_white_collar)
hrs_small %>% tab(r7_high_white_collar)
hrs_small %>% tab(emp_r7_low_white_collar)
hrs_small %>% tab(r7_low_white_collar)
hrs_small %>% tab(emp_r7_high_blue_collar)
hrs_small %>% tab(r7_high_blue_collar)
hrs_small %>% tab(emp_r7_low_blue_collar)
hrs_small %>% tab(r7_low_blue_collar)


# Employed workers low/high skill white/blue collar
hrs_small$emp_r12_high_white_collar <- ifelse(hrs_small$r12_employed == 1 &
                                                         hrs_small$r12_high_white_collar == 1, 1, 0)
hrs_small$emp_r12_low_white_collar <- ifelse(hrs_small$r12_employed == 1 &
                                                        hrs_small$r12_low_white_collar == 1, 1, 0)
hrs_small$emp_r12_high_blue_collar <- ifelse(hrs_small$r12_employed == 1 &
                                                        hrs_small$r12_high_blue_collar == 1, 1, 0)
hrs_small$emp_r12_low_blue_collar <- ifelse(hrs_small$r12_employed == 1 &
                                                       hrs_small$r12_low_blue_collar == 1, 1, 0)


hrs_small %>% tab(emp_r12_high_white_collar)
hrs_small %>% tab(r12_high_white_collar)
hrs_small %>% tab(emp_r12_low_white_collar)
hrs_small %>% tab(r12_low_white_collar)
hrs_small %>% tab(emp_r12_high_blue_collar)
hrs_small %>% tab(r12_high_blue_collar)
hrs_small %>% tab(emp_r12_low_blue_collar)
hrs_small %>% tab(r12_low_blue_collar) 

unique(hrs_small$r7drinkd)

#Drinking behaviours
#drink behaviors wave 7
hrs_small$r7_drink_NO <- ifelse(hrs_small$r7drinkd == 0, 1, 0) #does not drink
hrs_small$r7_drink_OTS <- ifelse(hrs_small$r7drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
hrs_small$r7_drink_MDW <- ifelse(hrs_small$r7drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
hrs_small$r7_drink_EDW <- ifelse(hrs_small$r7drinkd == 7, 1, 0) #drink every day of the week


#drink behaviors wave  11
hrs_small$r12_drink_NO <- ifelse(hrs_small$r12drinkd == 0, 1, 0) #does not drink
hrs_small$r12_drink_OTS <- ifelse(hrs_small$r12drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
hrs_small$r12_drink_MDW <- ifelse(hrs_small$r12drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
hrs_small$r12_drink_EDW <- ifelse(hrs_small$r12drinkd == 7, 1, 0) #drink every day of the week


write.csv(hrs_small, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/hrs_small.csv")

write_dta(hrs_small, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_small.dta")


hrs_cross_section <- hrsdata %>%
  select(hhidpn, r7cancre, r7diabe, r7hearte, r7hibpe, r7lunge, r7stroke, r7bmi,
         r12bmi, r12cancre, r12diabe, r12hearte, r12hibpe,r12lunge, r12stroke,
         r7gripsum, r12gripsum,r7agey_e, r12agey_e, r7vgactx, r12vgactx,
         r7mdactx, r12mdactx, r7smokev,r12smokev, r7smoken, r7drinkd,r7drinkn,
         r12drinkn, r12drinkd, raeducl, r7slfemp, r7jlasty, r12slfemp,
         r12jlasty, r7lbrf,r12lbrf, r7jcoccb, r12jcoccb, r7iearn, r12iearn, r7rcany,
         r12rcany) %>%
  filter(r7agey_e >= 50) %>%
  filter(r12agey_e >= 50)


health_vars <- hrs_cross_section[, c("r7cancre", "r7diabe", "r7hearte", "r7hibpe",
                           "r7lunge", "r7stroke", "r7bmi", "r12bmi",
                           "r12cancre", "r12diabe", "r12hearte", "r12hibpe",
                           "r12lunge", "r12stroke","r7gripsum", "r12gripsum")]
age_vars <- hrs_cross_section[, c("r7agey_e", "r12agey_e")]
behaviour_vars <- hrs_cross_section[,c( "r7vgactx", "r12vgactx", "r7mdactx", "r12mdactx",
                              "r7smokev","r12smokev", "r7smoken", "r7drinkd","r7drinkn",
                              "r12drinkn", "r12drinkd")]

educ_vars <- as.data.frame(hrs_cross_section$raeducl)
labor_vars <- hrs_cross_section[, c("r7slfemp", "r7jlasty", "r12slfemp",  "r12jlasty", "r7lbrf"
                          ,"r12lbrf", "r7jcoccb", "r12jcoccb" )]
# Convert income for 2002 to constant prices
hrs_cross_section$r7iearn_constant <- (hrs_cross_section$r7iearn / cpi_data$c2003cpindex) * 100

# Convert income for 2012 to constant prices
hrs_cross_section$r12iearn_constant <- (hrs_cross_section$r12iearn / cpi_data$c2003cpindex) * 100

#wealth_vars <- hrsdata[, c("r7hownrnt", "r12hownrnt") ]

missing_proportion_health <- colMeans(is.na(health_vars))

missing_proportion_age <- colMeans(is.na(age_vars))

missing_proportion_behaviour <- colMeans(is.na(behaviour_vars))

missing_proportion_educ <- colMeans(is.na(educ_vars))

missing_proportion_labor <- colMeans(is.na(labor_vars))

#missing_proportion_wealth <- colMeans(is.na(wealth_vars))

# Display the results
missing_proportion_health
missing_proportion_age
missing_proportion_behaviour
missing_proportion_educ
missing_proportion_labor


# Create dummy variables for education
hrs_cross_section <- dummy_cols(hrs_cross_section, select_columns = 'raeducl')

hrs_cross_section <- rename(hrs_cross_section,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                      tertiary_educ = raeducl_3)
# Create dummy variables for home ownership in wave 1
#hrs_cross_section <- dummy_cols(hrs_cross_section, select_columns = 'r1hownrnt')
# Create dummy variables for home ownership in wave 6
#hrs_cross_section <- dummy_cols(hrs_cross_section, select_columns = 'r6hownrnt')
# Define the income brackets based on the percentiles
hrs_cross_section$r7_low_income <- ifelse(hrs_cross_section$r7iearn_constant <= 18000, 1, 0)
hrs_cross_section$r7_middle_income <- ifelse(hrs_cross_section$r7iearn_constant > 18000 & 
                                         hrs_cross_section$r7iearn_constant <= 35438.81, 1, 0)
hrs_cross_section$r7_high_income <- ifelse(hrs_cross_section$r7iearn_constant > 35438.81, 1, 0)
# Define the income brackets based on the percentiles
hrs_cross_section$r12_low_income <- ifelse(hrs_cross_section$r12iearn_constant <= 18000, 1, 0)
hrs_cross_section$r12_middle_income <- ifelse(hrs_cross_section$r12iearn_constant > 18000
                                       & hrs_cross_section$r12iearn_constant <= 35438.81, 1, 0)
hrs_cross_section$r12_high_income <- ifelse(hrs_cross_section$r12iearn_constant > 35438.81, 1, 0)

# Define the BMI categories based on the WHO classification for wave 1
hrs_cross_section$r7_underweight <- ifelse(hrs_cross_section$r7bmi < 18.5, 1, 0)
hrs_cross_section$r7_normal_weight <- ifelse(hrs_cross_section$r7bmi >= 18.5 & hrs_cross_section$r7bmi < 25, 1, 0)
hrs_cross_section$r7_overweight <- ifelse(hrs_cross_section$r7bmi >= 25 & hrs_cross_section$r7bmi < 30, 1, 0)
hrs_cross_section$r7_obese <- ifelse(hrs_cross_section$r7bmi >= 30, 1, 0)

# Define the BMI categories based on the WHO classification for wave 6 
hrs_cross_section$r12_underweight <- ifelse(hrs_cross_section$r12bmi < 18.5, 1, 0)
hrs_cross_section$r12_normal_weight <- ifelse(hrs_cross_section$r12bmi >= 18.5 & hrs_cross_section$r12bmi < 25, 1, 0)
hrs_cross_section$r12_overweight <- ifelse(hrs_cross_section$r12bmi >= 25 & hrs_cross_section$r12bmi < 30, 1, 0)
hrs_cross_section$r12_obese <- ifelse(hrs_cross_section$r12bmi >= 30, 1, 0)


hrs_cross_section$r7morbi <- rowSums(hrs_cross_section[,c("r7cancre", "r7diabe", "r7hearte", "r7hibpe", "r7lunge", "r7stroke")])

# Convert morbi to a binary variable where 0 indicates only one disease and 1 indicates more than one disease
hrs_cross_section$r7morbi <- ifelse(hrs_cross_section$r7morbi > 1, 1, 0)
hrs_cross_section %>% tab(r7morbi)

hrs_cross_section$r12morbi <- rowSums(hrs_cross_section[,c("r12cancre", "r12diabe", "r12hearte", "r12hibpe", "r12lunge", "r12stroke")])

# Convert morbi to a binary variable where 0 indicates only one disease and 1 indicates more than one disease
hrs_cross_section$r12morbi <- ifelse(hrs_cross_section$r12morbi > 1, 1, 0)
hrs_cross_section %>% tab(r12morbi)


# Create dummies for low/high skill, blue/white collar workers for wave 7
unique(hrs_cross_section$r7jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
hrs_cross_section$r7_high_white_collar <- ifelse(hrs_cross_section$r7jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
hrs_cross_section$r7_low_white_collar <- ifelse(hrs_cross_section$r7jcoccb %in% c(17, 18), 1, 0)
hrs_cross_section$r7_high_blue_collar <- ifelse(hrs_cross_section$r7jcoccb %in% c(20, 21, 22, 23), 1, 0)
hrs_cross_section$r7_low_blue_collar <- ifelse(hrs_cross_section$r7jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(hrs_cross_section$r7lbrf)

hrs_cross_section$r7_employed <- ifelse(hrs_cross_section$r7lbrf == 1 & 2, 1, 0)
hrs_cross_section %>% tab(r7_employed)
hrs_cross_section$r7_retired <- ifelse(hrs_cross_section$r7lbrf == 4 & 5, 1, 0)
hrs_cross_section %>% tab(r7_retired)
hrs_cross_section$r7_unemployed <- ifelse(hrs_cross_section$r7lbrf == 3, 1, 0)
hrs_cross_section %>% tab(r7_unemployed)



# Create dummies for low/high skill, blue/white collar workers for wave 11
unique(hrs_cross_section$r12jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
hrs_cross_section$r12_high_white_collar <- ifelse(hrs_cross_section$r12jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
hrs_cross_section$r12_low_white_collar <- ifelse(hrs_cross_section$r12jcoccb %in% c(17, 18), 1, 0)
hrs_cross_section$r12_high_blue_collar <- ifelse(hrs_cross_section$r12jcoccb %in% c(20, 21, 22, 23), 1, 0)
hrs_cross_section$r12_low_blue_collar <- ifelse(hrs_cross_section$r12jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(hrs_cross_section$r12lbrf)

hrs_cross_section$r12_employed <- ifelse(hrs_cross_section$r12lbrf == 1 & 2, 1, 0)
hrs_cross_section %>% tab(r12_employed)
hrs_cross_section$r12_retired <- ifelse(hrs_cross_section$r12lbrf == 4 & 5, 1, 0)
hrs_cross_section %>% tab(r12_retired)
hrs_cross_section$r12_unemployed <- ifelse(hrs_cross_section$r12lbrf == 3, 1, 0)
hrs_cross_section %>% tab(r12_unemployed)


# Employed workers low/high skill white/blue collar
hrs_cross_section$emp_r7_high_white_collar <- ifelse(hrs_cross_section$r7_employed == 1 &
                                                         hrs_cross_section$r7_high_white_collar == 1, 1, 0)
hrs_cross_section$emp_r7_low_white_collar <- ifelse(hrs_cross_section$r7_employed == 1 &
                                                        hrs_cross_section$r7_low_white_collar == 1, 1, 0)
hrs_cross_section$emp_r7_high_blue_collar <- ifelse(hrs_cross_section$r7_employed == 1 &
                                                        hrs_cross_section$r7_high_blue_collar == 1, 1, 0)
hrs_cross_section$emp_r7_low_blue_collar <- ifelse(hrs_cross_section$r7_employed == 1 &
                                                       hrs_cross_section$r7_low_blue_collar == 1, 1, 0)


hrs_cross_section %>% tab(emp_r7_high_white_collar)
hrs_cross_section %>% tab(r7_high_white_collar)
hrs_cross_section %>% tab(emp_r7_low_white_collar)
hrs_cross_section %>% tab(r7_low_white_collar)
hrs_cross_section %>% tab(emp_r7_high_blue_collar)
hrs_cross_section %>% tab(r7_high_blue_collar)
hrs_cross_section %>% tab(emp_r7_low_blue_collar)
hrs_cross_section %>% tab(r7_low_blue_collar)


# Employed workers low/high skill white/blue collar
hrs_cross_section$emp_r12_high_white_collar <- ifelse(hrs_cross_section$r12_employed == 1 &
                                                         hrs_cross_section$r12_high_white_collar == 1, 1, 0)
hrs_cross_section$emp_r12_low_white_collar <- ifelse(hrs_cross_section$r12_employed == 1 &
                                                        hrs_cross_section$r12_low_white_collar == 1, 1, 0)
hrs_cross_section$emp_r12_high_blue_collar <- ifelse(hrs_cross_section$r12_employed == 1 &
                                                        hrs_cross_section$r12_high_blue_collar == 1, 1, 0)
hrs_cross_section$emp_r12_low_blue_collar <- ifelse(hrs_cross_section$r12_employed == 1 &
                                                       hrs_cross_section$r12_low_blue_collar == 1, 1, 0)


hrs_cross_section %>% tab(emp_r12_high_white_collar)
hrs_cross_section %>% tab(r12_high_white_collar)
hrs_cross_section %>% tab(emp_r12_low_white_collar)
hrs_cross_section %>% tab(r12_low_white_collar)
hrs_cross_section %>% tab(emp_r12_high_blue_collar)
hrs_cross_section %>% tab(r12_high_blue_collar)
hrs_cross_section %>% tab(emp_r12_low_blue_collar)
hrs_cross_section %>% tab(r12_low_blue_collar) 

unique(hrs_cross_section$r7drinkd)

#Drinking behaviours
#drink behaviors wave 7
hrs_cross_section$r7_drink_NO <- ifelse(hrs_cross_section$r7drinkd == 0, 1, 0) #does not drink
hrs_cross_section$r7_drink_OTS <- ifelse(hrs_cross_section$r7drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
hrs_cross_section$r7_drink_MDW <- ifelse(hrs_cross_section$r7drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
hrs_cross_section$r7_drink_EDW <- ifelse(hrs_cross_section$r7drinkd == 7, 1, 0) #drink every day of the week


#drink behaviors wave  11
hrs_cross_section$r12_drink_NO <- ifelse(hrs_cross_section$r12drinkd == 0, 1, 0) #does not drink
hrs_cross_section$r12_drink_OTS <- ifelse(hrs_cross_section$r12drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
hrs_cross_section$r12_drink_MDW <- ifelse(hrs_cross_section$r12drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
hrs_cross_section$r12_drink_EDW <- ifelse(hrs_cross_section$r12drinkd == 7, 1, 0) #drink every day of the week

write.csv(hrs_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/hrs_cross_section.csv")

write_dta(hrs_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_cross_section.dta")

