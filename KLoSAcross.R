remove(list=ls())
gc()
# Load required libraries
library(dplyr)
library(haven)
library(quantmod)
library(ggplot2)
library(sharp)
library(tabulator)
library(pastecs)
library(openxlsx)
library(fastDummies)
#WINHOME
#setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA")
#load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/MEMklosa_definitive.RData")
#SERVER
setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA")
#load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/MEMklosa_definitive.RData")
#MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/klosa")
#load("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/klosa/MEMklosa_definitive.RData")
#-------FORMATTING CPI DATA-------
# Set the currency pair and time period
currency_pair <- "KRW/USD"
start_date <- "2018-01-01"
end_date <- "2018-12-31"

# Get the exchange rate data
# Note: Yahoo Finance may not support KRW/USD directly; you may need to find a supported symbol
getSymbols("KRWUSD=X", src="yahoo", from=start_date, to=end_date, auto.assign=TRUE)

# Extract the closing prices
exchange_rates <- Cl(`KRWUSD=X`)

# Calculate the mean exchange rate for the year
mean_exchange_rate <- mean(exchange_rates, na.rm=TRUE)

# Print the result
print(mean_exchange_rate)

klosadata <- read_dta("output/H_KLoSA_e2.dta")
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
klosa_w3 <- klosadata %>%
  filter(r3iwstat == 1)
klosa_w8 <- klosadata %>%
  filter(r8iwstat == 1 | r8iwstat == 4)
klosa_w3 <- klosadata %>%
  select(pid, r3cancre, r3diabe, r3hearte, r3hibpe, r3lunge, r3stroke, r3bmi,
         r3gripsum,r3agey, r3vigactf_k,
         r3smokev, r3smoken, r3drinkx,r3drinkn_k,
         raeducl, r3slfemp, r3jlasty, r3lbrf_k, r3rcany, r3adlfiveb,
         r3shopb, r3mealsb, r3medsb, r3moneyb, r3itot, s3itot, ragender, h3hhres, ragender, r3wtresp
  ) %>%
  filter(r3agey >= 50)

klosa_w8 <- klosadata %>%
  select(pid, r8bmi, r8cancre, r8diabe, r8hearte, r8hibpe,r8lunge, r8stroke,
         r8gripsum, r8agey, r8vigactf_k,r8smokev,
         r8drinkn_k, r8drinkx, raeducl, r8slfemp,
         r8jlasty,r8lbrf_k, r8rcany, r8adlfiveb, r8shopb, r8mealsb, r8medsb, r8moneyb, r8wtresp,
         r8itot, s8itot, h8hhres, ragender ) %>%
  filter(r8agey >= 50)

# Convert income for 2002 to constant prices
klosa_w3$h3itot <- klosa_w3$r3itot + klosa_w3$s3itot
klosa_w8$h8itot <- klosa_w8$r8itot + klosa_w8$s8itot
klosa_w3$h3itot_constant <- (klosa_w3$h3itot / cpi_data$c2003cpindex) * 100
# Convert income for 2012 to constant prices
klosa_w8$h8itot_constant <- (klosa_w8$h8itot / cpi_data$c2003cpindex) * 100

klosa_w3$h3itot_constant <- klosa_w3$h3itot_constant* 0.00086 * 10000
klosa_w8$h8itot_constant <- klosa_w8$h8itot_constant * 0.00091 * 10000


# Create dummy variables for education
klosa_w3 <- dummy_cols(klosa_w3, select_columns = 'raeducl')

klosa_w3 <- rename(klosa_w3,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                 tertiary_educ = raeducl_3)
# Create dummy variables for education
klosa_w8 <- dummy_cols(klosa_w8, select_columns = 'raeducl')

klosa_w8 <- rename(klosa_w8,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                  tertiary_educ = raeducl_3)
# Create dummy variables for home ownership in wave 1
#klosa_small <- dummy_cols(klosa_small, select_columns = 'r1hownrnt')
# Create dummy variables for home ownership in wave 6
#klosa_small <- dummy_cols(klosa_small, select_columns = 'r6hownrnt')
# Define the income brackets based on the percentiles
# stat.desc(klosa_w3$r3iearn_constant)
# klosa_w3$r3_low_income <- ifelse(klosa_w3$r3iearn_constant <= 18000, 1, 0)
# klosa_w3$r3_middle_income <- ifelse(klosa_w3$r3iearn_constant > 18000 & 
#                                     klosa_w3$r3iearn_constant <= 35438.81, 1, 0)
# klosa_w3$r3_high_income <- ifelse(klosa_w3$r3iearn_constant > 35438.81, 1, 0)
# # Define the income brackets based on the percentiles
# klosa_w8$r8_low_income <- ifelse(klosa_w8$r8iearn_constant <= 18000, 1, 0)
# klosa_w8$r8_middle_income <- ifelse(klosa_w8$r8iearn_constant > 18000
#                                     & klosa_w8$r8iearn_constant <= 35438.81, 1, 0)
# klosa_w8$r8_high_income <- ifelse(klosa_w8$r8iearn_constant > 35438.81, 1, 0)

# Define the BMI categories based on the WHO classification for wave 1
klosa_w3$r3_underweight <- ifelse(klosa_w3$r3bmi < 18.5, 1, 0)
klosa_w3$r3_normal_weight <- ifelse(klosa_w3$r3bmi >= 18.5 & klosa_w3$r3bmi < 25, 1, 0)
klosa_w3$r3_overweight <- ifelse(klosa_w3$r3bmi >= 25 & klosa_w3$r3bmi < 30, 1, 0)
klosa_w3$r3_obese <- ifelse(klosa_w3$r3bmi >= 30, 1, 0)

# Define the BMI categories based on the WHO classification for wave 6 
klosa_w8$r8_underweight <- ifelse(klosa_w8$r8bmi < 18.5, 1, 0)
klosa_w8$r8_normal_weight <- ifelse(klosa_w8$r8bmi >= 18.5 & klosa_w8$r8bmi < 25, 1, 0)
klosa_w8$r8_overweight <- ifelse(klosa_w8$r8bmi >= 25 & klosa_w8$r8bmi < 30, 1, 0)
klosa_w8$r8_obese <- ifelse(klosa_w8$r8bmi >= 30, 1, 0)


# Create dummies for low/high skill, blue/white collar workers for wave 7
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
# klosa_w3$r3_high_white_collar <- ifelse(klosa_w3$r3jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# klosa_w3$r3_low_white_collar <- ifelse(klosa_w3$r3jcoccb %in% c(17, 18), 1, 0)
# klosa_w3$r3_high_blue_collar <- ifelse(klosa_w3$r3jcoccb %in% c(20, 21, 22, 23), 1, 0)
# klosa_w3$r3_low_blue_collar <- ifelse(klosa_w3$r3jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)
# 
 unique(klosa_w3$r3lbrf_k)

klosa_w3$r3_employed <- ifelse(klosa_w3$r3lbrf_k == 1 & 2 & 3, 1, 0)
klosa_w3 %>% tab(r3_employed)
klosa_w3$r3_retired <- ifelse(klosa_w3$r3lbrf_k == 7, 1, 0)
klosa_w3 %>% tab(r3_retired)
klosa_w3$r3_unemployed <- ifelse(klosa_w3$r3lbrf_k == 5, 1, 0)
klosa_w3 %>% tab(r3_unemployed)



# Create dummies for low/high skill, blue/white collar workers for wave 11
unique(klosa_w8$r8jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
# #better way
# klosa_w8$r8_high_white_collar <- ifelse(klosa_w8$r8jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# klosa_w8$r8_low_white_collar <- ifelse(klosa_w8$r8jcoccb %in% c(17, 18), 1, 0)
# klosa_w8$r8_high_blue_collar <- ifelse(klosa_w8$r8jcoccb %in% c(20, 21, 22, 23), 1, 0)
# klosa_w8$r8_low_blue_collar <- ifelse(klosa_w8$r8jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(klosa_w8$r8lbrf_k)

klosa_w8$r8_employed <- ifelse(klosa_w8$r8lbrf_k == 1 , 1, 0)
klosa_w8 %>% tab(r8_employed)
klosa_w8$r8_retired <- ifelse(klosa_w8$r8lbrf_k == 7, 1, 0)
klosa_w8 %>% tab(r8_retired)
klosa_w8$r8_unemployed <- ifelse(klosa_w8$r8lbrf_k == 5, 1, 0)
klosa_w8 %>% tab(r8_unemployed)


# # Employed workers low/high skill white/blue collar
# klosa_w3$emp_r3_high_white_collar <- ifelse(klosa_w3$r3_employed == 1 &
#                                             klosa_w3$r3_high_white_collar == 1, 1, 0)
# klosa_w3$emp_r3_low_white_collar <- ifelse(klosa_w3$r3_employed == 1 &
#                                            klosa_w3$r3_low_white_collar == 1, 1, 0)
# klosa_w3$emp_r3_high_blue_collar <- ifelse(klosa_w3$r3_employed == 1 &
#                                            klosa_w3$r3_high_blue_collar == 1, 1, 0)
# klosa_w3$emp_r3_low_blue_collar <- ifelse(klosa_w3$r3_employed == 1 &
#                                           klosa_w3$r3_low_blue_collar == 1, 1, 0)

# 
# klosa_w3 %>% tab(emp_r3_high_white_collar)
# klosa_w3 %>% tab(r3_high_white_collar)
# klosa_w3 %>% tab(emp_r3_low_white_collar)
# klosa_w3 %>% tab(r3_low_white_collar)
# klosa_w3 %>% tab(emp_r3_high_blue_collar)
# klosa_w3 %>% tab(r3_high_blue_collar)
# klosa_w3 %>% tab(emp_r3_low_blue_collar)
# klosa_w3 %>% tab(r3_low_blue_collar)
# 
# 
# # Employed workers low/high skill white/blue collar
# klosa_w8$emp_r8_high_white_collar <- ifelse(klosa_w8$r8_employed == 1 &
#                                               klosa_w8$r8_high_white_collar == 1, 1, 0)
# klosa_w8$emp_r8_low_white_collar <- ifelse(klosa_w8$r8_employed == 1 &
#                                              klosa_w8$r8_low_white_collar == 1, 1, 0)
# klosa_w8$emp_r8_high_blue_collar <- ifelse(klosa_w8$r8_employed == 1 &
#                                              klosa_w8$r8_high_blue_collar == 1, 1, 0)
# klosa_w8$emp_r8_low_blue_collar <- ifelse(klosa_w8$r8_employed == 1 &
#                                             klosa_w8$r8_low_blue_collar == 1, 1, 0)
# 
# 
# klosa_w8 %>% tab(emp_r8_high_white_collar)
# klosa_w8 %>% tab(r8_high_white_collar)
# klosa_w8 %>% tab(emp_r8_low_white_collar)
# klosa_w8 %>% tab(r8_low_white_collar)
# klosa_w8 %>% tab(emp_r8_high_blue_collar)
# klosa_w8 %>% tab(r8_high_blue_collar)
# klosa_w8 %>% tab(emp_r8_low_blue_collar)
# klosa_w8 %>% tab(r8_low_blue_collar) 

unique(klosa_w3$r3drinkx)

#Drinking behaviours
#drink behaviors wave 7
klosa_w3$r3_drink_NO <- ifelse(klosa_w3$r3drinkx == 0, 1, 0) #does not drink
klosa_w3$r3_drink_OTS <- ifelse(klosa_w3$r3drinkx == 1 & 2 & 3, 1, 0) #drink one to several times a week
klosa_w3$r3_drink_MDW <- ifelse(klosa_w3$r3drinkx == 4 & 5 & 6, 1, 0) #drink most days of the week
klosa_w3$r3_drink_EDW <- ifelse(klosa_w3$r3drinkx == 7, 1, 0) #drink every day of the week


#drink behaviors wave  11
klosa_w8$r8_drink_NO <- ifelse(klosa_w8$r8drinkx == 0, 1, 0) #does not drink
klosa_w8$r8_drink_OTS <- ifelse(klosa_w8$r8drinkx == 1 & 2 & 3, 1, 0) #drink one to several times a week
klosa_w8$r8_drink_MDW <- ifelse(klosa_w8$r8drinkx == 4 & 5 & 6, 1, 0) #drink most days of the week
klosa_w8$r8_drink_EDW <- ifelse(klosa_w8$r8drinkx == 7, 1, 0) #drink every day of the week

# Create dummy variables for each category of ADLs for wave 1
klosa_w3$r3adlfive_0 <- ifelse(klosa_w3$r3adlfiveb == 0, 1, 0) # No impedings
klosa_w3$r3adlfive_1_2 <- ifelse(klosa_w3$r3adlfiveb == 1 | klosa_w3$r3adlfiveb == 2, 1, 0) # 1 or 2 impedings
klosa_w3$r3adlfive_3_plus <- ifelse(klosa_w3$r3adlfiveb >= 3, 1, 0) # 3+ impedings

# Create dummy variables for each category of ADLs for wave 6
klosa_w8$r8adlfive_0 <- ifelse(klosa_w8$r8adlfiveb == 0, 1, 0) # No impedings
klosa_w8$r8adlfive_1_2 <- ifelse(klosa_w8$r8adlfiveb == 1 | klosa_w8$r8adlfiveb == 2, 1, 0) # 1 or 2 impedings
klosa_w8$r8adlfive_3_plus <- ifelse(klosa_w8$r8adlfiveb >= 3, 1, 0) # 3+ impedings

#summary of IADLs for wave 1
klosa_w3$r3_iadl_0 <- ifelse(klosa_w3$r3shopb == 0 & klosa_w3$r3mealsb == 0
                           & klosa_w3$r3medsb == 0 & klosa_w3$r3moneyb ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
klosa_w3$r3_iadl_sum <- klosa_w3$r3shopb + klosa_w3$r3mealsb + klosa_w3$r3medsb + klosa_w3$r3moneyb

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
klosa_w3$r3_iadl_1_2 <- ifelse(klosa_w3$r3_iadl_sum == 1 | klosa_w3$r3_iadl_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
klosa_w3$r3_iadl_3_plus <- ifelse(klosa_w3$r3_iadl_sum >= 3, 1, 0)

#summary of IADLs for wave 6
klosa_w8$r8_iadl_0 <- ifelse(klosa_w8$r8shopb == 0 & klosa_w8$r8mealsb == 0
                             & klosa_w8$r8medsb == 0 & klosa_w8$r8moneyb ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
klosa_w8$r8_iadl_sum <- klosa_w8$r8shopb + klosa_w8$r8mealsb + klosa_w8$r8medsb + klosa_w8$r8moneyb

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
klosa_w8$r8_iadl_1_2 <- ifelse(klosa_w8$r8_iadl_sum >= 1 & klosa_w8$r8_iadl_sum <= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
klosa_w8$r8_iadl_3_plus <- ifelse(klosa_w8$r8_iadl_sum >= 3, 1, 0)


#Disease summary
klosa_w3$r3_healthy <- ifelse(klosa_w3$r3cancre == 0 & klosa_w3$r3hibpe == 0
                            & klosa_w3$r3diabe == 0 & klosa_w3$r3hearte ==0
                            & klosa_w3$r3lunge ==0 & klosa_w3$r3stroke ==0, 1, 0) # No impedings


#------------------------------------------------------------------------------------------
# Create a new variable that sums up the four existing dummy variables
klosa_w3$r3_dis_sum <- klosa_w3$r3cancre + klosa_w3$r3hearte + klosa_w3$r3hibpe + klosa_w3$r3diabe +
  klosa_w3$r3lunge + klosa_w3$r3stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
klosa_w3$r3_morbi_2 <- ifelse(klosa_w3$r3_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
klosa_w3$r3_morbi_3_plus <- ifelse(klosa_w3$r3_dis_sum >= 3, 1, 0)

#Disease summary
klosa_w8$r8_healthy <- ifelse(klosa_w8$r8cancre == 0 & klosa_w8$r8hibpe == 0
                              & klosa_w8$r8diabe == 0 & klosa_w8$r8hearte ==0
                              & klosa_w8$r8lunge ==0 & klosa_w8$r8stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
klosa_w8$r8_dis_sum <- klosa_w8$r8cancre + klosa_w8$r8hearte + klosa_w8$r8hibpe + klosa_w8$r8diabe +
  klosa_w8$r8lunge + klosa_w8$r8stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
klosa_w8$r8_morbi_2 <- ifelse(klosa_w8$r8_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
klosa_w8$r8_morbi_3_plus <- ifelse(klosa_w8$r8_dis_sum >= 3, 1, 0)

#----------------------------------------------------
# level of functioning in social and daily activities
#----------------------------------------------------
klosa_w3$r3_independent <- ifelse(klosa_w3$r3adlfiveb == 0 | klosa_w3$r3_iadl_sum == 0, 1, 0)
klosa_w8$r8_independent <- ifelse(klosa_w8$r8adlfiveb == 0 | klosa_w8$r8_iadl_sum == 0, 1, 0)

klosa_w3$r3_semifunctional <- ifelse(klosa_w3$r3adlfiveb == 1 | klosa_w3$r3_iadl_sum == 1 | klosa_w3$r3adlfiveb == 2 | klosa_w3$r3_iadl_sum == 2, 1, 0)
klosa_w8$r8_semifunctional <- ifelse(klosa_w8$r8adlfiveb == 1 | klosa_w8$r8_iadl_sum == 1 | klosa_w8$r8adlfiveb == 2 | klosa_w8$r8_iadl_sum == 2, 1, 0)

klosa_w3$r3_notfunctional <- ifelse(klosa_w3$r3adlfiveb >= 3 | klosa_w3$r3_iadl_sum >= 3, 1, 0)
klosa_w8$r8_notfunctional <- ifelse(klosa_w8$r8adlfiveb >= 3 | klosa_w8$r8_iadl_sum >= 3, 1, 0)

#-----------------------------------
# risky behaviors
#-----------------------------------
klosa_w3$r3_drink_EV <- ifelse(klosa_w3$r3drinkn_k >= 1, 1, 0)
klosa_w8$r8_drink_EV <- ifelse(klosa_w8$r8drinkn_k >= 1, 1, 0)
# For wave 1
klosa_w3$r3_risky <- ifelse(klosa_w3$r3smokev >= 1 | klosa_w3$r3_drink_EV >= 1, 1, 0)

# For wave 6
klosa_w8$r8_risky <- ifelse(klosa_w8$r8smokev >= 1 | klosa_w8$r8_drink_EV >= 1, 1, 0)

# For wave 1
klosa_w3$r3_risky <- ifelse(is.na(klosa_w3$r3_risky), 0, klosa_w3$r3_risky)

# For wave 6
klosa_w8$r8_risky <- ifelse(is.na(klosa_w8$r8_risky), 0, klosa_w8$r8_risky)


#-----------------------------------
# physical activity behaviors
#-----------------------------------
klosa_w3$r3no_phys_act <- ifelse(klosa_w3$r3vigactf_k == 0 , 1, 0)
klosa_w3$r3good_phys_act <-  ifelse(klosa_w3$r3vigactf_k == 1 , 1, 0)

# For wave 6
klosa_w8$r8no_phys_act <- ifelse(klosa_w8$r8vigactf_k == 0, 1, 0)
klosa_w8$r8good_phys_act <- ifelse(klosa_w8$r8vigactf_k == 1, 1, 0)
klosa_w3 %>% tab(r3no_phys_act)
klosa_w3 %>% tab(r3good_phys_act)
klosa_w8 %>% tab(r8no_phys_act)
klosa_w8 %>% tab(r8good_phys_act)

#-----------------------------------
#              family 
#-----------------------------------

klosa_w3$r3_single <- ifelse(klosa_w3$h3hhres == 1, 1, 0)
klosa_w3$r3_notsingle <- ifelse(klosa_w3$h3hhres >= 2, 1, 0)
klosa_w3 %>% tab(r3_single)
klosa_w3 %>% tab(r3_notsingle)

klosa_w8$r8_single <- ifelse(klosa_w8$h8hhres == 1, 1, 0)
klosa_w8$r8_notsingle <- ifelse(klosa_w8$h8hhres >= 2, 1, 0)
klosa_w8 %>% tab(r8_single)
klosa_w8 %>% tab(r8_notsingle)

#-----------------------------------
#              gender
#-----------------------------------
klosa_w3$man <- ifelse(klosa_w3$ragender == 1, 1, 0)
klosa_w3$woman <- ifelse(klosa_w3$ragender == 2, 1, 0)

klosa_w8$man <- ifelse(klosa_w8$ragender == 1, 1, 0)
klosa_w8$woman <- ifelse(klosa_w8$ragender == 2, 1, 0)


# write.csv(klosa_cross_section, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/klosa_cross_section.csv")
# 
# write_dta(klosa_cross_section, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/output/klosa_cross_section.dta")
# write.csv(klosa_w3, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/klosa_w3.csv")
write_dta(klosa_w3, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA/output/klosa_w3.dta")
# write.csv(klosa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/klosa_w8.csv")
write_dta(klosa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA/output/klosa_w8.dta")
# 

#write_dta(klosa_w3, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/output/klosa_w3.dta")
#write_dta(klosa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/klosa/output/klosa_w8.dta")


#--dataset divided by gender
male_klosaw3 <- klosa_w3 %>%
  filter(man==1)
count(male_klosaw3)

female_klosaw3 <- klosa_w3 %>%
  filter(woman==1)
count(female_klosaw3)

male_klosaw8 <- klosa_w8 %>%
  filter(man==1)
count(male_klosaw8)

female_klosaw8 <- klosa_w8 %>%
  filter(woman==1)
count(female_klosaw8)

write_dta(male_klosaw3, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA/output/male_klosaw3.dta")
write_dta(female_klosaw3, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA/output/female_klosaw3.dta")

write_dta(male_klosaw8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA/output/male_klosaw8.dta")
write_dta(female_klosaw8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/KLoSA/output/female_klosaw8.dta")

