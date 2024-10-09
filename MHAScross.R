remove(list=ls())
gc()
# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(quantmod)
library(sharp)
library(tabulator)
library(pastecs)
library(openxlsx)
library(fastDummies)
#WINHOME
#setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS")

#SERVER
setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS")
#load("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/MEMHRS_definitive.RData")
#MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/HRS")
#load("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/HRS/MEMHRS_definitive.RData")
#-------FORMATTING CPI DATA-------

# Set the currency pair and time period
currency_pair <- "MXN/USD"
start_date <- "2012-01-01"
end_date <- "2012-12-31"

# Get the exchange rate data
# Note: Yahoo Finance may not support MXN/USD directly; you may need to find a supported symbol
getSymbols("MXNUSD=X", src="yahoo", from=start_date, to=end_date, auto.assign=TRUE)

# Extract the closing prices
exchange_rates <- Cl(`MXNUSD=X`)

# Calculate the mean exchange rate for the year
mean_exchange_rate <- mean(exchange_rates, na.rm=TRUE)

# Print the result
print(mean_exchange_rate)

mhasdata <- read_dta("output/H_MHAS_c2.dta")
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
mhas_w2 <- mhasdata %>%
  filter(r2iwstat == 1)
mhas_w4 <- mhasdata %>%
  filter(r4iwstat == 1 | r4iwstat == 4)
mhas_w2 <- mhasdata %>%
  select(unhhidnp, r2cancre, r2diabe, r2hibpe, r2respe, r2stroke, r2bmi,
         r2agey, r2vigact,
         r2smokev, r2smoken, r2drinkd,r2drinkn,
         raeducl, r2slfemp, r2jlasty, r2lbrf_m, r2iearn, r2rcany, r2adlfive,
         r2shopa, r2mealsa, r2medsa, r2moneya, r2ipena, h2itot, ragender, h2hhres, ragender, r2wtresp
  ) %>%
  filter(r2agey >= 50)

mhas_w4 <- mhasdata %>%
  select(unhhidnp, r4bmi, r4cancre, r4diabe, r4hearte, r4hibpe,r4respe, r4stroke,
         r4agey, r4vigact,r4smokev,
         r4drinkn, r4drinkd, raeducl, r4slfemp,
         r4jlasty,r4lbrf_m, r4iearn, r4rcany, r4adlfive, r4shopa, r4mealsa, r4medsa, r4moneya, r4wtresp,
         h4itot, h4hhres, ragender ) %>%
  filter(r4agey >= 50)

# Convert income for 2002 to constant prices
mhas_w2$h2itot_constant <- (mhas_w2$h2itot / cpi_data$c2003cpindex) * 100
# Convert income for 2012 to constant prices
mhas_w4$h4itot_constant <- (mhas_w4$h4itot / cpi_data$c2003cpindex) * 100

mhas_w2$h2itot_constant <- mhas_w2$h2itot_constant* 0.0890759
mhas_w4$h4itot_constant <- mhas_w4$h4itot_constant * 0.076

mhas_w2 <- mhas_w2 %>%
  filter(h2itot_constant >= 0)

mhas_w4 <- mhas_w4 %>%
  filter(h4itot_constant >= 0)

# Create dummy variables for education
mhas_w2 <- dummy_cols(mhas_w2, select_columns = 'raeducl')

mhas_w2 <- rename(mhas_w2,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                 tertiary_educ = raeducl_3)
# Create dummy variables for education
mhas_w4 <- dummy_cols(mhas_w4, select_columns = 'raeducl')

mhas_w4 <- rename(mhas_w4,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                  tertiary_educ = raeducl_3)
# Create dummy variables for home ownership in wave 1
#mhas_small <- dummy_cols(mhas_small, select_columns = 'r1hownrnt')
# Create dummy variables for home ownership in wave 6
#mhas_small <- dummy_cols(mhas_small, select_columns = 'r6hownrnt')
# Define the income brackets based on the percentiles
# stat.desc(mhas_w2$r2iearn_constant)
# mhas_w2$r2_low_income <- ifelse(mhas_w2$r2iearn_constant <= 18000, 1, 0)
# mhas_w2$r2_middle_income <- ifelse(mhas_w2$r2iearn_constant > 18000 & 
#                                     mhas_w2$r2iearn_constant <= 35438.81, 1, 0)
# mhas_w2$r2_high_income <- ifelse(mhas_w2$r2iearn_constant > 35438.81, 1, 0)
# # Define the income brackets based on the percentiles
# mhas_w4$r4_low_income <- ifelse(mhas_w4$r4iearn_constant <= 18000, 1, 0)
# mhas_w4$r4_middle_income <- ifelse(mhas_w4$r4iearn_constant > 18000
#                                     & mhas_w4$r4iearn_constant <= 35438.81, 1, 0)
# mhas_w4$r4_high_income <- ifelse(mhas_w4$r4iearn_constant > 35438.81, 1, 0)

# Define the BMI categories based on the WHO classification for wave 1
# mhas_w2$r2_underweight <- ifelse(mhas_w2$r2bmi < 18.5, 1, 0)
# mhas_w2$r2_normal_weight <- ifelse(mhas_w2$r2bmi >= 18.5 & mhas_w2$r2bmi < 25, 1, 0)
# mhas_w2$r2_overweight <- ifelse(mhas_w2$r2bmi >= 25 & mhas_w2$r2bmi < 30, 1, 0)
# mhas_w2$r2_obese <- ifelse(mhas_w2$r2bmi >= 30, 1, 0)
# 
# # Define the BMI categories based on the WHO classification for wave 6 
# mhas_w4$r4_underweight <- ifelse(mhas_w4$r4bmi < 18.5, 1, 0)
# mhas_w4$r4_normal_weight <- ifelse(mhas_w4$r4bmi >= 18.5 & mhas_w4$r4bmi < 25, 1, 0)
# mhas_w4$r4_overweight <- ifelse(mhas_w4$r4bmi >= 25 & mhas_w4$r4bmi < 30, 1, 0)
# mhas_w4$r4_obese <- ifelse(mhas_w4$r4bmi >= 30, 1, 0)


# Create dummies for low/high skill, blue/white collar workers for wave 7
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
# mhas_w2$r2_high_white_collar <- ifelse(mhas_w2$r2jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# mhas_w2$r2_low_white_collar <- ifelse(mhas_w2$r2jcoccb %in% c(17, 18), 1, 0)
# mhas_w2$r2_high_blue_collar <- ifelse(mhas_w2$r2jcoccb %in% c(20, 21, 22, 23), 1, 0)
# mhas_w2$r2_low_blue_collar <- ifelse(mhas_w2$r2jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(mhas_w2$r2lbrf_m)

mhas_w2$r2_employed <- ifelse(mhas_w2$r2lbrf_m == 1 , 1, 0)
mhas_w2 %>% tab(r2_employed)
mhas_w2$r2_retired <- ifelse(mhas_w2$r2lbrf_m == 3 , 1, 0)
mhas_w2 %>% tab(r2_retired)
mhas_w2$r2_unemployed <- ifelse(mhas_w2$r2lbrf_m == 2, 1, 0)
mhas_w2 %>% tab(r2_unemployed)



# Create dummies for low/high skill, blue/white collar workers for wave 11
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
# mhas_w4$r4_high_white_collar <- ifelse(mhas_w4$r4jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# mhas_w4$r4_low_white_collar <- ifelse(mhas_w4$r4jcoccb %in% c(17, 18), 1, 0)
# mhas_w4$r4_high_blue_collar <- ifelse(mhas_w4$r4jcoccb %in% c(20, 21, 22, 23), 1, 0)
# mhas_w4$r4_low_blue_collar <- ifelse(mhas_w4$r4jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(mhas_w4$r4lbrf)

mhas_w4$r4_employed <- ifelse(mhas_w4$r4lbrf_m == 1 , 1, 0)
mhas_w4 %>% tab(r4_employed)
mhas_w4$r4_retired <- ifelse(mhas_w4$r4lbrf_m == 3 & 5, 1, 0)
mhas_w4 %>% tab(r4_retired)
mhas_w4$r4_unemployed <- ifelse(mhas_w4$r4lbrf_m == 2, 1, 0)
mhas_w4 %>% tab(r4_unemployed)


# # Employed workers low/high skill white/blue collar
# mhas_w2$emp_r2_high_white_collar <- ifelse(mhas_w2$r2_employed == 1 &
#                                             mhas_w2$r2_high_white_collar == 1, 1, 0)
# mhas_w2$emp_r2_low_white_collar <- ifelse(mhas_w2$r2_employed == 1 &
#                                            mhas_w2$r2_low_white_collar == 1, 1, 0)
# mhas_w2$emp_r2_high_blue_collar <- ifelse(mhas_w2$r2_employed == 1 &
#                                            mhas_w2$r2_high_blue_collar == 1, 1, 0)
# mhas_w2$emp_r2_low_blue_collar <- ifelse(mhas_w2$r2_employed == 1 &
#                                           mhas_w2$r2_low_blue_collar == 1, 1, 0)


# mhas_w2 %>% tab(emp_r2_high_white_collar)
# mhas_w2 %>% tab(r2_high_white_collar)
# mhas_w2 %>% tab(emp_r2_low_white_collar)
# mhas_w2 %>% tab(r2_low_white_collar)
# mhas_w2 %>% tab(emp_r2_high_blue_collar)
# mhas_w2 %>% tab(r2_high_blue_collar)
# mhas_w2 %>% tab(emp_r2_low_blue_collar)
# mhas_w2 %>% tab(r2_low_blue_collar)


# Employed workers low/high skill white/blue collar
# mhas_w4$emp_r4_high_white_collar <- ifelse(mhas_w4$r4_employed == 1 &
#                                               mhas_w4$r4_high_white_collar == 1, 1, 0)
# mhas_w4$emp_r4_low_white_collar <- ifelse(mhas_w4$r4_employed == 1 &
#                                              mhas_w4$r4_low_white_collar == 1, 1, 0)
# mhas_w4$emp_r4_high_blue_collar <- ifelse(mhas_w4$r4_employed == 1 &
#                                              mhas_w4$r4_high_blue_collar == 1, 1, 0)
# mhas_w4$emp_r4_low_blue_collar <- ifelse(mhas_w4$r4_employed == 1 &
#                                             mhas_w4$r4_low_blue_collar == 1, 1, 0)


# mhas_w4 %>% tab(emp_r4_high_white_collar)
# mhas_w4 %>% tab(r4_high_white_collar)
# mhas_w4 %>% tab(emp_r4_low_white_collar)
# mhas_w4 %>% tab(r4_low_white_collar)
# mhas_w4 %>% tab(emp_r4_high_blue_collar)
# mhas_w4 %>% tab(r4_high_blue_collar)
# mhas_w4 %>% tab(emp_r4_low_blue_collar)
# mhas_w4 %>% tab(r4_low_blue_collar) 

unique(mhas_w2$r2drinkd)

#Drinking behaviours
#drink behaviors wave 7
mhas_w2$r2_drink_NO <- ifelse(mhas_w2$r2drinkd == 0, 1, 0) #does not drink
mhas_w2$r2_drink_OTS <- ifelse(mhas_w2$r2drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
mhas_w2$r2_drink_MDW <- ifelse(mhas_w2$r2drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
mhas_w2$r2_drink_EDW <- ifelse(mhas_w2$r2drinkd == 7, 1, 0) #drink every day of the week


#drink behaviors wave  11
mhas_w4$r4_drink_NO <- ifelse(mhas_w4$r4drinkd == 0, 1, 0) #does not drink
mhas_w4$r4_drink_OTS <- ifelse(mhas_w4$r4drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
mhas_w4$r4_drink_MDW <- ifelse(mhas_w4$r4drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
mhas_w4$r4_drink_EDW <- ifelse(mhas_w4$r4drinkd == 7, 1, 0) #drink every day of the week

# Create dummy variables for each category of ADLs for wave 1
mhas_w2$r2adlfive_0 <- ifelse(mhas_w2$r2adlfive == 0, 1, 0) # No impedings
mhas_w2$r2adlfive_1_2 <- ifelse(mhas_w2$r2adlfive == 1 | mhas_w2$r2adlfive == 2, 1, 0) # 1 or 2 impedings
mhas_w2$r2adlfive_3_plus <- ifelse(mhas_w2$r2adlfive >= 3, 1, 0) # 3+ impedings

# Create dummy variables for each category of ADLs for wave 6
mhas_w4$r4adlfive_0 <- ifelse(mhas_w4$r4adlfive == 0, 1, 0) # No impedings
mhas_w4$r4adlfive_1_2 <- ifelse(mhas_w4$r4adlfive == 1 | mhas_w4$r4adlfive == 2, 1, 0) # 1 or 2 impedings
mhas_w4$r4adlfive_3_plus <- ifelse(mhas_w4$r4adlfive >= 3, 1, 0) # 3+ impedings

#summary of IADLs for wave 1
mhas_w2$r2_iadl_0 <- ifelse(mhas_w2$r2shopa == 0 & mhas_w2$r2mealsa == 0
                           & mhas_w2$r2medsa == 0 & mhas_w2$r2moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
mhas_w2$r2_iadl_sum <- mhas_w2$r2shopa + mhas_w2$r2mealsa + mhas_w2$r2medsa + mhas_w2$r2moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
mhas_w2$r2_iadl_1_2 <- ifelse(mhas_w2$r2_iadl_sum == 1 | mhas_w2$r2_iadl_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
mhas_w2$r2_iadl_3_plus <- ifelse(mhas_w2$r2_iadl_sum >= 3, 1, 0)

#summary of IADLs for wave 6
mhas_w4$r4_iadl_0 <- ifelse(mhas_w4$r4shopa == 0 & mhas_w4$r4mealsa == 0
                             & mhas_w4$r4medsa == 0 & mhas_w4$r4moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
mhas_w4$r4_iadl_sum <- mhas_w4$r4shopa + mhas_w4$r4mealsa + mhas_w4$r4medsa + mhas_w4$r4moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
mhas_w4$r4_iadl_1_2 <- ifelse(mhas_w4$r4_iadl_sum >= 1 & mhas_w4$r4_iadl_sum <= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
mhas_w4$r4_iadl_3_plus <- ifelse(mhas_w4$r4_iadl_sum >= 3, 1, 0)


#Disease summary
mhas_w2$r2_healthy <- ifelse(mhas_w2$r2cancre == 0 & mhas_w2$r2hibpe == 0
                            & mhas_w2$r2diabe == 0 
                            & mhas_w2$r2respe ==0 & mhas_w2$r2stroke ==0, 1, 0) # No impedings


#------------------------------------------------------------------------------------------
# Create a new variable that sums up the four existing dummy variables
mhas_w2$r2_dis_sum <- mhas_w2$r2cancre  + mhas_w2$r2hibpe + mhas_w2$r2diabe +
  mhas_w2$r2respe + mhas_w2$r2stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
mhas_w2$r2_morbi_2 <- ifelse(mhas_w2$r2_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
mhas_w2$r2_morbi_3_plus <- ifelse(mhas_w2$r2_dis_sum >= 3, 1, 0)

#Disease summary
mhas_w4$r4_healthy <- ifelse(mhas_w4$r4cancre == 0 & mhas_w4$r4hibpe == 0
                              & mhas_w4$r4diabe == 0 
                              & mhas_w4$r4respe ==0 & mhas_w4$r4stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
mhas_w4$r4_dis_sum <- mhas_w4$r4cancre  + mhas_w4$r4hibpe + mhas_w4$r4diabe +
  mhas_w4$r4respe + mhas_w4$r4stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
mhas_w4$r4_morbi_2 <- ifelse(mhas_w4$r4_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
mhas_w4$r4_morbi_3_plus <- ifelse(mhas_w4$r4_dis_sum >= 3, 1, 0)

#----------------------------------------------------
# level of functioning in social and daily activities
#----------------------------------------------------
mhas_w2$r2_independent <- ifelse(mhas_w2$r2adlfive == 0 | mhas_w2$r2_iadl_sum == 0, 1, 0)
mhas_w4$r4_independent <- ifelse(mhas_w4$r4adlfive == 0 | mhas_w4$r4_iadl_sum == 0, 1, 0)

mhas_w2$r2_semifunctional <- ifelse(mhas_w2$r2adlfive == 1 | mhas_w2$r2_iadl_sum == 1 | mhas_w2$r2adlfive == 2 | mhas_w2$r2_iadl_sum == 2, 1, 0)
mhas_w4$r4_semifunctional <- ifelse(mhas_w4$r4adlfive == 1 | mhas_w4$r4_iadl_sum == 1 | mhas_w4$r4adlfive == 2 | mhas_w4$r4_iadl_sum == 2, 1, 0)

mhas_w2$r2_notfunctional <- ifelse(mhas_w2$r2adlfive >= 3 | mhas_w2$r2_iadl_sum >= 3, 1, 0)
mhas_w4$r4_notfunctional <- ifelse(mhas_w4$r4adlfive >= 3 | mhas_w4$r4_iadl_sum >= 3, 1, 0)

#-----------------------------------
# risky behaviors
#-----------------------------------
mhas_w2$r2_drink_EV <- ifelse(mhas_w2$r2drinkn >= 1, 1, 0)
mhas_w4$r4_drink_EV <- ifelse(mhas_w4$r4drinkn >= 1, 1, 0)
# For wave 1
mhas_w2$r2_risky <- ifelse(mhas_w2$r2smokev >= 1 | mhas_w2$r2_drink_EV >= 1, 1, 0)

# For wave 6
mhas_w4$r4_risky <- ifelse(mhas_w4$r4smokev >= 1 | mhas_w4$r4_drink_EV >= 1, 1, 0)

# For wave 1
mhas_w2$r2_risky <- ifelse(is.na(mhas_w2$r2_risky), 0, mhas_w2$r2_risky)

# For wave 6
mhas_w4$r4_risky <- ifelse(is.na(mhas_w4$r4_risky), 0, mhas_w4$r4_risky)


#-----------------------------------
# physical activity behaviors
#-----------------------------------
mhas_w2$r2no_phys_act <- ifelse(mhas_w2$r2vigact == 0 , 1, 0)
mhas_w2$r2good_phys_act <-  ifelse(mhas_w2$r2vigact == 1 , 1, 0)

# For wave 6
mhas_w4$r4no_phys_act <- ifelse(mhas_w4$r4vigact == 0, 1, 0)
mhas_w4$r4good_phys_act <- ifelse(mhas_w4$r4vigact == 1, 1, 0)
mhas_w2 %>% tab(r2no_phys_act)
mhas_w2 %>% tab(r2good_phys_act)
mhas_w4 %>% tab(r4no_phys_act)
mhas_w4 %>% tab(r4good_phys_act)

#-----------------------------------
#              family 
#-----------------------------------

mhas_w2$r2_single <- ifelse(mhas_w2$h2hhres == 1, 1, 0)
mhas_w2$r2_notsingle <- ifelse(mhas_w2$h2hhres >= 2, 1, 0)
mhas_w2 %>% tab(r2_single)
mhas_w2 %>% tab(r2_notsingle)

mhas_w4$r4_single <- ifelse(mhas_w4$h4hhres == 1, 1, 0)
mhas_w4$r4_notsingle <- ifelse(mhas_w4$h4hhres >= 2, 1, 0)
mhas_w4 %>% tab(r4_single)
mhas_w4 %>% tab(r4_notsingle)

#-----------------------------------
#              gender
#-----------------------------------
mhas_w2$man <- ifelse(mhas_w2$ragender == 1, 1, 0)
mhas_w2$woman <- ifelse(mhas_w2$ragender == 2, 1, 0)

mhas_w4$man <- ifelse(mhas_w4$ragender == 1, 1, 0)
mhas_w4$woman <- ifelse(mhas_w4$ragender == 2, 1, 0)


# write.csv(mhas_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/mhas_cross_section.csv")
# 
# write_dta(mhas_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/mhas_cross_section.dta")
# write.csv(mhas_w2, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/mhas_w2.csv")
write_dta(mhas_w2, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS/output/mhas_w2.dta")
# write.csv(mhas_w4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/mhas_w4.csv")
write_dta(mhas_w4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS/output/mhas_w4.dta")
# 

#write_dta(mhas_w2, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/mhas_w2.dta")
#write_dta(mhas_w4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/mhas_w4.dta")


#--dataset divided by gender
male_mhasw2 <- mhas_w2 %>%
  filter(man==1)
count(male_mhasw2)

female_mhasw2 <- mhas_w2 %>%
  filter(woman==1)
count(female_mhasw2)

male_mhasw4 <- mhas_w4 %>%
  filter(man==1)
count(male_mhasw4)

female_mhasw4 <- mhas_w4 %>%
  filter(woman==1)
count(female_mhasw4)

write_dta(male_mhasw2, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS/output/male_mhasw2.dta")
write_dta(female_mhasw2, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS/output/female_mhasw2.dta")

write_dta(male_mhasw4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS/output/male_mhasw4.dta")
write_dta(female_mhasw4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/MHAS/output/female_mhasw4.dta")

