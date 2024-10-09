remove(list=ls())
gc()
# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(sharp)
library(tabulator)
library(pastecs)
library(openxlsx)
library(fastDummies)
#WINHOME
setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS")
load("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/MEMHRS_definitive.RData")
#SERVER
#setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS")
#load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/MEMHRS_definitive.RData")
#MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/HRS")
#load("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/HRS/MEMHRS_definitive.RData")
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
hrs_w7 <- hrsdata %>%
  filter(r7iwstat == 1)
hrs_w12 <- hrsdata %>%
    filter(r12iwstat == 1 | r12iwstat == 4)
hrs_w7 <- hrsdata %>%
  select(hhidpn, r7cancre, r7diabe, r7hearte, r7hibpe, r7lunge, r7stroke, r7bmi,
         r7gripsum,r7agey_e, r7vgactx,
         r7mdactx, r7smokev, r7smoken, r7drinkd,r7drinkn,
         raeducl, r7slfemp, r7jlasty, r7lbrf, r7jcoccb, r7iearn, r7rcany, r7adlfive,
  r7shopa, r7mealsa, r7medsa, r7moneya, r7ipena, h7itot, ragender, h7hhres, ragender, rawtsamp
) %>%
  filter(r7agey_e >= 50)

hrs_w12 <- hrsdata %>%
  select(hhidpn, r12bmi, r12cancre, r12diabe, r12hearte, r12hibpe,r12lunge, r12stroke,
         r12gripsum, r12agey_e, r12vgactx, r12mdactx,r12smokev,
         r12drinkn, r12drinkd, raeducl, r12slfemp, r12ipena,
         r12jlasty,r12lbrf, r12jcoccb, r12iearn, r12rcany, r12adlfive, r12shopa, r12mealsa, r12medsa, r12moneya,
         h12itot, h12hhres, ragender, rawtsamp ) %>%
  filter(r12agey_e >= 50)

# Convert income for 2002 to constant prices
hrs_w7$r7iearn_constant <- (hrs_w7$r7iearn / cpi_data$c2003cpindex) * 100
hrs_w7$r7ipena_constant <- (hrs_w7$r7ipena / cpi_data$c2003cpindex) * 100
hrs_w7$r7earnings_constant <- hrs_w7$r7iearn_constant + hrs_w7$r7ipena_constant
hrs_w7$h7itot_constant <- (hrs_w7$h7itot / cpi_data$c2003cpindex) * 100
# Convert income for 2012 to constant prices
hrs_w12$r12iearn_constant <- (hrs_w12$r12iearn / cpi_data$c2003cpindex) * 100
hrs_w12$r12ipena_constant <- (hrs_w12$r12ipena / cpi_data$c2003cpindex) * 100
hrs_w12$r12earnings_constant <- hrs_w12$r12iearn_constant + hrs_w12$r12ipena_constant
hrs_w12$h12itot_constant <- (hrs_w12$h12itot / cpi_data$c2003cpindex) * 100

# Create dummy variables for education
hrs_w7 <- dummy_cols(hrs_w7, select_columns = 'raeducl')

hrs_w7 <- rename(hrs_w7,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                      tertiary_educ = raeducl_3)
# Create dummy variables for education
hrs_w12 <- dummy_cols(hrs_w12, select_columns = 'raeducl')

hrs_w12 <- rename(hrs_w12,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                      tertiary_educ = raeducl_3)
# Create dummy variables for home ownership in wave 1
#hrs_small <- dummy_cols(hrs_small, select_columns = 'r1hownrnt')
# Create dummy variables for home ownership in wave 6
#hrs_small <- dummy_cols(hrs_small, select_columns = 'r6hownrnt')
# Define the income brackets based on the percentiles
stat.desc(hrs_w7$r7iearn_constant)
hrs_w7$r7_low_income <- ifelse(hrs_w7$r7iearn_constant <= 18000, 1, 0)
hrs_w7$r7_middle_income <- ifelse(hrs_w7$r7iearn_constant > 18000 & 
                                         hrs_w7$r7iearn_constant <= 35438.81, 1, 0)
hrs_w7$r7_high_income <- ifelse(hrs_w7$r7iearn_constant > 35438.81, 1, 0)
# Define the income brackets based on the percentiles
hrs_w12$r12_low_income <- ifelse(hrs_w12$r12iearn_constant <= 18000, 1, 0)
hrs_w12$r12_middle_income <- ifelse(hrs_w12$r12iearn_constant > 18000
                                       & hrs_w12$r12iearn_constant <= 35438.81, 1, 0)
hrs_w12$r12_high_income <- ifelse(hrs_w12$r12iearn_constant > 35438.81, 1, 0)

# Define the BMI categories based on the WHO classification for wave 1
hrs_w7$r7_underweight <- ifelse(hrs_w7$r7bmi < 18.5, 1, 0)
hrs_w7$r7_normal_weight <- ifelse(hrs_w7$r7bmi >= 18.5 & hrs_w7$r7bmi < 25, 1, 0)
hrs_w7$r7_overweight <- ifelse(hrs_w7$r7bmi >= 25 & hrs_w7$r7bmi < 30, 1, 0)
hrs_w7$r7_obese <- ifelse(hrs_w7$r7bmi >= 30, 1, 0)

# Define the BMI categories based on the WHO classification for wave 6 
hrs_w12$r12_underweight <- ifelse(hrs_w12$r12bmi < 18.5, 1, 0)
hrs_w12$r12_normal_weight <- ifelse(hrs_w12$r12bmi >= 18.5 & hrs_w12$r12bmi < 25, 1, 0)
hrs_w12$r12_overweight <- ifelse(hrs_w12$r12bmi >= 25 & hrs_w12$r12bmi < 30, 1, 0)
hrs_w12$r12_obese <- ifelse(hrs_w12$r12bmi >= 30, 1, 0)


# Create dummies for low/high skill, blue/white collar workers for wave 7
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
hrs_w7$r7_high_white_collar <- ifelse(hrs_w7$r7jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
hrs_w7$r7_low_white_collar <- ifelse(hrs_w7$r7jcoccb %in% c(17, 18), 1, 0)
hrs_w7$r7_high_blue_collar <- ifelse(hrs_w7$r7jcoccb %in% c(20, 21, 22, 23), 1, 0)
hrs_w7$r7_low_blue_collar <- ifelse(hrs_w7$r7jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(hrs_w7$r7lbrf)

hrs_w7$r7_employed <- ifelse(hrs_w7$r7lbrf  %in% c(1, 2), 1, 0)
hrs_w7 %>% tab(r7_employed)
hrs_w7$r7_retired <- ifelse(hrs_w7$r7lbrf  %in% c(4, 5), 1, 0)
hrs_w7 %>% tab(r7_retired)
hrs_w7$r7_unemployed <- ifelse(hrs_w7$r7lbrf == 3, 1, 0)
hrs_w7 %>% tab(r7_unemployed)



# Create dummies for low/high skill, blue/white collar workers for wave 11
unique(hrs_w12$r12jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
#better way
hrs_w12$r12_high_white_collar <- ifelse(hrs_w12$r12jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
hrs_w12$r12_low_white_collar <- ifelse(hrs_w12$r12jcoccb %in% c(17, 18), 1, 0)
hrs_w12$r12_high_blue_collar <- ifelse(hrs_w12$r12jcoccb %in% c(20, 21, 22, 23), 1, 0)
hrs_w12$r12_low_blue_collar <- ifelse(hrs_w12$r12jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(hrs_w12$r12lbrf)

hrs_w12$r12_employed <- ifelse(hrs_w12$r12lbrf %in% c(1, 2), 1, 0)
hrs_w12 %>% tab(r12_employed)
hrs_w12$r12_retired <- ifelse(hrs_w12$r12lbrf %in% c(4, 5), 1, 0)
hrs_w12 %>% tab(r12_retired)
hrs_w12$r12_unemployed <- ifelse(hrs_w12$r12lbrf == 3, 1, 0)
hrs_w12 %>% tab(r12_unemployed)


# Employed workers low/high skill white/blue collar
hrs_w7$emp_r7_high_white_collar <- ifelse(hrs_w7$r7_employed == 1 &
                                                         hrs_w7$r7_high_white_collar == 1, 1, 0)
hrs_w7$emp_r7_low_white_collar <- ifelse(hrs_w7$r7_employed == 1 &
                                                        hrs_w7$r7_low_white_collar == 1, 1, 0)
hrs_w7$emp_r7_high_blue_collar <- ifelse(hrs_w7$r7_employed == 1 &
                                                        hrs_w7$r7_high_blue_collar == 1, 1, 0)
hrs_w7$emp_r7_low_blue_collar <- ifelse(hrs_w7$r7_employed == 1 &
                                                       hrs_w7$r7_low_blue_collar == 1, 1, 0)


hrs_w7 %>% tab(emp_r7_high_white_collar)
hrs_w7 %>% tab(r7_high_white_collar)
hrs_w7 %>% tab(emp_r7_low_white_collar)
hrs_w7 %>% tab(r7_low_white_collar)
hrs_w7 %>% tab(emp_r7_high_blue_collar)
hrs_w7 %>% tab(r7_high_blue_collar)
hrs_w7 %>% tab(emp_r7_low_blue_collar)
hrs_w7 %>% tab(r7_low_blue_collar)


# Employed workers low/high skill white/blue collar
hrs_w12$emp_r12_high_white_collar <- ifelse(hrs_w12$r12_employed == 1 &
                                                         hrs_w12$r12_high_white_collar == 1, 1, 0)
hrs_w12$emp_r12_low_white_collar <- ifelse(hrs_w12$r12_employed == 1 &
                                                        hrs_w12$r12_low_white_collar == 1, 1, 0)
hrs_w12$emp_r12_high_blue_collar <- ifelse(hrs_w12$r12_employed == 1 &
                                                        hrs_w12$r12_high_blue_collar == 1, 1, 0)
hrs_w12$emp_r12_low_blue_collar <- ifelse(hrs_w12$r12_employed == 1 &
                                                       hrs_w12$r12_low_blue_collar == 1, 1, 0)


hrs_w12 %>% tab(emp_r12_high_white_collar)
hrs_w12 %>% tab(r12_high_white_collar)
hrs_w12 %>% tab(emp_r12_low_white_collar)
hrs_w12 %>% tab(r12_low_white_collar)
hrs_w12 %>% tab(emp_r12_high_blue_collar)
hrs_w12 %>% tab(r12_high_blue_collar)
hrs_w12 %>% tab(emp_r12_low_blue_collar)
hrs_w12 %>% tab(r12_low_blue_collar) 

unique(hrs_w7$r7drinkd)

#Drinking behaviours
#drink behaviors wave 7
hrs_w7$r7_drink_NO <- ifelse(hrs_w7$r7drinkd == 0, 1, 0) #does not drink
hrs_w7$r7_drink_OTS <- ifelse(hrs_w7$r7drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
hrs_w7$r7_drink_MDW <- ifelse(hrs_w7$r7drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
hrs_w7$r7_drink_EDW <- ifelse(hrs_w7$r7drinkd == 7, 1, 0) #drink every day of the week


#drink behaviors wave  11
hrs_w12$r12_drink_NO <- ifelse(hrs_w12$r12drinkd == 0, 1, 0) #does not drink
hrs_w12$r12_drink_OTS <- ifelse(hrs_w12$r12drinkd == 1 & 2 & 3, 1, 0) #drink one to several times a week
hrs_w12$r12_drink_MDW <- ifelse(hrs_w12$r12drinkd == 4 & 5 & 6, 1, 0) #drink most days of the week
hrs_w12$r12_drink_EDW <- ifelse(hrs_w12$r12drinkd == 7, 1, 0) #drink every day of the week

# Create dummy variables for each category of ADLs for wave 1
hrs_w7$r7adlfive_0 <- ifelse(hrs_w7$r7adlfive == 0, 1, 0) # No impedings
hrs_w7$r7adlfive_1_2 <- ifelse(hrs_w7$r7adlfive == 1 | hrs_w7$r7adlfive == 2, 1, 0) # 1 or 2 impedings
hrs_w7$r7adlfive_3_plus <- ifelse(hrs_w7$r7adlfive >= 3, 1, 0) # 3+ impedings

# Create dummy variables for each category of ADLs for wave 6
hrs_w12$r12adlfive_0 <- ifelse(hrs_w12$r12adlfive == 0, 1, 0) # No impedings
hrs_w12$r12adlfive_1_2 <- ifelse(hrs_w12$r12adlfive == 1 | hrs_w12$r12adlfive == 2, 1, 0) # 1 or 2 impedings
hrs_w12$r12adlfive_3_plus <- ifelse(hrs_w12$r12adlfive >= 3, 1, 0) # 3+ impedings

#summary of IADLs for wave 1
hrs_w7$r7_iadl_0 <- ifelse(hrs_w7$r7shopa == 0 & hrs_w7$r7mealsa == 0
                              & hrs_w7$r7medsa == 0 & hrs_w7$r7moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
hrs_w7$r7_iadl_sum <- hrs_w7$r7shopa + hrs_w7$r7mealsa + hrs_w7$r7medsa + hrs_w7$r7moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
hrs_w7$r7_iadl_1_2 <- ifelse(hrs_w7$r7_iadl_sum == 1 | hrs_w7$r7_iadl_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
hrs_w7$r7_iadl_3_plus <- ifelse(hrs_w7$r7_iadl_sum >= 3, 1, 0)

#summary of IADLs for wave 6
hrs_w12$r12_iadl_0 <- ifelse(hrs_w12$r12shopa == 0 & hrs_w12$r12mealsa == 0
                              & hrs_w12$r12medsa == 0 & hrs_w12$r12moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
hrs_w12$r12_iadl_sum <- hrs_w12$r12shopa + hrs_w12$r12mealsa + hrs_w12$r12medsa + hrs_w12$r12moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
hrs_w12$r12_iadl_1_2 <- ifelse(hrs_w12$r12_iadl_sum >= 1 & hrs_w12$r12_iadl_sum <= 2, 1, 0)
hrs_w12 %>% tab(r12_iadl_1_2)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
hrs_w12$r12_iadl_3_plus <- ifelse(hrs_w12$r12_iadl_sum >= 3, 1, 0)


#Disease summary
hrs_w7$r7_healthy <- ifelse(hrs_w7$r7cancre == 0 & hrs_w7$r7hibpe == 0
                              & hrs_w7$r7diabe == 0 & hrs_w7$r7hearte ==0
                              & hrs_w7$r7lunge ==0 & hrs_w7$r7stroke ==0, 1, 0) # No impedings


#------------------------------------------------------------------------------------------
# Create a new variable that sums up the four existing dummy variables
hrs_w7$r7_dis_sum <- hrs_w7$r7cancre + hrs_w7$r7hearte + hrs_w7$r7hibpe + hrs_w7$r7diabe +
  hrs_w7$r7lunge + hrs_w7$r7stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
hrs_w7$r7_morbi_2 <- ifelse(hrs_w7$r7_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
hrs_w7$r7_morbi_3_plus <- ifelse(hrs_w7$r7_dis_sum >= 3, 1, 0)

#Disease summary
hrs_w12$r12_healthy <- ifelse(hrs_w12$r12cancre == 0 & hrs_w12$r12hibpe == 0
                              & hrs_w12$r12diabe == 0 & hrs_w12$r12hearte ==0
                              & hrs_w12$r12lunge ==0 & hrs_w12$r12stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
hrs_w12$r12_dis_sum <- hrs_w12$r12cancre + hrs_w12$r12hearte + hrs_w12$r12hibpe + hrs_w12$r12diabe +
  hrs_w12$r12lunge + hrs_w12$r12stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
hrs_w12$r12_morbi_2 <- ifelse(hrs_w12$r12_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
hrs_w12$r12_morbi_3_plus <- ifelse(hrs_w12$r12_dis_sum >= 3, 1, 0)

#----------------------------------------------------
# level of functioning in social and daily activities
#----------------------------------------------------
hrs_w7$r7_independent <- ifelse(hrs_w7$r7adlfive == 0 | hrs_w7$r7_iadl_sum == 0, 1, 0)
hrs_w12$r12_independent <- ifelse(hrs_w12$r12adlfive == 0 | hrs_w12$r12_iadl_sum == 0, 1, 0)

hrs_w7$r7_semifunctional <- ifelse(hrs_w7$r7adlfive == 1 | hrs_w7$r7_iadl_sum == 1 | hrs_w7$r7adlfive == 2 | hrs_w7$r7_iadl_sum == 2, 1, 0)
hrs_w12$r12_semifunctional <- ifelse(hrs_w12$r12adlfive == 1 | hrs_w12$r12_iadl_sum == 1 | hrs_w12$r12adlfive == 2 | hrs_w12$r12_iadl_sum == 2, 1, 0)

hrs_w7$r7_notfunctional <- ifelse(hrs_w7$r7adlfive >= 3 | hrs_w7$r7_iadl_sum >= 3, 1, 0)
hrs_w12$r12_notfunctional <- ifelse(hrs_w12$r12adlfive >= 3 | hrs_w12$r12_iadl_sum >= 3, 1, 0)

#-----------------------------------
# risky behaviors
#-----------------------------------
hrs_w7$r7_drink_EV <- ifelse(hrs_w7$r7drinkn >= 1, 1, 0)
hrs_w12$r12_drink_EV <- ifelse(hrs_w12$r12drinkn >= 1, 1, 0)
# For wave 1
hrs_w7$r7_risky <- ifelse(hrs_w7$r7smokev >= 1 | hrs_w7$r7_drink_EV >= 1, 1, 0)

# For wave 6
hrs_w12$r12_risky <- ifelse(hrs_w12$r12smokev >= 1 | hrs_w12$r12_drink_EV >= 1, 1, 0)

# For wave 1
hrs_w7$r7_risky <- ifelse(is.na(hrs_w7$r7_risky), 0, hrs_w7$r7_risky)

# For wave 6
hrs_w12$r12_risky <- ifelse(is.na(hrs_w12$r12_risky), 0, hrs_w12$r12_risky)


#-----------------------------------
# physical activity behaviors
#-----------------------------------
hrs_w7$r7no_phys_act <- ifelse(hrs_w7$r7vgactx == 5 | hrs_w7$r7mdactx == 5, 1, 0)
hrs_w7 %>% tab(r7no_phys_act)
hrs_w7$r7low_phys_act <- ifelse(hrs_w7$r7vgactx == 4 | hrs_w7$r7mdactx == 4, 1, 0)
hrs_w7 %>% tab(r7low_phys_act)
hrs_w7$r7good_phys_act <- ifelse(hrs_w7$r7vgactx == 3 | hrs_w7$r7mdactx == 3 | hrs_w7$r7vgactx == 2 | hrs_w7$r7mdactx == 2 , 1, 0)
hrs_w7 %>% tab(r7good_phys_act)
hrs_w7$r7vgood_phys_act <- ifelse(hrs_w7$r7vgactx == 2 | hrs_w7$r7mdactx == 2, 1, 0)
hrs_w7 %>% tab(r7vgood_phys_act)

# For wave 6
hrs_w12$r12no_phys_act <- ifelse(hrs_w12$r12vgactx == 5 | hrs_w12$r12mdactx == 5, 1, 0)
hrs_w12$r12low_phys_act <- ifelse(hrs_w12$r12vgactx == 4 | hrs_w12$r12mdactx == 4, 1, 0)
hrs_w12$r12good_phys_act <- ifelse(hrs_w12$r12vgactx == 3 | hrs_w12$r12mdactx == 3 |hrs_w12$r12vgactx == 2 | hrs_w12$r12mdactx == 2 , 1, 0)
hrs_w12$r12vgood_phys_act <- ifelse(hrs_w12$r12vgactx == 2 | hrs_w12$r12mdactx == 2, 1, 0)
hrs_w12 %>% tab(r12no_phys_act)
hrs_w12 %>% tab(r12low_phys_act)
hrs_w12 %>% tab(r12good_phys_act)
hrs_w12 %>% tab(r12vgood_phys_act)

#-----------------------------------
#              family 
#-----------------------------------

hrs_w7$r7_single <- ifelse(hrs_w7$h7hhres == 1, 1, 0)
hrs_w7$r7_notsingle <- ifelse(hrs_w7$h7hhres >= 2, 1, 0)
hrs_w7 %>% tab(r7_single)
hrs_w7 %>% tab(r7_notsingle)

hrs_w12$r12_single <- ifelse(hrs_w12$h12hhres == 1, 1, 0)
hrs_w12$r12_notsingle <- ifelse(hrs_w12$h12hhres >= 2, 1, 0)
hrs_w12 %>% tab(r12_single)
hrs_w12 %>% tab(r12_notsingle)

#-----------------------------------
#              gender
#-----------------------------------
hrs_w7$man <- ifelse(hrs_w7$ragender == 1, 1, 0)
hrs_w7$woman <- ifelse(hrs_w7$ragender == 2, 1, 0)

hrs_w12$man <- ifelse(hrs_w12$ragender == 1, 1, 0)
hrs_w12$woman <- ifelse(hrs_w12$ragender == 2, 1, 0)


# write.csv(hrs_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/hrs_cross_section.csv")
# 
# write_dta(hrs_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_cross_section.dta")
# write.csv(hrs_w7, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/hrs_w7.csv")
write_dta(hrs_w7, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_w7.dta")
# write.csv(hrs_w12, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/hrs_w12.csv")
write_dta(hrs_w12, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_w12.dta")
# 

#write_dta(hrs_w7, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_w7.dta")
#write_dta(hrs_w12, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_w12.dta")

#--dataset divided by gender
male_hrsw7 <- hrs_w7 %>%
  filter(man==1)
count(male_hrsw7)

female_hrsw7 <- hrs_w7 %>%
  filter(woman==1)
count(female_hrsw7)

male_hrsw12 <- hrs_w12 %>%
  filter(man==1)
count(male_hrsw12)

female_hrsw12 <- hrs_w12 %>%
  filter(woman==1)
count(female_hrs_w12)

write_dta(male_hrsw7, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/malehrs_w7.dta")
write_dta(female_hrsw7, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/femalehrs_w7.dta")

write_dta(male_hrsw12, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/malehrs_w12.dta")
write_dta(female_hrsw12, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/femalehrs_w12.dta")

