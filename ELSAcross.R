remove(list=ls())
gc()
# Load required libraries
library(dplyr)
library(quantmod)
library(haven)
library(ggplot2)
library(sharp)
library(tabulator)
library(pastecs)
library(openxlsx)
library(fastDummies)
#WINHOME
#setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS")
#load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/MEMHRS_definitive.RData")
#SERVER
setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata")
load("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/MEMELSA.RData")
#MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/ELSA/UKDA-5050-stata")
#load("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/ELSA/UKDA-5050-stata/MEMELSA.RData")
#-------FORMATTING CPI DATA-------
#elsadata <- read_dta("output/h_elsa_g3.dta")
# Set the currency pair and time period
currency_pair <- "GBP/USD"
start_date <- "2012-01-01"
end_date <- "2012-12-31"

# Get the exchange rate data
getSymbols("GBPUSD=X", src="yahoo", from=start_date, to=end_date, auto.assign=TRUE)

# Extract the closing prices
exchange_rates <- Cl(`GBPUSD=X`)

# Calculate the mean exchange rate for the year
mean_exchange_rate <- mean(exchange_rates, na.rm=TRUE)
# Print the result
print(mean_exchange_rate)
#1.753 for 2003
#1.585 for 2012

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
elsa_w2 <- elsadata %>%
  filter(r2iwstat == 1)
elsa_w8 <- elsadata %>%
  filter(r8iwstat == 1 | r8iwstat == 4)
elsa_w2 <- elsadata %>%
  select( idauniq, r2cancre, r2diabe, r2hearte, r2hibpe, r2lunge, r2stroke, r2mbmi,
         r2gripsum,r2agey, r2vgactx_e,
         r2mdactx_e, r2smokev,r2lbrf_e, r2smoken, r2drinkd_e,r2drinkn_e,
         raeducl, r2slfemp, r2adlfive,
         r2shopa, r2mealsa, r2medsa, r2moneya, h2itot, ragender, h2hhres, ragender,r2cwtresp
  ) %>%
  filter(r2agey >= 50)

elsa_w8 <- elsadata %>%
  select(  idauniq, r8cancre, r8diabe, r8hearte, r8hibpe, r8lunge, r8stroke, r8mbmi,
           r8gripsum,r8agey, r8vgactx_e,
           r8mdactx_e, r8smokev,r8lbrf_e, r8smoken, r8drinkd_e,r8drinkwn_e,
           raeducl, r8slfemp, r8adlfive,
           r8shopa, r8mealsa, r8medsa, r8moneya, h8itot, ragender, h8hhres, ragender, r8cwtresp
  ) %>%
  filter(r8agey >= 50)

# Convert income for 2002 to constant prices
elsa_w2$h2itot_constant <- (elsa_w2$h2itot / cpi_data$c2003cpindex) * 100
# Convert income for 2012 to constant prices
elsa_w8$h8itot_constant <- (elsa_w8$h8itot / cpi_data$c2003cpindex) * 100

hist(elsa_w2$h2itot_constant)

# Convert to USD using the historical exchange rates
elsa_w2$h2itot_constant <- elsa_w2$h2itot_constant * 1.753
elsa_w8$h8itot_constant <- elsa_w8$h8itot_constant * 1.584


# Create dummy variables for education
elsa_w2 <- dummy_cols(elsa_w2, select_columns = 'raeducl')

elsa_w2 <- rename(elsa_w2,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                 tertiary_educ = raeducl_3)
# Create dummy variables for education
elsa_w8 <- dummy_cols(elsa_w8, select_columns = 'raeducl')

elsa_w8 <- rename(elsa_w8,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                  tertiary_educ = raeducl_3)
# Create dummy variables for home ownership in wave 1
#hrs_small <- dummy_cols(hrs_small, select_columns = 'r1hownrnt')
# Create dummy variables for home ownership in wave 6
#hrs_small <- dummy_cols(hrs_small, select_columns = 'r6hownrnt')



# Create dummies for low/high skill, blue/white collar workers for wave 7
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
# #better way
# elsa_w2$r2_high_white_collar <- ifelse(elsa_w2$r2jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# elsa_w2$r2_low_white_collar <- ifelse(elsa_w2$r2jcoccb %in% c(17, 18), 1, 0)
# elsa_w2$r2_high_blue_collar <- ifelse(elsa_w2$r2jcoccb %in% c(20, 21, 22, 23), 1, 0)
# elsa_w2$r2_low_blue_collar <- ifelse(elsa_w2$r2jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(elsa_w2$r2lbrf_e)

elsa_w2$r2_employed <- ifelse(elsa_w2$r2lbrf_e == 1 & 2, 1, 0)
elsa_w2 %>% tab(r2_employed)
elsa_w2$r2_retired <- ifelse(elsa_w2$r2lbrf_e %in% c(4, 5) , 1, 0)
elsa_w2 %>% tab(r2_retired)
elsa_w2$r2_unemployed <- ifelse(elsa_w2$r2lbrf_e == 3, 1, 0)
elsa_w2 %>% tab(r2_unemployed)



# Create dummies for low/high skill, blue/white collar workers for wave 11
unique(elsa_w8$r8jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
# #better way
# elsa_w8$r8_high_white_collar <- ifelse(elsa_w8$r8jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# elsa_w8$r8_low_white_collar <- ifelse(elsa_w8$r8jcoccb %in% c(17, 18), 1, 0)
# elsa_w8$r8_high_blue_collar <- ifelse(elsa_w8$r8jcoccb %in% c(20, 21, 22, 23), 1, 0)
# elsa_w8$r8_low_blue_collar <- ifelse(elsa_w8$r8jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(elsa_w8$r8lbrf_e)

elsa_w8$r8_employed <- ifelse(elsa_w8$r8lbrf_e == 1 & 2, 1, 0)
elsa_w8 %>% tab(r8_employed)
elsa_w8$r8_retired <- ifelse(elsa_w8$r8lbrf_e %in% c(4, 5) , 1, 0)
elsa_w8 %>% tab(r8_retired)
elsa_w8$r8_unemployed <- ifelse(elsa_w8$r8lbrf_e == 3, 1, 0)
elsa_w8 %>% tab(r8_unemployed)


unique(elsa_w2$r2drinkd)

#Drinking behaviours
#drink behaviors wave 7
elsa_w2$r2_drink_NO <- ifelse(elsa_w2$r2drinkd_e == 0, 1, 0) #does not drink
elsa_w2$r2_drink_OTS <- ifelse(elsa_w2$r2drinkd_e == 1 & 2 & 3, 1, 0) #drink one to several times a week
elsa_w2$r2_drink_MDW <- ifelse(elsa_w2$r2drinkd_e == 4 & 5 & 6, 1, 0) #drink most days of the week
elsa_w2$r2_drink_EDW <- ifelse(elsa_w2$r2drinkd_e == 7, 1, 0) #drink every day of the week


#drink behaviors wave  11
elsa_w8$r8_drink_NO <- ifelse(elsa_w8$r8drinkd_e == 0, 1, 0) #does not drink
elsa_w8$r8_drink_OTS <- ifelse(elsa_w8$r8drinkd_e == 1 & 2 & 3, 1, 0) #drink one to several times a week
elsa_w8$r8_drink_MDW <- ifelse(elsa_w8$r8drinkd_e == 4 & 5 & 6, 1, 0) #drink most days of the week
elsa_w8$r8_drink_EDW <- ifelse(elsa_w8$r8drinkd_e == 7, 1, 0) #drink every day of the week

# Create dummy variables for each category of ADLs for wave 1
elsa_w2$r2adlfive_0 <- ifelse(elsa_w2$r2adlfive == 0, 1, 0) # No impedings
elsa_w2$r2adlfive_1_2 <- ifelse(elsa_w2$r2adlfive == 1 | elsa_w2$r2adlfive == 2, 1, 0) # 1 or 2 impedings
elsa_w2$r2adlfive_3_plus <- ifelse(elsa_w2$r2adlfive >= 3, 1, 0) # 3+ impedings

# Create dummy variables for each category of ADLs for wave 6
elsa_w8$r8adlfive_0 <- ifelse(elsa_w8$r8adlfive == 0, 1, 0) # No impedings
elsa_w8$r8adlfive_1_2 <- ifelse(elsa_w8$r8adlfive == 1 | elsa_w8$r8adlfive == 2, 1, 0) # 1 or 2 impedings
elsa_w8$r8adlfive_3_plus <- ifelse(elsa_w8$r8adlfive >= 3, 1, 0) # 3+ impedings

#summary of IADLs for wave 1
elsa_w2$r2_iadl_0 <- ifelse(elsa_w2$r2shopa == 0 & elsa_w2$r2mealsa == 0
                           & elsa_w2$r2medsa == 0 & elsa_w2$r2moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
elsa_w2$r2_iadl_sum <- elsa_w2$r2shopa + elsa_w2$r2mealsa + elsa_w2$r2medsa + elsa_w2$r2moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
elsa_w2$r2_iadl_1_2 <- ifelse(elsa_w2$r2_iadl_sum == 1 | elsa_w2$r2_iadl_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
elsa_w2$r2_iadl_3_plus <- ifelse(elsa_w2$r2_iadl_sum >= 3, 1, 0)

#summary of IADLs for wave 6
elsa_w8$r8_iadl_0 <- ifelse(elsa_w8$r8shopa == 0 & elsa_w8$r8mealsa == 0
                             & elsa_w8$r8medsa == 0 & elsa_w8$r8moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
elsa_w8$r8_iadl_sum <- elsa_w8$r8shopa + elsa_w8$r8mealsa + elsa_w8$r8medsa + elsa_w8$r8moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
elsa_w8$r8_iadl_1_2 <- ifelse(elsa_w8$r8_iadl_sum >= 1 & elsa_w8$r8_iadl_sum <= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
elsa_w8$r8_iadl_3_plus <- ifelse(elsa_w8$r8_iadl_sum >= 3, 1, 0)


#Disease summary
elsa_w2$r2_healthy <- ifelse(elsa_w2$r2cancre == 0 & elsa_w2$r2hibpe == 0
                            & elsa_w2$r2diabe == 0 & elsa_w2$r2hearte ==0
                            & elsa_w2$r2lunge ==0 & elsa_w2$r2stroke ==0, 1, 0) # No impedings


#------------------------------------------------------------------------------------------
# Create a new variable that sums up the four existing dummy variables
elsa_w2$r2_dis_sum <- elsa_w2$r2cancre + elsa_w2$r2hearte + elsa_w2$r2hibpe + elsa_w2$r2diabe +
  elsa_w2$r2lunge + elsa_w2$r2stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
elsa_w2$r2_morbi_2 <- ifelse(elsa_w2$r2_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
elsa_w2$r2_morbi_3_plus <- ifelse(elsa_w2$r2_dis_sum >= 3, 1, 0)

#Disease summary
elsa_w8$r8_healthy <- ifelse(elsa_w8$r8cancre == 0 & elsa_w8$r8hibpe == 0
                              & elsa_w8$r8diabe == 0 & elsa_w8$r8hearte ==0
                              & elsa_w8$r8lunge ==0 & elsa_w8$r8stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
elsa_w8$r8_dis_sum <- elsa_w8$r8cancre + elsa_w8$r8hearte + elsa_w8$r8hibpe + elsa_w8$r8diabe +
  elsa_w8$r8lunge + elsa_w8$r8stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
elsa_w8$r8_morbi_2 <- ifelse(elsa_w8$r8_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
elsa_w8$r8_morbi_3_plus <- ifelse(elsa_w8$r8_dis_sum >= 3, 1, 0)

#----------------------------------------------------
# level of functioning in social and daily activities
#----------------------------------------------------
elsa_w2$r2_independent <- ifelse(elsa_w2$r2adlfive == 0 | elsa_w2$r2_iadl_sum == 0, 1, 0)
elsa_w8$r8_independent <- ifelse(elsa_w8$r8adlfive == 0 | elsa_w8$r8_iadl_sum == 0, 1, 0)

elsa_w2$r2_semifunctional <- ifelse(elsa_w2$r2adlfive == 1 | elsa_w2$r2_iadl_sum == 1 | elsa_w2$r2adlfive == 2 | elsa_w2$r2_iadl_sum == 2, 1, 0)
elsa_w8$r8_semifunctional <- ifelse(elsa_w8$r8adlfive == 1 | elsa_w8$r8_iadl_sum == 1 | elsa_w8$r8adlfive == 2 | elsa_w8$r8_iadl_sum == 2, 1, 0)

elsa_w2$r2_notfunctional <- ifelse(elsa_w2$r2adlfive >= 3 | elsa_w2$r2_iadl_sum >= 3, 1, 0)
elsa_w8$r8_notfunctional <- ifelse(elsa_w8$r8adlfive >= 3 | elsa_w8$r8_iadl_sum >= 3, 1, 0)

#-----------------------------------
# risky behaviors
#-----------------------------------
elsa_w2$r2_drink_EV <- ifelse(elsa_w2$r2drinkn_e >= 1, 1, 0)
elsa_w8$r8_drink_EV <- ifelse(elsa_w8$r8drinkwn_e >= 1, 1, 0)
# For wave 1
elsa_w2$r2_risky <- ifelse(elsa_w2$r2smokev >= 1 | elsa_w2$r2_drink_EV >= 1, 1, 0)

# For wave 6
elsa_w8$r8_risky <- ifelse(elsa_w8$r8smokev >= 1 | elsa_w8$r8_drink_EV >= 1, 1, 0)

# For wave 1
elsa_w2$r2_risky <- ifelse(is.na(elsa_w2$r2_risky), 0, elsa_w2$r2_risky)

# For wave 6
elsa_w8$r8_risky <- ifelse(is.na(elsa_w8$r8_risky), 0, elsa_w8$r8_risky)


#-----------------------------------
# physical activity behaviors
#-----------------------------------
elsa_w2$r2no_phys_act <- ifelse(elsa_w2$r2vgactx_e == 5 | elsa_w2$r2mdactx_e == 5, 1, 0)
elsa_w2 %>% tab(r2no_phys_act)
elsa_w2$r2low_phys_act <- ifelse(elsa_w2$r2vgactx_e == 4 | elsa_w2$r2mdactx_e == 4, 1, 0)
elsa_w2 %>% tab(r2low_phys_act)
elsa_w2$r2good_phys_act <- ifelse(elsa_w2$r2vgactx_e == 3 | elsa_w2$r2mdactx_e == 3 | elsa_w2$r2vgactx_e == 2 | elsa_w2$r2mdactx_e == 2 , 1, 0)
elsa_w2 %>% tab(r2good_phys_act)
elsa_w2$r2vgood_phys_act <- ifelse(elsa_w2$r2vgactx_e == 2 | elsa_w2$r2mdactx_e == 2, 1, 0)
elsa_w2 %>% tab(r2vgood_phys_act)

# For wave 6
elsa_w8$r8no_phys_act <- ifelse(elsa_w8$r8vgactx_e == 5 | elsa_w8$r8mdactx_e == 5, 1, 0)
elsa_w8$r8low_phys_act <- ifelse(elsa_w8$r8vgactx_e == 4 | elsa_w8$r8mdactx_e == 4, 1, 0)
elsa_w8$r8good_phys_act <- ifelse(elsa_w8$r8vgactx_e == 3 | elsa_w8$r8mdactx_e == 3 |elsa_w8$r8vgactx_e == 2 | elsa_w8$r8mdactx_e == 2 , 1, 0)
elsa_w8$r8vgood_phys_act <- ifelse(elsa_w8$r8vgactx_e == 2 | elsa_w8$r8mdactx_e == 2, 1, 0)
elsa_w8 %>% tab(r8no_phys_act)
elsa_w8 %>% tab(r8low_phys_act)
elsa_w8 %>% tab(r8good_phys_act)
elsa_w8 %>% tab(r8vgood_phys_act)

#-----------------------------------
#              family 
#-----------------------------------

elsa_w2$r2_single <- ifelse(elsa_w2$h2hhres == 1, 1, 0)
elsa_w2$r2_notsingle <- ifelse(elsa_w2$h2hhres >= 2, 1, 0)
elsa_w2 %>% tab(r2_single)
elsa_w2 %>% tab(r2_notsingle)

elsa_w8$r8_single <- ifelse(elsa_w8$h8hhres == 1, 1, 0)
elsa_w8$r8_notsingle <- ifelse(elsa_w8$h8hhres >= 2, 1, 0)
elsa_w8 %>% tab(r8_single)
elsa_w8 %>% tab(r8_notsingle)

#-----------------------------------
#              gender
#-----------------------------------
elsa_w2$man <- ifelse(elsa_w2$ragender == 1, 1, 0)
elsa_w2$woman <- ifelse(elsa_w2$ragender == 2, 1, 0)

elsa_w8$man <- ifelse(elsa_w8$ragender == 1, 1, 0)
elsa_w8$woman <- ifelse(elsa_w8$ragender == 2, 1, 0)


# write.csv(hrs_cross_section, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/hrs_cross_section.csv")
# 
# write_dta(hrs_cross_section, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_cross_section.dta")
# write.csv(elsa_w2, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/elsa_w2.csv")
write_dta(elsa_w2, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/elsa_w2.dta")
# write.csv(elsa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/elsa_w8.csv")
write_dta(elsa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/elsa_w8.dta")

write_dta(elsa_w2, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/elsa_w2.dta")
# write.csv(elsa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/elsa_w8.csv")
write_dta(elsa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/elsa_w8.dta")


#write_dta(elsa_w2, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/elsa_w2.dta")
#write_dta(elsa_w8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/elsa_w8.dta")



#--dataset divided by gender
male_elsaw2 <- elsa_w2 %>%
  filter(man==1)
count(male_elsaw2)

female_elsaw2 <- elsa_w2 %>%
  filter(woman==1)
count(female_elsaw2)

male_elsaw8 <- elsa_w8 %>%
  filter(man==1)
count(male_elsaw8)

female_elsaw8 <- elsa_w8 %>%
  filter(woman==1)
count(female_elsaw8)

write_dta(male_elsaw2, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/male_elsaw2.dta")
write_dta(female_elsaw2, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/female_elsaw2.dta")

write_dta(male_elsaw8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/male_elsaw8.dta")
write_dta(female_elsaw8, "C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/output/female_elsaw8.dta")

