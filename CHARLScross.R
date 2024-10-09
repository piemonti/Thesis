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
#setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS")
#load("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/MEMHRS_definitive.RData")
#SERVER
setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS")
#load("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/ELSA/UKDA-5050-stata/MEMELSA.RData")
#MAC
#setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/HRS")
#load("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA/HRS/MEMHRS_definitive.RData")
#-------FORMATTING CPI DATA-------
charlsdata <- read_dta("output/H_CHARLS.dta")
currency_pair <- "CNY/USD"
start_date <- "2018-01-01"
end_date <- "2018-12-31"

# Get the exchange rate data
# Note: Yahoo Finance may not support CNY/USD directly; you may need to find a supported symbol
getSymbols("CNYUSD=X", src="yahoo", from=start_date, to=end_date, auto.assign=TRUE)

# Extract the closing prices
exchange_rates <- Cl(`CNYUSD=X`)

# Calculate the mean exchange rate for the year
mean_exchange_rate <- mean(exchange_rates, na.rm=TRUE)
print(mean_exchange_rate)

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
charls_w1 <- charlsdata %>%
  filter(r1iwstat == 1)
charls_w4 <- charlsdata %>%
  filter(r4iwstat == 4 | r4iwstat == 4)
charls_w1 <- charlsdata %>%
  select( ID, r1cancre, r1diabe, r1hearte, r1hibpe, r1lunge, r1stroke, r1mbmi,
         r1gripsum,r1agey, r1vgactx_c,
         r1mdactx_c, r1smokev,r1lbrf_c, r1smoken,r1drinkn_c,
         raeducl, r1slfemp, r1adlfive,
         r1shopa, r1mealsa, r1medsa, r1moneya, hh1itot, ragender, h1hhres, ragender, r1wtresp
  ) %>%
  filter(r1agey >= 50)

charls_w4 <- charlsdata %>%
  select( ID, r4cancre, r4diabe, r4hearte, r4hibpe, r4lunge, r4stroke,
           r4agey, r4vgactx_c,
           r4mdactx_c, r4smokev,r4lbrf_c, r4smoken,r4drinkn_c,
           raeducl, r4slfemp, r4adlfive,
           r4shopa, r4mealsa, r4medsa, r4moneya, hh4itot, ragender, h4hhres, ragender, r4wtresp
  ) %>%
  filter(r4agey >= 50)

# Convert income for 2002 to constant prices
charls_w1$h1itot_constant <- (charls_w1$hh1itot / cpi_data$c2010cpindex) * 100
# Convert income for 2012 to constant prices
charls_w4$h4itot_constant <- (charls_w4$hh4itot / cpi_data$c2010cpindex) * 100

# For charls_w1
charls_w1 <- charls_w1 %>% filter(h1itot_constant >= 0)

# For charls_w4
charls_w4 <- charls_w4 %>% filter(h4itot_constant >= 0)

charls_w1$h1itot_constant <- charls_w1$h1itot_constant * 0.148
charls_w4$h4itot_constant <- charls_w4$h4itot_constant * 0.151

# Create dummy variables for education
charls_w1 <- dummy_cols(charls_w1, select_columns = 'raeducl')

charls_w1 <- rename(charls_w1,low_educ = raeducl_1 , secondary_educ = raeducl_2,
                 tertiary_educ = raeducl_3)
# Create dummy variables for education
charls_w4 <- dummy_cols(charls_w4, select_columns = 'raeducl')

charls_w4 <- rename(charls_w4,low_educ = raeducl_1 , secondary_educ = raeducl_2,
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
# charls_w1$r1_high_white_collar <- ifelse(charls_w1$r1jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# charls_w1$r1_low_white_collar <- ifelse(charls_w1$r1jcoccb %in% c(17, 18), 1, 0)
# charls_w1$r1_high_blue_collar <- ifelse(charls_w1$r1jcoccb %in% c(20, 21, 22, 23), 1, 0)
# charls_w1$r1_low_blue_collar <- ifelse(charls_w1$r1jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(charls_w1$r1lbrf_c)

charls_w1$r1_employed <- ifelse(charls_w1$r1lbrf_c %in% c(1, 2, 3, 4), 1, 0)
charls_w1 %>% tab(r1_employed)
charls_w1$r1_retired <- ifelse(charls_w1$r1lbrf_c == 7, 1, 0)
charls_w1 %>% tab(r1_retired)
charls_w1$r1_unemployed <- ifelse(charls_w1$r1lbrf_c == 6, 1, 0)
charls_w1 %>% tab(r1_unemployed)



# Create dummies for low/high skill, blue/white collar workers for wave 11
unique(charls_w4$r4jcoccb)
# Define the groups
#low_blue_collar <- c("14.food prep+serving occs", "15.blding/grounds/clean/maint occups", "16.personal care+service occups", "19.farm/fish/forestry occups", "24.transport/material moving occups")
#high_blue_collar <- c("20.construction trades", "21.extraction workers", "22.install/maint/repair workrs", "23.production occups")
#low_white_collar <- c("17.sales occups", "18.office+admin support occups")
#high_white_collar <- c("01.management occupations", "02.business oper-ns specialists", "03.financial specialists", "04.computer+math occs", "05.architecture+engineering occs", "06.life/physical/socialsci occs", "07.community+social svcs occs", "08.legal occups", "09.education/training/library occs", "10.arts/design/entertnmt occs", "11.hlthcare practition/tech occs", "12.healthcare support occs", "13.protective service occs")
# #better way
# charls_w4$r4_high_white_collar <- ifelse(charls_w4$r4jcoccb %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1, 0)
# charls_w4$r4_low_white_collar <- ifelse(charls_w4$r4jcoccb %in% c(17, 18), 1, 0)
# charls_w4$r4_high_blue_collar <- ifelse(charls_w4$r4jcoccb %in% c(20, 21, 22, 23), 1, 0)
# charls_w4$r4_low_blue_collar <- ifelse(charls_w4$r4jcoccb %in% c(14, 15, 16, 19, 24), 1, 0)

unique(charls_w4$r4lbrf_c)

charls_w4$r4_employed <- ifelse(charls_w4$r4lbrf_c %in% c(1, 2, 3, 4), 1, 0)
charls_w4 %>% tab(r4_employed)
charls_w4$r4_retired <- ifelse(charls_w4$r4lbrf_c == 7, 1, 0)
charls_w4 %>% tab(r4_retired)
charls_w4$r4_unemployed <- ifelse(charls_w4$r4lbrf_c == 6, 1, 0)
charls_w4 %>% tab(r4_unemployed)


unique(charls_w1$r1drinkd)

#Drinking behaviours
#drink behaviors wave 7
charls_w1$r1_drink_NO <- ifelse(charls_w1$r1drinkn_c == 0, 1, 0) #does not drink
charls_w1$r1_drink_OTS <- ifelse(charls_w1$r1drinkn_c == 1 & 2 & 3, 1, 0) #drink one to several times a month
charls_w1$r1_drink_MDW <- ifelse(charls_w1$r1drinkn_c == 4 & 5 & 6, 1, 0) #drink most days of the week
charls_w1$r1_drink_EDW <- ifelse(charls_w1$r1drinkn_c == 7 & 8 & 9, 1, 0) #drink every day of the week


#drink behaviors wave  11
charls_w4$r4_drink_NO <- ifelse(charls_w4$r4drinkn_c == 0, 1, 0) #does not drink
charls_w4$r4_drink_OTS <- ifelse(charls_w4$r4drinkn_c == 1 & 2 & 3, 1, 0) #drink one to several times a month
charls_w4$r4_drink_MDW <- ifelse(charls_w4$r4drinkn_c == 4 & 5 & 6, 1, 0) #drink most days of the week
charls_w4$r4_drink_EDW <- ifelse(charls_w4$r4drinkn_c == 7 & 8 & 9, 1, 0) #drink every day of the week

# Create dummy variables for each category of ADLs for wave 1
charls_w1$r1adlfive_0 <- ifelse(charls_w1$r1adlfive == 0, 1, 0) # No impedings
charls_w1$r1adlfive_1_2 <- ifelse(charls_w1$r1adlfive == 1 | charls_w1$r1adlfive == 2, 1, 0) # 1 or 2 impedings
charls_w1$r1adlfive_3_plus <- ifelse(charls_w1$r1adlfive >= 3, 1, 0) # 3+ impedings

# Create dummy variables for each category of ADLs for wave 6
charls_w4$r4adlfive_0 <- ifelse(charls_w4$r4adlfive == 0, 1, 0) # No impedings
charls_w4$r4adlfive_1_2 <- ifelse(charls_w4$r4adlfive == 1 | charls_w4$r4adlfive == 2, 1, 0) # 1 or 2 impedings
charls_w4$r4adlfive_3_plus <- ifelse(charls_w4$r4adlfive >= 3, 1, 0) # 3+ impedings

#summary of IADLs for wave 1
charls_w1$r1_iadl_0 <- ifelse(charls_w1$r1shopa == 0 & charls_w1$r1mealsa == 0
                           & charls_w1$r1medsa == 0 & charls_w1$r1moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
charls_w1$r1_iadl_sum <- charls_w1$r1shopa + charls_w1$r1mealsa + charls_w1$r1medsa + charls_w1$r1moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
charls_w1$r1_iadl_1_2 <- ifelse(charls_w1$r1_iadl_sum == 1 | charls_w1$r1_iadl_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
charls_w1$r1_iadl_3_plus <- ifelse(charls_w1$r1_iadl_sum >= 3, 1, 0)

#summary of IADLs for wave 6
charls_w4$r4_iadl_0 <- ifelse(charls_w4$r4shopa == 0 & charls_w4$r4mealsa == 0
                             & charls_w4$r4medsa == 0 & charls_w4$r4moneya ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
charls_w4$r4_iadl_sum <- charls_w4$r4shopa + charls_w4$r4mealsa + charls_w4$r4medsa + charls_w4$r4moneya

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
charls_w4$r4_iadl_1_2 <- ifelse(charls_w4$r4_iadl_sum >= 1 & charls_w4$r4_iadl_sum <= 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
charls_w4$r4_iadl_3_plus <- ifelse(charls_w4$r4_iadl_sum >= 3, 1, 0)


#Disease summary
charls_w1$r1_healthy <- ifelse(charls_w1$r1cancre == 0 & charls_w1$r1hibpe == 0
                            & charls_w1$r1diabe == 0 & charls_w1$r1hearte ==0
                            & charls_w1$r1lunge ==0 & charls_w1$r1stroke ==0, 1, 0) # No impedings


#------------------------------------------------------------------------------------------
# Create a new variable that sums up the four existing dummy variables
charls_w1$r1_dis_sum <- charls_w1$r1cancre + charls_w1$r1hearte + charls_w1$r1hibpe + charls_w1$r1diabe +
  charls_w1$r1lunge + charls_w1$r1stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
charls_w1$r1_morbi_2 <- ifelse(charls_w1$r1_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
charls_w1$r1_morbi_3_plus <- ifelse(charls_w1$r1_dis_sum >= 3, 1, 0)

#Disease summary
charls_w4$r4_healthy <- ifelse(charls_w4$r4cancre == 0 & charls_w4$r4hibpe == 0
                              & charls_w4$r4diabe == 0 & charls_w4$r4hearte ==0
                              & charls_w4$r4lunge ==0 & charls_w4$r4stroke ==0, 1, 0) # No impedings
# Create a new variable that sums up the four existing dummy variables
charls_w4$r4_dis_sum <- charls_w4$r4cancre + charls_w4$r4hearte + charls_w4$r4hibpe + charls_w4$r4diabe +
  charls_w4$r4lunge + charls_w4$r4stroke

# Create a new dummy variable that indicates whether an individual has either 1 or 2 impedings
charls_w4$r4_morbi_2 <- ifelse(charls_w4$r4_dis_sum == 2, 1, 0)

# Create a new dummy variable that indicates whether an individual has either three or more impedings
charls_w4$r4_morbi_3_plus <- ifelse(charls_w4$r4_dis_sum >= 3, 1, 0)

#----------------------------------------------------
# level of functioning in social and daily activities
#----------------------------------------------------
charls_w1$r1_independent <- ifelse(charls_w1$r1adlfive == 0 | charls_w1$r1_iadl_sum == 0, 1, 0)
charls_w4$r4_independent <- ifelse(charls_w4$r4adlfive == 0 | charls_w4$r4_iadl_sum == 0, 1, 0)

charls_w1$r1_semifunctional <- ifelse(charls_w1$r1adlfive == 1 | charls_w1$r1_iadl_sum == 1 | charls_w1$r1adlfive == 2 | charls_w1$r1_iadl_sum == 2, 1, 0)
charls_w4$r4_semifunctional <- ifelse(charls_w4$r4adlfive == 1 | charls_w4$r4_iadl_sum == 1 | charls_w4$r4adlfive == 2 | charls_w4$r4_iadl_sum == 2, 1, 0)

charls_w1$r1_notfunctional <- ifelse(charls_w1$r1adlfive >= 3 | charls_w1$r1_iadl_sum >= 3, 1, 0)
charls_w4$r4_notfunctional <- ifelse(charls_w4$r4adlfive >= 3 | charls_w4$r4_iadl_sum >= 3, 1, 0)

#-----------------------------------
# risky behaviors
#-----------------------------------
charls_w1$r1_drink_EV <- ifelse(charls_w1$r1drinkn_c >= 1, 1, 0)
charls_w4$r4_drink_EV <- ifelse(charls_w4$r4drinkn_c >= 1, 1, 0)
# For wave 1
charls_w1$r1_risky <- ifelse(charls_w1$r1smokev >= 1 | charls_w1$r1_drink_EV >= 1, 1, 0)

# For wave 6
charls_w4$r4_risky <- ifelse(charls_w4$r4smokev >= 1 | charls_w4$r4_drink_EV >= 1, 1, 0)

# For wave 1
charls_w1$r1_risky <- ifelse(is.na(charls_w1$r1_risky), 0, charls_w1$r1_risky)

# For wave 6
charls_w4$r4_risky <- ifelse(is.na(charls_w4$r4_risky), 0, charls_w4$r4_risky)


#-----------------------------------
# physical activity behaviors
#-----------------------------------
charls_w1$r1no_phys_act <- ifelse(charls_w1$r1vgactx_c == 5 | charls_w1$r1mdactx_c == 5, 1, 0)
charls_w1 %>% tab(r1no_phys_act)
charls_w1$r1low_phys_act <- ifelse(charls_w1$r1vgactx_c == 4 | charls_w1$r1mdactx_c == 4, 1, 0)
charls_w1 %>% tab(r1low_phys_act)
charls_w1$r1good_phys_act <- ifelse(charls_w1$r1vgactx_c == 3 | charls_w1$r1mdactx_c == 3 | charls_w1$r1vgactx_c == 2 | charls_w1$r1mdactx_c == 2 , 1, 0)
charls_w1 %>% tab(r1good_phys_act)
charls_w1$r1vgood_phys_act <- ifelse(charls_w1$r1vgactx_c == 2 | charls_w1$r1mdactx_c == 2, 1, 0)
charls_w1 %>% tab(r1vgood_phys_act)

# For wave 6
charls_w4$r4no_phys_act <- ifelse(charls_w4$r4vgactx_c == 5 | charls_w4$r4mdactx_c == 5, 1, 0)
charls_w4$r4low_phys_act <- ifelse(charls_w4$r4vgactx_c == 4 | charls_w4$r4mdactx_c == 4, 1, 0)
charls_w4$r4good_phys_act <- ifelse(charls_w4$r4vgactx_c == 3 | charls_w4$r4mdactx_c == 3 |charls_w4$r4vgactx_c == 2 | charls_w4$r4mdactx_c == 2 , 1, 0)
charls_w4$r4vgood_phys_act <- ifelse(charls_w4$r4vgactx_c == 2 | charls_w4$r4mdactx_c == 2, 1, 0)
charls_w4 %>% tab(r4no_phys_act)
charls_w4 %>% tab(r4low_phys_act)
charls_w4 %>% tab(r4good_phys_act)
charls_w4 %>% tab(r4vgood_phys_act)

#-----------------------------------
#              family 
#-----------------------------------

charls_w1$r1_single <- ifelse(charls_w1$h1hhres == 1, 1, 0)
charls_w1$r1_notsingle <- ifelse(charls_w1$h1hhres >= 2, 1, 0)
charls_w1 %>% tab(r1_single)
charls_w1 %>% tab(r1_notsingle)

charls_w4$r4_single <- ifelse(charls_w4$h4hhres == 1, 1, 0)
charls_w4$r4_notsingle <- ifelse(charls_w4$h4hhres >= 2, 1, 0)
charls_w4 %>% tab(r4_single)
charls_w4 %>% tab(r4_notsingle)

#-----------------------------------
#              gender
#-----------------------------------
charls_w1$man <- ifelse(charls_w1$ragender == 1, 1, 0)
charls_w1$woman <- ifelse(charls_w1$ragender == 2, 1, 0)

charls_w4$man <- ifelse(charls_w4$ragender == 1, 1, 0)
charls_w4$woman <- ifelse(charls_w4$ragender == 2, 1, 0)


# write.csv(hrs_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/hrs_cross_section.csv")
# 
# write_dta(hrs_cross_section, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/hrs_cross_section.dta")
# write.csv(charls_w1, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/charls_w1.csv")
write_dta(charls_w1, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS/output/charls_w1.dta")
# write.csv(charls_w4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/charls_w4.csv")
write_dta(charls_w4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS/output/charls_w4.dta")
# 

#write_dta(charls_w1, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/charls_w1.dta")
#write_dta(charls_w4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/HRS/output/charls_w4.dta")


#--dataset divided by gender
male_charlsw1 <- charls_w1 %>%
  filter(man==1)
count(male_charlsw1)

female_charlsw1 <- charls_w1 %>%
  filter(woman==1)
count(female_charlsw1)

male_charlsw4 <- charls_w4 %>%
  filter(man==1)
count(male_charlsw4)

female_charlsw4 <- charls_w4 %>%
  filter(woman==1)
count(female_charlsw4)

write_dta(male_charlsw1, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS/output/male_charlsw1.dta")
write_dta(female_charlsw1, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS/output/female_charlsw1.dta")

write_dta(male_charlsw4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS/output/male_charlsw4.dta")
write_dta(female_charlsw4, "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA/CHARLS/output/female_charlsw4.dta")

