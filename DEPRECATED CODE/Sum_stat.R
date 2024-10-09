remove(list=ls())
# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(tabulator)
library(stargazer)
#MAC
setwd("/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA")
#WIN HOME
#setwd("C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")
#SERVER
#setwd("C:/Users/stud1/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")
#read datasets
hrs <- read_dta("HRS/output/hrs_small.dta")
share <- read_dta("SHARE/output/share_small.dta")
share_w1 <-read_dta("SHARE/output/share_w1.dta")
share_w6 <- read_dta("SHARE/output/share_w6.dta")
hrs_w7 <- read_dta("HRS/output/hrs_w7.dta")
hrs_w11 <- read_dta("HRS/output/hrs_w11.dta")
#---Age---
age_vars <- data.frame(hrs[, c("r7agey_e", "r11agey_e")])
stargazer(age_vars, title = "Age Summary Stats.")
age_vars <- data.frame(share[, c("r1agey", "r6agey")])
summary(age_vars)
stargazer(age_vars, title = "Age Summary Stats.")

#---Income---
#HRS
income_vars <- data.frame(hrs[,c("r7iearn_constant", "r11iearn_constant")])
stargazer(income_vars, title = "Income Summary Stats.")
#SHARE
income_vars <- data.frame(share[,c("r1iearn_constant", "r6itearn_constant")])
stargazer(income_vars, title = "Income Summary Stats.")

#---Health---
#HRS
health_vars <- data.frame(hrs[, c( "r7bmi", "r11bmi","r7gripsum", "r11gripsum")])
stargazer(health_vars, title = "Health Summary Stats.")
#SHARE
health_vars <- data.frame(share[, c( "r1bmi", "r6bmi","r1gripsum", "r6gripsum")])
stargazer(health_vars, title = "Health Summary Stats.")

#---Labor---
#HRS
df <- data.frame(
  Category = c(rep(c("High White Collar", "Low White Collar", "High Blue Collar", "Low Blue Collar"), 2)),
  Proportion = c(hrs$r7_high_white_collar, hrs$r7_low_white_collar, hrs$r7_high_blue_collar, hrs$r7_low_blue_collar,
                 hrs$r11_high_white_collar, hrs$r11_low_white_collar, hrs$r11_high_blue_collar, hrs$r11_low_blue_collar),
  Type = rep(c("2004", "2014"), each = 4)
)

# Convert to long format
hrs_long <- reshape2::melt(df, id.vars = c("Category", "Type"))
palette <- RColorBrewer::brewer.pal(4, "Greens")
# Create the percent stacked bar plot with the new color palette
hr <- ggplot(hrs_long, aes(x = Type, y = value, fill = Category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage", x = "Employment Type", fill = "Category") +
  theme_minimal()
hr
# Save the plot
ggsave(filename = "HRS_empl.png", plot = hr, path = "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")

#SHARE
df <- data.frame(
  Category = c(rep(c("High White Collar", "Low White Collar", "High Blue Collar", "Low Blue Collar"), 2)),
  Proportion = c(share$r1_high_white_collar, share$r1_low_white_collar, share$r1_high_blue_collar, share$r1_low_blue_collar,
                 share$r6_high_white_collar, share$r6_low_white_collar, share$r6_high_blue_collar, share$r6_low_blue_collar),
  Type = rep(c("2004", "2014"), each = 4)
)

# Convert to long format
share_long <- reshape2::melt(df, id.vars = c("Category", "Type"))
palette <- RColorBrewer::brewer.pal(4, "Greens")
# Create the percent stacked bar plot with the new color palette
sh <- ggplot(share_long, aes(x = Type, y = value, fill = Category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage", x = "Employment Type", fill = "Category") +
  theme_minimal()
sh
# Save the plot
ggsave(filename = "SHARE_empl.png", plot = sh, path = "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")



#------------------------------------------------------------------------------
#                           Cross section variants 
#------------------------------------------------------------------------------
#---Age---
age_vars <- data.frame(hrs_w7$r7agey_e)
stargazer(age_vars, title = "Age Summary Stats.")
age_vars <- data.frame(hrs_w11$r11agey_e)
stargazer(age_vars, title = "Age Summary Stats.")
age_vars <- data.frame(share_w1$r1agey)
stargazer(age_vars, title = "Age Summary Stats.")
age_vars <- data.frame(share_w6$r6agey)
stargazer(age_vars, title = "Age Summary Stats.")
#---Income---
#HRS
income_vars <- data.frame(hrs_w7$r7iearn_constant)
stargazer(income_vars, title = "Income Summary Stats.")
income_vars <- data.frame(hrs_w11$r11iearn_constant)
stargazer(income_vars, title = "Income Summary Stats.")
#SHARE
income_vars <- data.frame(share_w1$r1iearn_constant)
stargazer(income_vars, title = "Income Summary Stats.")
income_vars <- data.frame(share_w6$r6itearn_constant)
stargazer(income_vars, title = "Income Summary Stats.")

#---Health---
#HRS
health_vars <- data.frame(hrs_w7$r7bmi)
stargazer(health_vars, title = "Health Summary Stats.")
health_vars <- data.frame(hrs_w11$r11bmi)
stargazer(health_vars, title = "Health Summary Stats.")
health_vars <- data.frame(hrs_w7$r7gripsum)
stargazer(health_vars, title = "Health Summary Stats.")
health_vars <- data.frame(hrs_w11$r11gripsum)
stargazer(health_vars, title = "Health Summary Stats.")
#SHARE
health_vars <- data.frame(share_w1$r1bmi)
stargazer(health_vars, title = "Health Summary Stats.")
health_vars <- data.frame(share_w6$r6bmi)
stargazer(health_vars, title = "Health Summary Stats.")
health_vars <- data.frame(share_w1$r1gripsum)
stargazer(health_vars, title = "Health Summary Stats.")
health_vars <- data.frame(share_w6$r6gripsum)
stargazer(health_vars, title = "Health Summary Stats.")

#---Labor---
#HRS
df <- data.frame(
  Category = c(rep(c("High White Collar", "Low White Collar", "High Blue Collar", "Low Blue Collar"), 2)),
  Proportion = c(cross_hrs$r7_high_white_collar, cross_hrs$r7_low_white_collar, cross_hrs$r7_high_blue_collar, cross_hrs$r7_low_blue_collar,
                 cross_hrs$r11_high_white_collar, cross_hrs$r11_low_white_collar, cross_hrs$r11_high_blue_collar, cross_hrs$r11_low_blue_collar),
  Type = rep(c("2004", "2014"), each = 4)
)

# Convert to long format
cross_hrs_long <- reshape2::melt(df, id.vars = c("Category", "Type"))
palette <- RColorBrewer::brewer.pal(4, "Greens")
# Create the percent stacked bar plot with the new color palette
hr <- ggplot(cross_hrs_long, aes(x = Type, y = value, fill = Category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage", x = "Employment Type", fill = "Category") +
  theme_minimal()
hr
# Save the plot
ggsave(filename = "HRScross_empl.png", plot = hr, path = "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")

#SHARE
df <- data.frame(
  Category = c(rep(c("High White Collar", "Low White Collar", "High Blue Collar", "Low Blue Collar"), 2)),
  Proportion = c(cross_share$r1_high_white_collar, cross_share$r1_low_white_collar, cross_share$r1_high_blue_collar, cross_share$r1_low_blue_collar,
                 cross_share$r6_high_white_collar, cross_share$r6_low_white_collar, cross_share$r6_high_blue_collar, cross_share$r6_low_blue_collar),
  Type = rep(c("2004", "2014"), each = 4)
)

# Convert to long format
cross_share_long <- reshape2::melt(df, id.vars = c("Category", "Type"))
palette <- RColorBrewer::brewer.pal(4, "Greens")
# Create the percent stacked bar plot with the new color palette
sh <- ggplot(cross_share_long, aes(x = Type, y = value, fill = Category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage", x = "Employment Type", fill = "Category") +
  theme_minimal()
sh
# Save the plot
ggsave(filename = "SHAREcross_empl.png", plot = sh, path = "C:/Users/Pietro/OneDrive - Universita' degli Studi di Roma Tor Vergata/RA")
