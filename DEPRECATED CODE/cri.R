remove(list = ls())
setwd("C:/Users/Pietro/Desktop/crici")
 library(scales)
data <- read.csv("Gn.csv", header = TRUE, sep = ";")

data <- data[-1,]
write.csv(data, "Gn2.csv", row.names = FALSE)

data <- read.csv("Gn2.csv", header = TRUE, sep = ";")

rn <-  apply(data, 1, rescale)