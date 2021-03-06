library(dplyr)
library(scales) # to access break formatting functions
library(ggplot2)

setwd("/home/kamalm/homework-0")

agencyabb <- c("DHS","DOC","DOD","DOJ","GSA","HHS","SEC","TREAS","USDA","VA")


#DHS_1 <- read.csv(file="data/DHS_forhistogram.csv", sep=",", colClasses=c("NULL", "NULL", NA, "NULL", "NULL", "NULL"))

#DHS_1 <- read.csv(file="data/DOC_forhistogram.csv", sep=",", colClasses=c("NULL", "NULL", NA, "NULL", "NULL", "NULL"))

DHS_1 <- read.csv(file="data/DOD_forhistogram.csv", sep=",", colClasses=c("NULL", "NULL", NA, "NULL", "NULL", "NULL"))

DHSaward <- filter(DHS_1, DHS_1$base_and_exercised_options_value > 0)

DHSmean <- mean(DHSaward$base_and_exercised_options_value)
DHSsd <- sd(DHSaward$base_and_exercised_options_value)

DHSmean1 <- mean(log10(DHSaward$base_and_exercised_options_value))
DHSsd1 <- sd(log10(DHSaward$base_and_exercised_options_value))

p1 <- ggplot(DHSaward, aes(x = base_and_exercised_options_value)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.5, color="black", fill = "steelblue" ) +
  #stat_function(fun = ~dnorm(.x, mean = DHSmean, sd = DHSsd), color = "green", lwd=2) +  
  xlab("Award Value") + ylab("Density") +  ggtitle("Award Distribution 2009-2019") + 
  geom_density(aes(y=..density..), col="red", lwd=2)  +
  # scale_x_continuous(trans = "log10") #+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
p1

scientific_10 <- function(x) {
  parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(10^x)))
}


awardvaluelog <- log10(DHSaward$base_and_exercised_options_value)
p2 <- ggplot(DHSaward, aes(x = awardvaluelog)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.5, color="black", fill = "steelblue" ) +
  stat_function(fun = ~dnorm(.x, mean = DHSmean1, sd = DHSsd1), color = "green", lwd=2) +  
  xlab("Award Value") + ylab("Density") +  ggtitle("Award Distribution 2009-2019") + 
  geom_density(aes(y=..density..), col="red", lwd=2) + scale_x_continuous(labels=scientific_10) 
p2




#===================
x <- 1:4
y <- c(0, 0.0001, 0.0002, 0.0003)

dd <- data.frame(x, y)

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

ggplot(dd, aes(x, y)) + geom_point()+scale_y_continuous(label=scientific_10)
#====================


