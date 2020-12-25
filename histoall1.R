library(dplyr)
library(scales) # to access break formatting functions
library(ggplot2)
library(psych)

setwd("/home/kamalm/homework-0")

agencyabb <- c("DHS","DOC","DOD","DOJ","GSA","HHS","SEC","TREAS","USDA","VA")

scientific_10 <- function(x) {
  parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(10^x)))
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


for (val in agencyabb) {
  filename <- paste("data/",val,"_forhistogram.csv", sep = "")
  
   awarddata <- read.csv(file=filename, sep=",", colClasses=c("NULL", "NULL", NA, "NULL", "NULL", "NULL"))
   filtaward <- filter(awarddata, awarddata$base_and_exercised_options_value > 0)
   print(val)
   r <- describe(awarddata$base_and_exercised_options_value)
   print(r)
   r <- getmode(awarddata$base_and_exercised_options_value)
   print(r)
   
    mean1 <- mean(log10(filtaward$base_and_exercised_options_value))
   sd1 <- sd(log10(filtaward$base_and_exercised_options_value))
   
   awardvaluelog <- log10(filtaward$base_and_exercised_options_value)
   
   plotdata <- ggplot(filtaward, aes(x = awardvaluelog)) + 
     geom_histogram(aes(y = ..density..), binwidth = 0.5, color="black", fill = "steelblue" ) +
     stat_function(fun = ~dnorm(.x, mean = mean1, sd = sd1), color = "green", lwd=2) +  
     xlab(paste(val,"Award Value")) + ylab("Density") +  ggtitle(paste(val,"Award Distribution 2009-2019")) + 
     geom_density(aes(y=..density..), col="red", lwd=2) + scale_x_continuous(labels=scientific_10) 
  
   ggsave(paste("figs/",val,"_histogram.jpg", sep = ""), device="jpg", dpi=300, width = 5, height = 4, units = "in")
}

