library("ggplot2")
p1 <- ggplot(DHS_Plot, aes(x = base_and_all_options_value)) + geom_histogram(aes(y = ..density..), binwidth = 0.2, color="black", fill = "blue" ) +
  stat_function(fun = dnorm, color = "green", args = list(mean = mean(DHS_Plot$base_and_all_options_value, na.rm = TRUE),
  sd = sd(DHS_Plot$base_and_all_options_value, na.rm = TRUE))) +  xlab("Award Value") +
  ylab("Density") +  ggtitle("DHS Award Distribution 2009-2019") + 
  geom_density(aes(y=..density..), col="red", size=1.5)  +
  scale_x_continuous(trans = "log10") 
p1


p2 <- ggplot(DHS_Plot, aes(x = base_and_all_options_value)) + geom_histogram(aes(y = ..density..), binwidth = 0.2, color="black", fill = "steelblue") +
   xlab("Award Value") + 
   ylab("Density") +  ggtitle("DHS Award Distribution 2009-2019") + geom_density(aes(y=..density..), col="red", size=1.5)  +
   scale_x_continuous(trans = "log10")
p2

p3 <- ggplot(DHS_Plot, aes(x = base_and_all_options_value)) + geom_histogram(aes(y = ..density..), binwidth = 0.085, color="black", fill = "blue" ) +
      stat_function(fun = dnorm, color = "red", args = list(mean = 0, sd = 1)) +  xlab("Award Value (log scale)") +
      ylab("Density") +  ggtitle("DHS Award Distribution 2009-2019") + scale_x_continuous(trans = "log10")
p3

awardval <- log10(DHS_Plot$base_and_all_options_value)
p4 <- ggplot(DHS_Plot, aes(x = awardval )) + geom_histogram(aes(y = ..density..), binwidth = 0.2, color="black", fill = "steelblue") 
p4 + xlab("Award Value - [10 to the power x-axis value]") + ylab("Density") +  
  ggtitle("DHS Award Distribution 2009-2019") + 
  stat_function(fun = dnorm, color = "green", args = list(mean = mean(awardval, na.rm = TRUE), sd = sd(awardval, na.rm = TRUE)), size=1.5) + 
  geom_density(aes(y=..density..), col="red", size=1.5)

