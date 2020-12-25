set.seed(14)
median_clt <- rnorm(1000, mean = 10, sd = 2)

many_sample_medians <- function(vec, n, reps) {
  rep_vec <- replicate(reps, sample(vec, n), simplify = "vector")
  median_vec <- apply(rep_vec, 2, median)
  return(median_vec)
}

median_clt_test <- many_sample_medians(median_clt, 500, 1000)

median_clt_test_df <- data.frame(median_clt_test)
bw_clt <- 2 * IQR(median_clt_test_df$median_clt_test) / length(median_clt_test_df$median_clt_test)^(1/3)


ggplot(median_clt_test_df, aes(x = median_clt_test)) + 
  geom_histogram(binwidth = bw_clt, aes(y = ..density..),
                 fill = "hotpink1", col = "white") + 
  stat_function(fun = ~dnorm(.x, mean = 10, sd = 2),
                col = "darkorchid1", 
                lwd = 2) +
  xlim(c(5, 15)) +
  coord_cartesian(xlim = c(5, 15), ylim = c(0, 1)) +
  theme_classic()

ggplot(median_clt_test_df, aes(x = median_clt_test)) + 
  geom_histogram(binwidth = bw_clt, aes(y = ..density..),
                 fill = "hotpink1", col = "white") + 
  stat_function(fun = ~dnorm(.x, 
                             mean = mean(median_clt_test), 
                             sd = sd(median_clt_test)),
                col = "darkorchid1", 
                lwd = 2)
theme_classic()


xmean <- mean(DHS_Plot$base_and_all_options_value, na.rm = TRUE)
xsd <- sd(DHS_Plot$base_and_all_options_value, na.rm = TRUE)

p5 <- ggplot(DHS_Plot, aes(x = base_and_all_options_value)) + geom_histogram(aes(y = ..density..), binwidth = 0.2, color="black", fill = "blue" ) +
  stat_function(fun = ~dnorm(.x, mean = mean(DHS_Plot$base_and_all_options_value, na.rm = TRUE),
      sd = sd(DHS_Plot$base_and_all_options_value, na.rm = TRUE)), color = "green", lwd=2) +  
  xlab("Award Value") +
  ylab("Density") +  ggtitle("DHS Award Distribution 2009-2019") + 
  geom_density(aes(y=..density..), col="red", size=1.5)  +
  scale_x_continuous(trans = "log10") 





