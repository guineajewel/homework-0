library("tidyverse")
library("dslabs")
data(heights)

heights %>% ggplot(aes(heights, fill = sex)) +
  geom_density(alpha = 0.05)