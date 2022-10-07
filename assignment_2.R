
#setwd("~/Desktop/2nd Year PhD/Econ 245/HW2/245HW2")

#rm(list=ls())

library(tidyverse)

#1 - a)
airbnb <- read_csv("assign_2.csv")

#1 - b) not graded using View() and colnames()

#1 - c)
airbnb <- airbnb %>%
  rename(neighborhood = neighbourhood)

#2 - a)
neighborhoods <- airbnb %>%
  count(neighborhood, sort = T)

#2 - b)
neighborhoods <- neighborhoods %>%
  filter(!is.na(neighborhood)) %>%
  arrange(desc(n)) %>%
  head(20)

#2 - c)
airbnb_top_neighborhoods <- airbnb %>%
  filter(neighborhood %in% neighborhoods$neighborhood)

#2 - d)
summary_stats_top_neighborhoods  <- airbnb_top_neighborhoods %>%
  group_by(neighborhood) %>%
  summarize(avg_square_feet = mean(square_feet, na.rm = T),
            avg_price = mean(price, na.rm = T),
            sd_price  = sd(price, na.rm = T),
            max_price = max(price, na.rm = T),
            min_price = min(price, na.rm = T)) %>%
  arrange(desc(avg_square_feet))

#2 - e)
highest_avg_square_ft <- summary_stats_top_neighborhoods %>%
  slice(1) %>%
  pull(avg_square_feet)

#2 - f)
second_avg_price <- summary_stats_top_neighborhoods %>%
  arrange(desc(avg_price)) %>%
  slice(2) %>%
  pull(avg_price)




