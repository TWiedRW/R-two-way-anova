## ---------------------------
##
## Script name: create_data.R
##
## Purpose of script: Creates data for workshop
##
## Author: Tyler Wiederich
##
## Date Created: 2025-03-13
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

library(tidyverse)
set.seed(2026)

# ---- Example 1 ----

fertilizer <- c('Low', 'High')
brand <- c('Fast-Grow', 'Easy-Plant')
df_agriculture <- expand_grid(fertilizer = fertilizer, brand = brand)
df_agriculture$height_mean <-  c(20, 30, 40, 25)

agriculture_data <- expand_grid(df_agriculture, rep = 1:4) %>%
  mutate(height = map_dbl(height_mean, \(x)(rnorm(1, mean = x, sd = 2)))) %>%
  select(fertilizer, brand, rep, height)

with(agriculture_data, interaction.plot(fertilizer, brand, height))
with(agriculture_data, interaction.plot(brand, fertilizer, height))

write_csv(agriculture_data, 'data/example1-agriculture.csv')

# ---- Example 2 ----

ph <- c(6, 7, 8)
salt <- c(15, 20, 25)

bacteria <- expand_grid(ph, salt) %>%
  mutate(logcfu_mean = c(2, 2.1, 2.2,   2.2, 2.4, 2.5,    2.2, 2.2, 2.4)) %>%
  expand_grid(rep = 1:4) %>%
  mutate(logcfu = map_dbl(logcfu_mean, \(x)(rnorm(1, mean = x, sd = 0.2)))) %>%
  select(ph, salt, rep, logcfu)

with(bacteria, interaction.plot(ph, salt, logcfu))
with(bacteria, interaction.plot(salt, ph, logcfu))

write_csv(bacteria, 'data/example2-bacteria.csv')
