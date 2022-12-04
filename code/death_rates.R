library(tidyverse)
library(magrittr)
library(lubridate)

source("code/make_death_rates.R")

pop_quarterly_10 <- c19dk::get_pop_by_breaks(data = "data/tidy_DST_pop.csv", age_breaks = c19dk::get_age_breaks(maxage = 90, agesplit = 10))

deaths_10 <- read_csv2("data/tidy_DST_daily_deaths_age_sex_10.csv")

make_death_rates(deaths_10, pop_quarterly_10, Age, Sex) %>% 
  write_csv("data/death_rates_10y_age_sex.csv")

make_death_rates(deaths_10, pop_quarterly_10, Age) %>% 
  write_csv("data/death_rates_10y_age.csv")
  
make_death_rates(deaths_10, pop_quarterly_10) %>% 
  write_csv("data/death_rates_no_strata.csv")

pop_quarterly_5 <- c19dk::get_pop_by_breaks(data = "data/tidy_DST_pop.csv", age_breaks = c19dk::get_age_breaks(maxage = 95, agesplit = 5))

deaths_5 <- read_csv2("data/tidy_DST_daily_deaths_age_sex_5.csv") %>% 
  # grouping 95+ into one group
  mutate(Age = case_when(
    Age == "95-99" ~ "95+",
    Age == "100+" ~ "95+",
    TRUE ~ Age
  )) %>% 
  group_by(Date, Age, Sex) %>% 
  summarize(Deaths = sum(Deaths))

deaths_5$Age <- factor(deaths_5$Age, levels = c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
  "80-84", "85-89", "90-94", "95+"
))


make_death_rates(deaths_5, pop_quarterly_5, Age, Sex) %>% 
  write_csv("data/death_rates_5y_age_sex.csv")

make_death_rates(deaths_5, pop_quarterly_5, Age) %>% 
  write_csv("data/death_rates_5y_age.csv")


