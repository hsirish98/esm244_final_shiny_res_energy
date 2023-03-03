library(tidyverse)

summary_stat <- readxl::read_xlsx(here("data", "2015_home_sum_stat.xlsx")) %>%
  janitor::clean_names() %>%
  select(-7:ncol(summary_stat))

sum_stat_short <- summary_stat[-c(2,18:nrow(summary_stat)),] 

colnames(sum_stat_short)[1] <- "census_region"
