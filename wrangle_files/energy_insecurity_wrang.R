library(tidyverse)
library(waffle)
library(extrafont)

house_char <- readxl::read_xlsx(here::here("data","State_Household_Characteristics.xlsx"))
insecurity <- readxl::read_xlsx(here::here("data", "insecurity.xlsx"))

energy_ins <- house_char %>%
  select(1,10) %>%
  drop_na() 

colnames(energy_ins) <- c("name", "pct_insecure")

energy_ins <- energy_ins %>%
  filter(!(name %in% c("All homes", "District of Columbia")))


