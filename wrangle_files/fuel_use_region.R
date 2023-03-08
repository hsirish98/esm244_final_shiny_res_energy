library(tidyverse)
library(here)


fuel_use <- read_csv(here("data","by_fuel_by_end_use.csv"))

fuel_use_tidy <- fuel_use %>%
  pivot_longer(cols=c(4:7), names_to = "fuel", values_to = "btu") 


fuel_use_tot <- fuel_use_tidy %>%
  group_by(census_region,end_use,sub_region,fuel) %>%
  summarize(btu=sum(btu, na.rm=TRUE))


fuel_use_coll_all <- fuel_use_tidy %>%
  group_by(census_region, sub_region, end_use) %>%
  summarize(btu=sum(btu, na.rm=TRUE)) 
