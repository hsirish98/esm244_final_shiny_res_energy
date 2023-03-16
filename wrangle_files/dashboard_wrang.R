library(tidyverse)

fuels_79_15 <- read_csv(here::here("data","fuels_over_time.csv")) %>%
  janitor::clean_names() 

colnames(fuels_79_15)[3]="quad_btu"

fuels_79_15$fuel <- gsub("LPG", "Propane", fuels_79_15$fuel)




top_4_tot <- fuels_79_15 %>%
  filter(fuel %in% c("Electricity", "Natural Gas", "Fuel Oil/Kerosene", "Propane", "Total")) %>%
  mutate(MJ = quad_btu*0.0011*1000000, MJ_hh = mill_btu_per_hh_avg*0.0011*1000) 

top_2 <- top_4_tot %>%
  filter(fuel %in% c("Electricity", "Natural Gas"))

