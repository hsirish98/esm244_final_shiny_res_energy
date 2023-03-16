library(tidyverse)
library(here)


fuel_use <- read_csv(here("data","by_fuel_by_end_use.csv"))

fuel_use_tidy <- fuel_use %>%
  pivot_longer(cols=c(4:7), names_to = "fuel", values_to = "btu") %>%
  mutate(MJ = btu*0.0011*1000) ##this goes from trillion BTU to billion MJ

fuel_use_tidy_test <-   fuel_use_tidy %>%
  filter(end_use %in% "Space Heating", census_region %in% "Midwest") %>%
  group_by(sub_region) %>%
  summarize(MJ = sum(MJ, na.rm=TRUE))

fuel_use_tot <- fuel_use_tidy %>%
  group_by(census_region,fuel,end_use) %>%
  summarize(MJ=sum(MJ, na.rm=TRUE))




fuel_use_tot_test <- fuel_use_tot %>%
  filter(census_region %in% "Northeast")%>%
  filter(end_use %in% "Space Heating") %>%
  # group_by(sub_region, fuel)%>%
  summarize(sum=sum(MJ))


fuel_use_coll_all <- fuel_use_tidy %>%
  group_by(census_region, sub_region, end_use) %>%
  summarize(MJ=sum(MJ, na.rm=TRUE)) 
