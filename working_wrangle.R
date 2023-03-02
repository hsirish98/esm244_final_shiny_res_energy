library(tidyverse)
library(here)


fuel_use <- read_csv(here("data","by_fuel_by_end_use.csv"))

fuel_use_tidy <- fuel_use %>%
  pivot_longer(cols=c(4:7), names_to = "fuel", values_to = "btu")
