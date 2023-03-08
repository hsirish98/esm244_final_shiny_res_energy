library(sf)
library(tidyverse)

source(here::here("wrangle_files/percent_electrified.R"))
source(here::here("wrangle_files/energy_insecurity_wrang.R"))
       
states_sf <- read_sf(here("spat_dat"),layer="ne_110m_admin_1_states_provinces") %>%
  select(name, geometry, postal)

  
states_contig_sf <- states_sf %>%
  filter(!(postal %in% c("DC")))

states_contig_sf <- merge(states_contig_sf, pct_e_20, by="name")

census_reg <- read_csv(here::here("data/census_regions.csv"))
colnames(census_reg)[3]<- "postal"

states_contig_sf <- merge(states_contig_sf,census_reg, by="postal")

states_contig_sf <- merge(states_contig_sf, energy_ins, by="name")




states_sf <- st_transform(states_sf,3857)
states_contig_sf <- st_transform(states_contig_sf,3857)






