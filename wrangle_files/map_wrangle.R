library(sf)
library(tidyverse)

source(here::here("wrangle_files/percent_electrified.R"))
       
states_sf <- read_sf(here("spat_dat"),layer="ne_110m_admin_1_states_provinces") %>%
  select(name, geometry, postal)

  
states_contig_sf <- states_sf %>%
  filter(!(postal %in% c("DC", "AK","HI")))

states_contig_sf$pct_e <- pct_e_20$home_is_all_electric

alaska_sf <- states_sf %>%
  filter(postal=="AK")

alaska_sf$pct_e <- pct_alaska$home_is_all_electric

hawaii_sf <- states_sf %>%
  filter(postal=="HI")

hawaii_sf$pct_e <- pct_hawaii$home_is_all_electric

states_sf <- st_transform(states_sf,3857)
states_contig_sf <- st_transform(states_contig_sf,3857)
alaska_sf <- st_transform(alaska_sf,3857)
hawaii_sf <- st_transform(hawaii_sf, 3857)






