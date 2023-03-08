library(tidyverse)

# test <- readxl::read_xlsx(here("data/states_fuels_used.xlsx")) 

pct_e <- readxl::read_xlsx(here("data/states_fuels_used.xlsx")) %>%
  select(1,4,6) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(home_is_all_electric=as.numeric(home_is_all_electric))

colnames(pct_e)<- c("name","pct_e","pct_i")

pct_e_20 <- pct_e %>%
  filter(!(name %in% c("District of Columbia", "All homes")))



