library(tidyverse)

pct_e <- readxl::read_xlsx(here("data/states_fuels_used.xlsx")) %>%
  select(1,4) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(home_is_all_electric=as.numeric(home_is_all_electric))

pct_e_20 <- pct_e %>%
  filter(!(x1 %in% c("District of Columbia", "All homes", "Alaska", "Hawaii")))

pct_alaska <- pct_e %>%
  filter(x1=="Alaska")

pct_hawaii <- pct_e %>%
  filter(x1=="Hawaii")


