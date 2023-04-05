library(tidyverse)
library(waffle)
library(extrafont)
library(here)

house_char <- readxl::read_xlsx(here("data","State_Household_Characteristics.xlsx"))

energy_ins <- house_char %>%
  select(1,10) %>%
  drop_na() 

colnames(energy_ins) <- c("name", "pct_insecure")

energy_ins <- energy_ins %>%
  filter(!(name %in% c("All homes", "District of Columbia")))

insecurity <- read_csv(here("data", "fuels_ins.csv"))

colnames(insecurity) <- c("fuel", "households_mill", "any_ins", "reduce_food_med", 
                          "unhealthy_temp","disconnect", "unable_heat", "unable_air")


insecurity <- insecurity %>% 
  select(-8)


ins_tidy <- pivot_longer(insecurity,
                         cols = (3:7),
                         names_to = "indicator",
                         values_to = "houses") %>%
  mutate(percent = houses/households_mill)


#%>%
  #filter(indicator=="any_ins")
# 
#  ggplot(ins_tidy, aes(x=fuel, y=percent, fill=percent)) +
#    geom_col(aes(fill=percent))+
#    scale_fill_gradient(low="lightgreen", high="darkgreen")+
#    theme_bw()
 






