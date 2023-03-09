library(tidyverse)

fuels_79_15 <- read_csv(here::here("data","fuels_over_time.csv")) %>%
  janitor::clean_names() 

colnames(fuels_79_15)[3]="quad_btu"


top_4 <- fuels_79_15 %>%
  filter(fuel %in% c("Electricity", "Natural Gas", "Fuel Oil/Kerosene", "LPG"))%>%
  filter(year %in% (1979:2001))

dashboard <- ggplot(data=top_4, aes(x=year, y=quad_btu, group=fuel)) +
  geom_line(aes(color=fuel)) +
  geom_point(size=0.2,aes(color=fuel))+
  scale_color_brewer(palette = "Set1")+
  theme_minimal()

dashboard
