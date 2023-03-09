library(tidyverse)

fuels_79_15 <- read_csv(here::here("data","fuels_over_time.csv")) %>%
  janitor::clean_names() 

colnames(fuels_79_15)[3]="quad_btu"



top_4_tot <- fuels_79_15 %>%
  filter(fuel %in% c("Electricity", "Natural Gas", "Fuel Oil/Kerosene", "LPG", "Total")) %>%
  mutate(MJ = quad_btu*0.011*1000, MJ_hh = mill_btu_per_hh_avg*0.011*1000) 

top_2 <- top_4_tot %>%
  filter(fuel %in% c("Electricity", "Natural Gas"))

 dashboard <- ggplot(data=top_2,(aes(x=year, y=MJ, group=fuel))) +
geom_line(size=2, aes(color=fuel)) +
  geom_point(size=1,aes(color=fuel))+
  labs(y="MJ", x="", title="Total U.S. Fuel Use by Type, 1997-2015", color="Fuel Type")+
  scale_color_manual(values=c( "seagreen3", "seagreen1"))+
  theme_minimal()

dashboard
