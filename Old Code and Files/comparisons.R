
sf_scaled %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  mutate(monthyr = floor_date(TIMESTAMP, unit = "week")) %>%
  filter(Date == "2021-06-12") %>% 
  filter(F <= 0.005, F >= 0) %>%
  group_by(Plot, Species, Hour) %>% 
  summarise(F_avg = mean(Fd*3.6, na.rm = TRUE)) -> sf_plot_avg


#plot hourly midday sapflow
ggplot(sf_plot_avg) + 
  geom_point(aes (x = Hour, y = F_avg, color = Species)) + 
  facet_wrap(~Plot, ncol = 1, scales = "fixed") + 
  labs(y = "Sap Flow L/h", x = "Hour", title = "Plot Avg, June 12th 2021")


sf_scaled %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  mutate(monthyr = floor_date(TIMESTAMP, unit = "week")) %>%
  filter(Date == "2021-07-06") %>% 
  filter(F <= 0.005, F >= 0) %>%
  group_by(Plot, Species, Hour) %>% 
  summarise(F_avg = mean(Fd*3.6, na.rm = TRUE)) -> sf_plot_avg


#plot hourly midday sapflow
ggplot(sf_plot_avg) + 
  geom_point(aes (x = Hour, y = F_avg, color = Species)) + 
  facet_wrap(~Plot, ncol = 1, scales = "fixed") + 
  labs(y = "Sap Flow L/h", x = "Hour", title = "Plot Avg, July 6th 2021")

#soil ec comparison 

final_tmp_data %>%
  filter(Plot != "Freshwater", 
         Species == "Tulip Poplar") %>%
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  group_by(Plot, ID, Date, Year) %>%
  summarise(ec_avg = mean(soil_ec_15cm))-> soil_ec_plot

ggplot(soil_ec_plot) +
  geom_point(aes (x = Date, y = ec_avg, color = as.factor(Year))) + 
  geom_vline(xintercept = as.Date(c("2022-06-22", "2023-06-06", "2023-06-07","2024-06-11", "2024-06-13")), linetype = "dashed", color = "black")+
  facet_wrap(~ Plot, ncol= 2, scales = "fixed") + 
  labs(y = "Soil EC, Averaged Daily", x = "Date", title = "Soil EC in Control vs Saltwater Plots")




