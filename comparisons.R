
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
