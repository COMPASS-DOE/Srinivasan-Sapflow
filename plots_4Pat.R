

filtered_data %>%
  ungroup() %>%
  mutate(Year = as.factor(Year)) %>%
  filter(
    Year == "2021" &
      (Species != "Tulip poplar" | (Species == "Tulip poplar" & `F` < 0.003))
  ) %>%
  group_by(Plot, Species, Date, Year) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE)) %>%
  ungroup() -> y21

filtered_data %>%
  ungroup() %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Year, Date) %>%
  filter(max(PAR) > 500) %>%
  group_by(Plot, Species, Date, Year) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE)) %>% 
  ungroup() %>%
  bind_rows(y21) -> plot_data

plot_data %>%
  group_by(Year, Plot, Species) %>%
  arrange(Date) %>%
  mutate(f_roll = zoo::rollmean(F_avg, 40, na.rm = TRUE,
                                align = "center", fill = NA)) -> rolled_data

ggplot() +
  geom_jitter(data = plot_data, aes(y = F_avg, x = yday(Date),
                                    color = Year, group = Year),
              alpha = 0.1, size = 0.8) +
  geom_line(data = rolled_data, aes(y = f_roll, x = yday(Date),
                                    color = Year, group = Year),
            linewidth = 1.5) +
  facet_grid(Species ~ Plot, scales="free") +
  scale_color_viridis_d(option = 'D', begin = 0.1, end = 0.975) +
  labs(y = expression ("Midday Sap Flow, cm"^3* " s"^-1),
       x = expression(paste("Day of Year"))) +
  theme_bw() + theme(legend.position="bottom", element_text(size = 14))

ggplot() +
  geom_jitter(data = plot_data %>% filter(Plot == "Saltwater",
                                          Species == "Tulip Poplar"), aes(y = F_avg, x = yday(Date),
                                    color = Year, group = Year),
              alpha = 0.2, size = 0.8) +
  geom_line(data = rolled_data %>% filter(Plot == "Saltwater",
                                          Species == "Tulip Poplar"), aes(y = f_roll, x = yday(Date),
                                    color = Year, group = Year),
            linewidth = 1.75) +
  #facet_grid(Species ~ Plot, scales="free") +
  scale_color_viridis_d(option = 'D', begin = 0.1, end = 0.975) +
  labs(y = expression ("Midday Sap Flow, cm"^3* " s"^-1),
       x = expression(paste("Day of Year"))) +
  theme_bw() + theme(legend.position="bottom", element_text(size = 14))
