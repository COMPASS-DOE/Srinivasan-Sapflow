
midday <- final_data %>%
  ungroup() %>%
  # filter values below need to be updated, Fd_scaled does not exsist
  # filter(case_when(Species == "Beech" ~ Fd_scaled < 5,
  #                  Species == "Red Maple" ~ Fd_scaled < 2.25,
  #                  Species == "Tulip Poplar" ~ Fd_scaled < 8.5)) %>%
  mutate(Date = date(TIMESTAMP),
         Hour = hour(TIMESTAMP),
         doy = yday(Date),
         Year = as.factor(Year)) %>%
  filter(Hour == 12,
         F <= 0.005, F >= 0) %>%
  group_by(Year, doy, Species, Plot) %>%
  summarize(midday = mean(Fd, na.rm = TRUE))

midday <- midday %>%
  ungroup() %>%
  group_by(Year, Plot, Species) %>%
  mutate(f_roll = zoo::rollmean(midday, 28, align = "center", fill = NA))

#from .Rmd
events$Year <- as.factor(events$Year)

midday <- midday %>%
  left_join(events, by = "Year") %>%
  mutate(doy_start = yday(flood_start),
         doy_end = yday(flood_end))

midday %>%
  filter(Species == "Tulip Poplar",
         Plot != "Freshwater",
         midday < 6) %>%
  ggplot(aes(y = midday, x = doy, color = Plot, group = Plot)) +
  geom_vline(data = midday[midday$Year != "2021",], #Plot event dates
             aes(xintercept = doy_start,
                 group = Year), color = 'red') +
  geom_line(aes(y = f_roll), linewidth = 0.95) +  # Plot the rolling mean
  geom_point(alpha = 0.15) +  # Plot original points
  facet_wrap(. ~ Year, ncol = 4) +
  scale_color_viridis_d(option = 'G', begin = 0.7, end = 0.3) +
  labs(y = expression ("Midday Sap Flow, cm"^3* " s"^-1),
       x = expression(paste("Day of Year"))) +
  theme_bw() + theme(legend.position="bottom", element_text(size = 14))
