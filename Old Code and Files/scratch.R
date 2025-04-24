

salt <- data %>%
  ungroup() %>%
  filter(case_when(Species == "Beech" ~ Fd_scaled < 5,
                   Species == "Red Maple" ~ Fd_scaled < 2.25,
                   Species == "Tulip Poplar" ~ Fd_scaled < 8.5)) %>%
  mutate(Date = date(TIMESTAMP),
         Hour = hour(TIMESTAMP),
         doy = yday(Date),
         Year = as.factor(Year)) %>%
  filter(Hour == 12) %>%
  group_by(Year, doy, Plot) %>%
  summarize(midday_vwc = mean(soil_vwc_15cm, na.rm = TRUE),
            midday_ec = mean(soil_ec_15cm, na.rm = TRUE))

salt <- salt %>%
  ungroup() %>%
  group_by(Year, Plot) %>%
  mutate(wc_roll = zoo::rollmean(midday_vwc, 28, align = "center", fill = NA),
         ec_roll = zoo::rollmean(midday_ec, 28, align = "center", fill = NA))

# Create the ggplot
ggplot(data = salt, aes(y = midday_vwc, x = doy, color = midday_ec)) +
  geom_point(alpha = 0.20) +  # Plot original points
  geom_line(aes(y = wc_roll), linewidth = 1.15) +  # Plot the rolling mean
  facet_grid(Plot ~ Year) +
  scale_color_viridis_c(begin = 0.9, end = 0.2) +
  theme_classic() + theme(legend.position="bottom")


ggplot(data = midday, aes(y = midday, x = doy, color = Year, group = Year)) +
  geom_point(alpha = 0.20) +  # Plot original points
  geom_line(aes(y = f_roll), linewidth = 0.95) +  # Plot the rolling mean
  facet_grid(Species ~ Plot, scales = "free_y") +
  scale_color_viridis_d(option = 'D', begin = 0.9, end = 0.2) +
  theme_bw() + theme(legend.position="bottom", element_text(size = 14))