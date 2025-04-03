
midday <- data %>%
  ungroup() %>%
  filter(case_when(Species == "Beech" ~ Fd_scaled < 5,
                   Species == "Red Maple" ~ Fd_scaled < 2.25,
                   Species == "Tulip Poplar" ~ Fd_scaled < 8.5)) %>%
  mutate(Date = date(TIMESTAMP),
         Hour = hour(TIMESTAMP),
         doy = yday(Date),
         Year = as.factor(Year)) %>%
  filter(Hour == 12) %>%
  group_by(Year, doy, Species, Plot) %>%
  summarize(midday = mean(Fd_scaled, na.rm = TRUE))

midday <- midday %>%
  ungroup() %>%
  group_by(Year, Plot, Species) %>%
  mutate(f_roll = zoo::rollmean(midday, 28, align = "center", fill = NA))

# Create the ggplot
ggplot(data = midday, aes(y = midday, x = doy, color = Plot, group = Plot)) +
  geom_point(alpha = 0.20) +  # Plot original points
  geom_line(aes(y = f_roll), linewidth = 1.15) +  # Plot the rolling mean
  facet_wrap(Species ~ Year, scales = "free", ncol = 4) +
  scale_color_viridis_d(option = 'D', begin = 0.9, end = 0.2) +
  theme_classic() + theme(legend.position="bottom")


ggplot(data = midday, aes(y = midday, x = doy, color = Year, group = Year)) +
  geom_point(alpha = 0.20) +  # Plot original points
  geom_line(aes(y = f_roll), linewidth = 0.95) +  # Plot the rolling mean
  facet_grid(Species ~ Plot, scales = "free_y") +
  scale_color_viridis_d(option = 'D', begin = 0.9, end = 0.2) +
  theme_bw() + theme(legend.position="bottom", element_text(size = 14))

midday %>%
  filter(Plot == "Saltwater",
         Species == "Tulip Poplar") %>%
ggplot(aes(y = midday, x = doy, color = Year, group = Year)) +
  geom_point(alpha = 0.20) +  # Plot original points
  geom_line(aes(y = f_roll), linewidth = 0.95) +  # Plot the rolling mean
  facet_grid(Species ~ Plot, scales = "free_y") +
  scale_color_viridis_d(option = 'D', begin = 0.9, end = 0.2) +
  theme_bw() + theme(legend.position="bottom", element_text(size = 14))

window = 30

data %>%
  dplyr::select(Year, Species, TIMESTAMP, ID, Plot, Fd_scaled) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Hour < 14, Hour >= 11,
         Fd_scaled <= 8.5, Fd_scaled >= 0,
         Species == "Tulip Poplar") %>%
  ungroup() %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         BA = factor(BA, levels = c("Before", "After"))) %>%
  group_by(BA, Plot, Year, Date) %>%
  summarize(F_avg = mean(Fd_scaled, na.rm = TRUE)) -> sf

data %>%
  dplyr::select(Year, TIMESTAMP, Plot, soil_ec_15cm, soil_vwc_15cm) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Hour < 14, Hour >= 11) %>%
  ungroup() %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         BA = factor(BA, levels = c("Before", "After"))) %>%
  group_by(BA, Plot, Date, Year) %>%
  summarize(ec_avg = mean(soil_ec_15cm, na.rm = TRUE),
            wc_avg = mean(soil_vwc_15cm, na.rm = TRUE)) -> ec

ec %>%
  right_join(sf, by = c("BA", "Year", "Date", "Plot")) %>%
  filter(Year == 2021) -> sf_ec21

ggplot(data = sf_ec21) +
  stat_smooth(aes(x = ec_avg, y = F_avg, group = Year), method = 'lm',
              color = 'darkblue', alpha = 0.2, size = 1) +
  geom_point(aes(x = ec_avg, y = F_avg,
                 color = wc_avg),
             size = 2.2) +
  facet_wrap(Plot ~ BA, ncol = 2, scales = "free_x") + theme_classic() +
  labs(y = expression ("Mean Sap Flux Density, m" ^3* " s"^-1),
       x = expression(paste("Mean Soil EC, ", mu, "S cm"^-1)),
       color = " ")

ec %>%
  filter(Plot == "Saltwater") %>%
  filter(case_when(BA == "Before" ~ ec_avg < 150,
                   BA == "After" ~ ec_avg > 150 &
                     ec_avg < 1000)) -> ec_salt

ec %>%
  filter(Plot != "Saltwater") %>%
  bind_rows(ec_salt) %>%
  right_join(sf, by = c("BA", "Year", "Date", "Plot")) -> sf_ec

ggplot(data = sf_ec[sf_ec$Year == 2021,]) +
  stat_smooth(aes(x = ec_avg, y = F_avg, group = Year), method = 'lm',
              color = 'darkblue', alpha = 0.2, size = 1) +
  geom_point(aes(x = ec_avg, y = F_avg,
                 color = wc_avg, shape = as.factor(Year)),
             size = 2.2) +
  facet_wrap(Plot ~ BA, ncol = 2, scales = "free_x") + theme_classic() +
  labs(y = expression ("Mean Sap Flux Density, m" ^3* " s"^-1),
       x = expression(paste("Mean Soil EC, ", mu, "S cm"^-1)),
       color = " ")


tsb_2_summary2 = tsb_2 %>%
  filter(soil_ec_avg < 1250) %>%
  mutate(BA = case_when(Year == 2021 ~ "Before", .default = BA),
         BA = factor(BA, levels = c("Before", "After")),) %>%
  group_by(Year,BA,Plot) %>%
  summarise(mean_annual_vwc = mean(soil_vwc_avg),
            sd_annual_vwc = sd(soil_vwc_avg),
            mean_annual_EC = mean(soil_ec_avg),
            sd_annual_EC = sd(soil_ec_avg),
            n = n())

tsb_2_summary2 %>%
  ggplot() +
  geom_point(aes(mean_annual_EC, mean_annual_vwc,
                 color = as.factor(Year)), size = 3) +
  geom_errorbar(aes(x = mean_annual_EC,
                    ymin = mean_annual_vwc - sd_annual_vwc,
                    ymax = mean_annual_vwc + sd_annual_vwc,
                    color = Year), size = 1) +
  geom_errorbarh(aes(xmin = mean_annual_EC - sd_annual_EC,
                     xmax = mean_annual_EC + sd_annual_EC,
                     y = mean_annual_vwc,
                     color = Year), size = 1) +
  stat_smooth(aes(x = soil_ec_avg, y = soil_vwc_avg, group = BA), method = 'lm',
              color = 'black', size = 1,
              data = tsb_2 %>% filter(soil_ec_avg < 1200)) +
  geom_point(aes(x = soil_ec_avg, y = soil_vwc_avg, color = as.factor(Year)),
             data = tsb_2 %>% filter(soil_ec_avg < 1200), alpha = 0.2) +
  scale_color_viridis_d(begin = 0.9, end = 0.2) +
  facet_wrap(Plot ~ BA, scales = "free_x") + theme_classic() +
  labs(y = expression ("Mean Soil Volumetric Water Content cm" ^3* " cm"^-3),
       x = expression(paste("Mean Soil EC, ", mu, "S cm"^-1)),
       color = " ")


