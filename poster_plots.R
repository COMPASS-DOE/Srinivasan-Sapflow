
data %>%
  dplyr::select(Year, TIMESTAMP, Plot,
                soil_vwc_15cm, soil_ec_15cm) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         BA = as.factor(BA),
         Plot = as.factor(Plot),
         data_start = flood_start - window,
         data_end = flood_end + window,
         #BA = case_when(Year == 2021 ~ "Before", .default = BA),
         BA = factor(BA, levels = c("Before", "After"))) %>%
  filter(Hour < 14, Hour >= 11,
         Plot != "Freshwater",
Date > data_start & Date < flood_start | Date < data_end & Date > flood_end) %>%
  ungroup() %>%
  group_by(Plot, BA, Year, Date, Hour) %>%
  summarise(soil_ec_avg = mean(soil_ec_15cm), 
            soil_vwc_avg = mean(soil_vwc_15cm),
            n = n()) -> soil_data

soil_data_summary = soil_data %>%
  group_by(Year,BA,Plot) %>%
  summarise(mean_annual_VWC = mean(soil_vwc_avg),
            sd_annual_VWC = sd(soil_vwc_avg),
            mean_annual_EC = mean(soil_ec_avg),
            sd_annual_EC = sd(soil_ec_avg),
            n = n())


tulip_salt %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         BA = as.factor(BA),
         Plot = as.factor(Plot),
         BA = case_when(Year == 2021 ~ "Before", .default = BA),
         BA = factor(BA, levels = c("Before", "After"))) %>%
  ungroup() %>%
  group_by(Plot, BA, Year, Date, Hour) %>%
  summarise(soil_ec_avg = mean(soil_ec_15cm), 
            soil_vwc_avg = mean(soil_vwc_15cm),
            n = n()) -> hourly_soil_data

hourly_soil_data %>%
  ggplot(aes(soil_ec_avg, soil_vwc_avg)) +
  geom_point(aes(color = as.factor(Year))) +
  scale_color_viridis_d(begin = 0.9, end = 0.2) +
  facet_wrap(Plot ~ BA, scales = "free_x") +
  geom_smooth(data = hourly_soil_data[hourly_soil_data$soil_ec_avg < 1200,],
              aes(soil_ec_avg, soil_vwc_avg),
              method = 'lm',
              color = 'black', size = 1) +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  geom = "text_npc",
                  aes(label = paste("y = ", round(..estimate..[2], 2), " * x + ", round(..estimate..[1], 2),
                                    "\nR² = ", round(..r.squared.., 2),
                                    "\np = ", formatC(..p.value.., format = "e", digits = 2), sep = "")),
                  label.x = "left",
                  label.y = "top",
                  size = 3.5) +
  theme_light() +
  labs(y = expression ("Soil VWC, m" ^3* " m"^-3),
       x = expression(paste("Soil EC, ", mu, "S cm"^-1)),
       color = " ")

hourly_soil_data %>%
  ggplot(aes(soil_ec_avg, soil_vwc_avg)) +
  geom_point(aes(color = as.factor(Year))) +
  scale_color_viridis_d(begin = 0.9, end = 0.2) +
  facet_wrap(Plot ~ BA, scales = "free_x") +
  geom_smooth(data = hourly_soil_data[hourly_soil_data$soil_ec_avg < 1200, ],
              aes(soil_ec_avg, soil_vwc_avg),
              method = 'lm',
              color = 'black', size = 1) +
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  geom = "text_npc",
                  aes(label = paste("y = ", round(..estimate..[2], 2), " * x + ", round(..estimate..[1], 2),
                                    "\nR² = ", round(..r.squared.., 2),
                                    "\np = ", formatC(..p.value.., format = "e", digits = 2), sep = "")),
                  label.x = "left",
                  label.y = "top",
                  size = 3.5) +
  theme_light() +
  labs(y = expression("Soil VWC, m" ^ 3 * " m" ^ -3),
       x = expression(paste("Soil EC, ", mu, "S cm" ^ -1)),
       color = " ")
