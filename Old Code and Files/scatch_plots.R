library(tidyverse)
library(ggpubr)

full_data %>%
  mutate(SWC = soil_vwc_15cm) %>%
  mutate(SEC = soil_ec_15cm) %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`, PAR, SWC, SEC) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(#Date > data_start & Date < flood_start |
  #        Date < data_end & Date > flood_end,
         Hour < 14, Hour >= 11, 
         #Plot != "Freshwater",
         Species == "Tulip Poplar") %>%
  ungroup() -> tulip_all

tulip_all %>%
  group_by(Year) %>%
  mutate(BA = if_else(Date > flood_end, "After", "Before"),
         BA = if_else(Year == 2021, "Before", BA),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  filter(`F` < 4e-06, 
         SWC > 0.2,
         SWC < 0.6,
         (PAR > 480 | is.na(PAR))) %>%#,
         #month(Date) > 3 | month(Date) < 10) %>%
  group_by(Date, Plot, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n(),
            soil_ec_avg = mean(SEC),
            soil_vwc_avg = mean(SWC)) %>%
  ungroup() -> tsb_4

tsb_4 %>%
  mutate(BA = fct_relevel(BA, "Before", "After")) -> tsb_4

tsb_4 %>%
  filter(BA == "Before",
         #SEC < 500,
         soil_ec_avg < 500,
         Plot != "Control") -> tsb_4a

tsb_4 %>%
  filter(BA == "Before",
         Plot == "Control",
         soil_ec_avg > 50
         #SEC > 50
         ) -> tsb_4b

tsb_4 %>%
  filter(BA == "After",
         soil_ec_avg < 1200,
         #SEC < 1200
         ) %>%
  bind_rows(tsb_4a, tsb_4b) -> tsb_5

#All before data for 2021
tsb_5 %>%
  filter(Year == 2021,
         Plot != "Freshwater") %>%
ggplot(aes(soil_ec_avg, soil_vwc_avg,
           color = Date, group = Plot)) +
  geom_point(aes(shape = Plot), size = 3) +
  stat_smooth(method='lm', color = 'black') + 
  theme_light()

#Before and After data for 2024
tsb_5 %>%
  filter(Plot != "Freshwater") %>%
  ggplot(aes(soil_ec_avg, soil_vwc_avg)) +
  geom_point(aes(color = BA)) +
  stat_smooth(method='lm', color = 'black') + 
  facet_wrap(Year~Plot, scales = "free") +
  theme_light()


ctrl_plot <- tsb_5 %>%
  filter(Plot != 'Freshwater') %>%
  ggplot(aes(x = soil_ec_avg, y = soil_vwc_avg, group = BA)) +
  geom_point(aes(color = Year)) +
  stat_smooth(method = 'lm', color = 'darkblue') +
  facet_wrap(~Plot, scales = 'free')
  
ctrl_plot

#keeping this one because it's so pretty
tsb_4 %>%
  filter(Year == 2021) %>%
  group_by(Year, BA, Plot)
ggplot(aes(soil_ec_avg, F_avg)) +
  geom_point(aes(color = soil_vwc_avg, shape = Plot)) + scale_color_viridis_c() +
  stat_smooth(method = 'lm', color = 'black',
              size = 1) +
  stat_cor(label.y = 2.2e-06) +
  stat_regline_equation() +
  ggtitle("Pre-Treatment EC vs Sapflow") +
  labs(color = "VWC", y = "Sapflow", x = "Soil EC") +
  theme_light()


