#Full plot of sapflow, soil VWC, soil EC, and PAR 
#Midday, control plot average, over all 4 years 

library(patchwork)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

full_data <- readRDS("Full_21_24_updated.rds")

full_data %>%
  group_by(TIMESTAMP) %>%
  mutate(PAR = ifelse(is.na(PAR), PAR[Plot == "Freshwater"], PAR)) %>%
  ungroup() -> dat

##This is going to be diurnal pattern for control plot and tulip poplar, 
##averaged by hour, error-barred for year, 30 day sapflow pattern 

#TEMPEST EVENTS
tempest_events <- bind_rows(
  tibble( Year = 2021, flood_start = "2021-06-12", flood_end = "2021-06-12"), #average date = June 12
  tibble( Year = 2022, flood_start = "2022-06-22", flood_end = "2022-06-22"), #June 22
  tibble( Year = 2023, flood_start = "2023-06-06", flood_end = "2023-06-07"), #June 6, 7
  tibble( Year = 2024, flood_start = "2024-06-11", flood_end = "2024-06-13")) #June 11, 12, 13

tempest_events %>%
  mutate(flood_start = ymd(flood_start),
         flood_end = ymd(flood_end)) -> events

#window of data to look at
window <- days(15)
#Tidy up our data:
dat %>%
  mutate(SWC = soil_vwc_15cm) %>%
  mutate(SEC = soil_ec_15cm) %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`, PAR, SWC, SEC) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Plot == "Control",
         Species == "Tulip Poplar",
         `F` < 2e-06,
         SWC > 0.25) %>%
  ungroup() -> dat1


dat1 %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before")) %>% #remove outliers, ggplot below for confirmation
  mutate(BA = ifelse(Year == 2021, "Before", BA)) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) %>%
  ungroup() %>%
  mutate(day_hr = Day * 24 + Hour) %>%
  group_by(day_hr) %>%
  summarise(F_avg = mean(`F`),
            F_se = sd(`F`)/sqrt(n()),
            F_sd = sd(`F`),
            swc_avg = mean(SWC),
            swc_se = sd(SWC)/sqrt(n()),
            sec_avg = mean(SEC),
            sec_se = sd(SEC)/sqrt(n()),
            par_avg = mean(na.omit(PAR)),
            par_se = sd(na.omit(PAR))/sqrt(n())) %>%
  ungroup() -> dat2


sapflow_plot <- 
dat2 %>%
  filter(day_hr < 97 & day_hr > -97) %>%
  ggplot(aes(x = day_hr, y = F_avg)) +
  geom_ribbon(aes(x = day_hr, ymin = (F_avg - F_sd),
                  ymax = (F_avg + F_sd)),
              fill = 'lightblue', alpha = 0.5) +
  geom_line(color = 'skyblue', size = 0.75 ) +
  geom_point(color = 'blue') +
  scale_x_continuous(name = NULL,
                     breaks = seq(from = min(dat2$day_hr), to = max(dat2$day_hr), by = 24),
                     labels = function(x) x %/% 24 ) +
  labs (y = "Sap Flux Density, g/m^3")

par_plot <- 
  dat2 %>%
  filter(day_hr < 97 & day_hr > -97) %>%
  ggplot(aes(x = day_hr, y = par_avg)) +
  geom_ribbon(aes(x = day_hr, ymin = (par_avg - par_se), 
                  ymax = (par_avg + par_se)),
              fill = 'lightblue', alpha = 0.5) +
    geom_line(color = 'skyblue', size = 0.75 ) +
    geom_point(color = 'blue') +
  scale_x_continuous(name = NULL, 
                     breaks = seq(from = min(dat2$day_hr), to = max(dat2$day_hr), by = 24), 
                     labels = function(x) x %/% 24 ) + 
  labs(y = "PAR, umol/m^2/s")

swc_plot <- dat2 %>%
  filter(day_hr < 145 & day_hr > -145) %>%
  ggplot(aes(x = day_hr, y = swc_avg)) +
  geom_ribbon(aes(x = day_hr, ymin = (swc_avg - swc_se), 
                  ymax = (swc_avg + swc_se)),
              fill = 'lightblue', alpha = 0.5) +
  geom_line(color = 'skyblue', size = 0.75 ) +
  geom_point(color = 'blue') +
  scale_x_continuous(name = "Day", 
                     breaks = seq(from = min(dat2$day_hr), to = max(dat2$day_hr), by = 24), 
                     labels = function(x) x %/% 24 ) +
  labs(y = "SWC, m^3/m^3")

sec_plot <- dat2 %>%
  filter(day_hr < 145 & day_hr > -145) %>%
  ggplot(aes(x = day_hr, y = sec_avg)) +
  geom_ribbon(aes(x = day_hr, ymin = (sec_avg - sec_se), 
                  ymax = (sec_avg + sec_se)),
              fill = 'lightblue', alpha = 0.5) +
  geom_line(color = 'skyblue', size = 0.75 ) +
  geom_point(color = 'blue') +
  scale_x_continuous(name = "Day", 
                     breaks = seq(from = min(dat2$day_hr), to = max(dat2$day_hr), by = 24), 
                     labels = function(x) x %/% 24 ) +
  labs(y = "SEC, m^2/s")

combined_diurnal <- (sapflow_plot | par_plot) / (swc_plot | sec_plot)

combined_diurnal + 
  plot_annotation(title = "Sapflow and Abiotic Variables in Control Plot, 
                   averaged Hourly for pre and post flood period")

ggsave("Diurnal_Sapflow_Abiotic.jpeg")

##Now we do it annually!

dat %>%
  mutate(SWC = soil_vwc_15cm) %>%
  mutate(SEC = soil_ec_15cm) %>%
  mutate(Hour = hour(TIMESTAMP), 
         Date = date(TIMESTAMP),
         day_month = format(TIMESTAMP, "%m-%d")) %>%
  filter(Hour < 14, Hour >= 11, 
         Plot == "Control",
         Species == "Tulip Poplar",
         `F` < 1.5e-06,
          soil_vwc_15cm > 0.25) %>%
  group_by(day_month) %>%
  summarize(F_avg = mean(`F`),
             F_se = sd(`F`)/sqrt(n()),
             swc_avg = mean(SWC),
             swc_se = sd(SWC)/sqrt(n()),
             sec_avg = mean(SEC),
             sec_se = sd(SEC)/sqrt(n()),
             par_avg = mean(PAR, na.rm = TRUE),
             par_se = sd(PAR, na.rm = TRUE)/sqrt(n())) %>%
  ungroup() -> dat3


dat3 %>% 
  pivot_longer(cols = c(F_avg, swc_avg, par_avg, sec_avg), 
               names_to = "Variable") -> long_plot

sapflow_plot <- dat3 %>%
  ggplot() + 
  geom_errorbar(aes(x = day_month, ymin = (F_avg - F_se), 
                  ymax = (F_avg + F_se)) , color = "red", width = 0.2) +
  geom_point(aes(x = day_month, y = F_avg)) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", 
                              "06-01", "07-01", "08-01", "09-01", "10-01", 
                              "11-01", "12-01"), 
                   labels = month.abb) +
  labs(x = "Date", y = "Sap Flux Density, g/m^3") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

par_plot <- dat3 %>%
  ggplot() +
  geom_errorbar(aes(x = day_month, ymin = (par_avg - par_se), 
                   ymax = (par_avg + par_se)) , color = "red", width = 0.2) +
  geom_point(aes(x = day_month, y = par_avg)) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", 
                              "06-01", "07-01", "08-01", "09-01", "10-01", 
                              "11-01", "12-01"), 
                   labels = month.abb) +
  labs(x = "Date", y = "PAR, umol/m^2/s") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

swc_plot <- dat3 %>%
  ggplot() +
  geom_errorbar(aes(x = day_month, ymin = (swc_avg - swc_se), 
                  ymax = (swc_avg + swc_se)) , color = "red", width = 0.2) +
  geom_point(aes(x = day_month, y = swc_avg)) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", 
                              "06-01", "07-01", "08-01", "09-01", "10-01", 
                              "11-01", "12-01"), 
                   labels = month.abb) +
  labs(x = "Date", y = "SWC, m^3/m^3") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

sec_plot <-  dat3 %>%
  ggplot() +
  geom_errorbar(aes(x = day_month, ymin = (sec_avg - sec_se), 
                    ymax = (sec_avg + sec_se)) , color = "red", width = 0.2) +
  geom_point(aes(x = day_month, y = sec_avg)) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", 
                              "06-01", "07-01", "08-01", "09-01", "10-01", 
                              "11-01", "12-01"), 
                   labels = month.abb) +
  labs(x = "Date", y = "SEC, m^2/s") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

combined_plot <- (sapflow_plot | par_plot) / (swc_plot | sec_plot)

combined_plot + 
  plot_annotation( title = "Sapflow and Abiotic Variables in Control Plot, 
                   averaged 11 AM - 2 PM") 

ggplot(data = long_plot, aes(x= day_month, y = value)) + 
  geom_point() + 
  facet_wrap(~Variable, scales = "free", labeller = label_both) 
  

  
  