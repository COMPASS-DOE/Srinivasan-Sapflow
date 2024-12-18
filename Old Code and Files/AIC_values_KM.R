#All AIC values for models we've been using thus far: 

#load packages
.packages = c("tidyr", "ggplot2", "dplyr", "lme4", "AICcmodavg", "MuMIn", 
              "pbkrtest", "readr", "lubridate", "car",
              "parallel", "data.table", "blmeco", "lsmeans")

sapply(.packages, require, character.only=TRUE)

#Load data
#setwd("C:\\Users\\srin662\\Documents\\R\\TEMPEST-SULI-2024")
full_data <- readRDS("Full_21_24_updated.rds")

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
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Hour < 14, Hour >= 11, 
         Plot != "Freshwater",
         Species == "Tulip Poplar") %>%
  ungroup() -> tulip_salt

tulip_salt %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  filter(`F` < 4e-06, 
         SWC > 0.25,
         (PAR > 480 | is.na(PAR))) %>%
  group_by(Date, Plot, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n(), 
            soil_ec_avg = mean(SEC), 
            soil_vwc_avg = mean(SWC)) %>%
  ungroup() -> tsb_1

tsb_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) %>%
  mutate(BA = as.character(BA)) %>%
  mutate(BA = ifelse(Year == 2021, "Before", BA)) -> tsb_2

##F_avg is sqrt tranformed: 

model.sqrt.int <- glmer(sqrt(F_avg) ~ BA + Plot  + BA*Plot +
                     (1|ID) + (1|ID:Year),
                   data = tsb_2, family = gaussian)
mod1 <- AIC(model.sqrt.int)
#[1] -17949.12

model.sqrt.ec <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot + soil_ec_avg +
                         (1|ID) + (1|ID:Year),
                       data = tsb_2, family = gaussian)
mod2 <- AIC(model.sqrt.ec)
#[1] -17936.29

model.sqrt.vwc <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot + soil_ec_avg + 
                     soil_vwc_avg +
                     (1|ID) + (1|ID:Year),
                   data = tsb_2, family = gaussian)
mod3 <- AIC(model.sqrt.vwc)
#[1] -17935.79

#Also going to add in the fit for ec + random effects without the BACI term:
model.sqrt.ec.nobaci <- glmer(sqrt(F_avg) ~ soil_ec_avg + (1|ID) + (1|ID:Year),
                      data = tsb_2, family = gaussian)
mod4 <- AIC(model.sqrt.ec.nobaci)
#[1] -17963.55


mod_names <- c("BACI only", "BACI + EC", "BACI + EC + VWC", "EC only")
mods <- c(mod1, mod2, mod3, mod4)

mod_data <- tibble(mods, mod_names)

library(ggplot2)

ggplot(mod_data, aes(mod_names, mods + 17920,
                     fill = mod_names)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
