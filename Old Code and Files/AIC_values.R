#All AIC values for models we've been using thus far: 

#load packages
.packages = c("tidyr", "ggplot2", "dplyr", "lme4", "AICcmodavg", "MuMIn", 
              "pbkrtest", "readr", "lubridate", "car",
              "parallel", "data.table", "blmeco", "lsmeans")

sapply(.packages, require, character.only=TRUE)

#Load data
setwd("C:\\Users\\srin662\\Documents\\R\\TEMPEST-SULI-2024")
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

##F_avg not sqrt transformed: 
model.int <- glmer(F_avg ~ BA + Plot  + BA*Plot +
                     (1|ID) + (1|Year) + (1|ID:Year),
                   data = tsb_2, family = gaussian)
AIC(model.int)
#[1] -34002.17

model.noint <- glmer(F_avg ~ BA + Plot  + BA*Plot +
                     (1|ID) + (1|Year),
                   data = tsb_2, family = gaussian)
AIC(model.noint)
#[1] -33538.48

model.ec <- glmer(F_avg ~ BA + Plot + BA*Plot + soil_ec_avg +
                    (1|ID) + (1|Year) + (1|ID:Year),
                  data = tsb_2, family = gaussian)
AIC(model.ec)
#[1] -33965.63
  
model.noec <- glmer(F_avg ~ BA + Plot  + BA*Plot +
                         (1|ID) + (1|Year) + (1|ID:Year),
                       data = tsb_2, family = gaussian)
AIC(model.noec)
#[1] -34002.17

model.vwc <- glmer(F_avg ~ BA + Plot + BA*Plot + soil_ec_avg + 
                     soil_vwc_avg +
                     (1|ID) + (1|Year) + (1|ID:Year),
                   data = tsb_2, family = gaussian)
AIC(model.vwc)
#[1] -33957.12

model.novwc <- glmer(F_avg ~ BA + Plot  + BA*Plot + soil_ec_avg + 
                       (1|ID) + (1|Year) + (1|ID:Year),
                     data = tsb_2, family = gaussian)

AIC(model.novwc)
#[1] -33965.63

##F_avg is sqrt tranformed: 

model.sqrt.int <- glmer(sqrt(F_avg) ~ BA + Plot  + BA*Plot +
                     (1|ID) + (1|Year) + (1|ID:Year),
                   data = tsb_2, family = gaussian)
AIC(model.sqrt.int)
#[1] -17967.69

model.sqrt.noint <- glmer(sqrt(F_avg) ~ BA + Plot  + BA*Plot +
                       (1|ID) + (1|Year),
                     data = tsb_2, family = gaussian)
AIC(model.sqrt.noint)
#[1] -17611.68

model.sqrt.ec <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot + soil_ec_avg +
                         (1|ID) + (1|Year) + (1|ID:Year),
                       data = tsb_2, family = gaussian)
AIC(model.sqrt.ec)
#[1] -17955.2

model.sqrt.noec <- glmer(sqrt(F_avg) ~ BA + Plot  + BA*Plot +
                      (1|ID) + (1|Year) + (1|ID:Year),
                    data = tsb_2, family = gaussian)
AIC(model.sqrt.noec)
#[1] -17967.69

model.sqrt.vwc <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot + soil_ec_avg + 
                     soil_vwc_avg +
                     (1|ID) + (1|Year) + (1|ID:Year),
                   data = tsb_2, family = gaussian)
AIC(model.sqrt.vwc)
#[1] -17952.33

model.sqrt.novwc <- glmer(sqrt(F_avg) ~ BA + Plot  + BA*Plot + soil_ec_avg + 
                       (1|ID) + (1|Year) + (1|ID:Year),
                     data = tsb_2, family = gaussian)

AIC(model.sqrt.novwc)
#[1] -17955.2

#Also going to add in the fit for ec + random effects without the BACI term:
model.nobaci <- glmer(F_avg ~ soil_ec_avg + (1|ID) + (1|Year) + (1|ID:Year),
                      data = tsb_2, family = gaussian)
AIC(model.nobaci)
#[1] -34050.22

model.sqrt.nobaci <- glmer(sqrt(F_avg) ~ soil_ec_avg + (1|ID) + (1|Year) + (1|ID:Year),
                      data = tsb_2, family = gaussian)
AIC(model.sqrt.nobaci)
#[1] -17992.04
