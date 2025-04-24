##Testing for variance explained by random effects in BACI + ec model

.packages = c("tidyr", "ggplot2", "dplyr", "lme4",
              "AICcmodavg", "MuMIn", "pbkrtest", "readr",
              "lubridate", "car", "parallel", "data.table",
              "blmeco", "lsmeans", "patchwork", "viridis",
              "forcats", "ggpmisc", "stringr", "e1071")
#Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

#Attach packages
sapply(.packages, require, character.only=TRUE)

data <- readRDS("Full_21_24_updated.rds")

###This is all cleaning up & filtering data: 

#TEMPEST EVENTS
tempest_events <- bind_rows(
  tibble( Year = 2021, flood_start = "2021-06-12", flood_end = "2021-06-12"), #average date = June 12
  tibble( Year = 2022, flood_start = "2022-06-22", flood_end = "2022-06-22"), #June 22
  tibble( Year = 2023, flood_start = "2023-06-06", flood_end = "2023-06-07"), #June 6, 7
  tibble( Year = 2024, flood_start = "2024-06-11", flood_end = "2024-06-13")) #June 11, 12, 13

tempest_events %>%
  mutate(flood_start = ymd(flood_start),
         flood_end = ymd(flood_end)) -> events

#window of data to look at for BACI analysis
window <- days(15)

data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`,
                soil_vwc_15cm, soil_ec_15cm) %>%
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
  filter(`F` < 4e-06) %>% #remove outliers, ggplot below for confirmation
  group_by(Date, Plot, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            soil_ec_avg = mean(soil_ec_15cm), 
            soil_vwc_avg = mean(soil_vwc_15cm),
            n = n()) %>%
  ungroup() -> tsb_1

tsb_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) %>%
  mutate(BA = as.character(BA)) %>%
  mutate(BA = ifelse(Year == 2021, "Before", BA)) -> tsb_2

###New code:

model.int.ec <- lmer(sqrt(F_avg) ~ BA*Plot + soil_ec_avg +
                       (1|ID) + (1|Year) + (1|ID:Year),
                     data = tsb_2)
model.noint.ec <- lmer(sqrt(F_avg) ~ soil_ec_avg +
                         (1|ID) + (1|Year) + (1|ID:Year),
                       data = tsb_2)
model.int.og <- lmer(sqrt(F_avg) ~ BA*Plot +
                       (1|ID) + (1|Year) + (1|ID:Year),
                     data = tsb_2)
#For MuMin 
MuMIn::r.squaredGLMM(model.int.ec)
#           R2m       R2c
#[1,] 0.05035081 0.7693562
MuMIn::r.squaredGLMM(model.noint.ec)
#            R2m      R2c
#[1,] 0.0006389255 0.756138
MuMIn::r.squaredGLMM(model.int.og)            
#            R2m       R2c
#[1,] 0.04604163 0.7665745


#For eta squared
aov(formula = (sqrt(F_avg) ~ BA + Plot + BA * Plot + soil_ec_avg), 
    data = tsb_2)
#Terms:
#                          BA         Plot  soil_ec_avg      BA:Plot    Residuals
#Sum of Squares  1.529620e-06 6.146420e-06 3.628000e-08 7.333600e-07 8.681772e-05
#Deg. of Freedom            1            1            1            1         1249

#Residual standard error: 0.0002636471

#BA: 1.170480e-06/9.187385e-05 = 0.01274008
#Plot: 5.354920e-06/9.187385e-05 = 0.05828557
#soil_ec_avg: 1.045300e-07/9.187385e-05 = 0.001137756
#BA:Plot: 7.455100e-07/9.187385e-05 = 0.008114496
