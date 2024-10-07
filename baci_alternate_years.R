##Recreating analysis in TMP_BACI_Sapflow_Analysis with the following years: 
## all of '21 (before) and post-'22 (after)
## all of '21 (before) and post-'23 (after)
## all of '21 (before) and post-'24 (after)

#load packages
.packages = c("tidyr", "ggplot2", "dplyr", "lme4", "AICcmodavg", "MuMIn", 
              "pbkrtest", "readr", "lubridate", "car",
              "parallel", "data.table", "blmeco", "lsmeans")

sapply(.packages, require, character.only=TRUE)

#Load data
full_data <- readRDS("Full_21_24.rds")

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
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`, PAR, SWC) %>%
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

##Confirm outliers with plotting: 
#Histograms & scatterplot of PAR and SWC so we can see what the distribution 
#is like: 

tulip_salt %>%
  filter (`F` < 4e-06) %>%
  group_by (Hour,Date, Year, Plot) %>%
  summarize(avg_PAR = mean(PAR)) -> parplot

ggplot(parplot, aes(x = avg_PAR)) + 
  geom_histogram()

ggplot(parplot, aes(x=Date, y= avg_PAR)) + 
  geom_point() + facet_grid(~Year, scales = "free")

#Filter out <500? <480?

tulip_salt %>%
  filter (`F` < 4e-06) %>%
  group_by (Hour, Date, Year, Plot) %>%
  summarize(avg_SWC = mean(SWC)) -> swcplot

ggplot(swcplot, aes(x = avg_SWC)) + 
  geom_histogram()

ggplot(swcplot, aes(x=Date, y= avg_SWC)) + 
  geom_point() + facet_grid(Plot~Year, scales = "free")

#FIlter out <0.25

ggplot(tulip_salt[tulip_salt$`F` < 4e-06,],
       aes(Date, `F`, color = ID)) +
  geom_point() + facet_grid(Plot~Year, scales = "free")

#

tulip_salt %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  filter(`F` < 4e-06, 
         SWC > 0.25, 
         (PAR >480 | is.na(PAR))) %>%
  group_by(Date, Plot, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n()) %>%
  ungroup() -> tsb_1

tsb_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) %>%
  mutate(BA = as.character(BA)) %>%
  mutate(BA = ifelse(Year == 2021, "Before", BA)) -> tsb_2

ggplot(tsb_2, aes(Day, F_avg, color = ID, shape = BA)) +
  geom_point(size = 2.5) + facet_grid(Plot~Year, scales = "free")

####
##Filter out dataframes for 22, 23, and 24

tsb_2 %>%
  filter(Year == '2021' | 
         (Year == '2022' & BA == 'After')) -> tsb22
tsb_2 %>%
  filter(Year == '2021' | 
           (Year == '2023' & BA == 'After')) -> tsb23
tsb_2 %>%
  filter(Year == '2021' | 
           (Year == '2024' & BA == 'After')) -> tsb24

##for 22: 
model.int_22 <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot +
                          (1|ID),
                        data = tsb22, family = gaussian)
Anova(model.int_22, type = "III")

model.noint_22 <- glmer(sqrt(F_avg) ~ BA + Plot +
                          (1|ID),
                        data = tsb22, family = gaussian)

refdist.pb.100.interaction <- PBrefdist(largeModel = model.int_22, 
                                        smallModel = model.noint_22, 
                                        nsim = 100)

compar.interaction.100 <- PBmodcomp(largeModel = model.int_22, 
                                    smallModel = model.noint_22,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100

##for 23: 
model.int_23 <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot +
                        (1|ID),
                      data = tsb23, family = gaussian)
Anova(model.int_23, type = "III")

model.noint_23 <- glmer(sqrt(F_avg) ~ BA + Plot +
                          (1|ID),
                        data = tsb23, family = gaussian)

refdist.pb.100.interaction <- PBrefdist(largeModel = model.int_23, 
                                        smallModel = model.noint_23, 
                                        nsim = 100)

compar.interaction.100 <- PBmodcomp(largeModel = model.int_23, 
                                    smallModel = model.noint_23,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100

##for 24: 
model.int_24 <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot +
                        (1|ID),
                      data = tsb24, family = gaussian)
Anova(model.int_24, type = "III")

model.noint_24 <- glmer(sqrt(F_avg) ~ BA + Plot +
                          (1|ID),
                        data = tsb24, family = gaussian)

refdist.pb.100.interaction <- PBrefdist(largeModel = model.int_24, 
                                        smallModel = model.noint_24, 
                                        nsim = 100)

compar.interaction.100 <- PBmodcomp(largeModel = model.int_24, 
                                    smallModel = model.noint_24,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100



