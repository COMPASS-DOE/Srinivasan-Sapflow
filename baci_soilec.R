##(Trying to) bring in soil electrical conductivity to the BACI model

#Model: 
#F_avg ~ BA + Plot + BA*Plot + soil_ec +
#        (1|ID) + (1|Year) + (1|ID:Year)
#Preliminarily focus on all years (21-24 before and after), saltwater
#treatment, and tulip poplars 

#load packages
.packages = c("tidyr", "ggplot2", "dplyr", "lme4", "AICcmodavg", "MuMIn", 
              "pbkrtest", "readr", "lubridate", "car",
              "parallel", "data.table", "blmeco", "lsmeans")

sapply(.packages, require, character.only=TRUE)

#Load data
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

##Confirm outliers with plotting: 
#Histograms & scatterplot of SEC so we can see what the distribution 
#is like: 

tulip_salt %>%
  filter (`F` < 4e-06) %>%
  group_by (Hour,Date, Year, Plot) %>%
  summarize(avg_SEC = mean(SEC)) -> secplot

ggplot(secplot, aes(x = avg_SEC)) + 
  geom_histogram()

ggplot(secplot, aes(x=Date, y= avg_SEC, color = Plot)) + 
  geom_point() + facet_grid(~Year, scales = "free")

#Filter out <500

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

ggplot(tsb_2, aes(Day, F_avg, color = ID, shape = BA)) +
  geom_point(size = 2.5) + facet_grid(Plot~Year, scales = "free")

####

model.int <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot + soil_ec_avg +
                        (1|ID) + (1|Year) + (1|ID:Year),
                      data = tsb_2, family = gaussian)
Anova(model.int, type = "III")

model.noint <- glmer(sqrt(F_avg) ~ BA + Plot + soil_ec_avg +
                          (1|ID) + (1|Year) + (1|ID:Year),
                        data = tsb_2, family = gaussian)

refdist.pb.100.interaction <- PBrefdist(largeModel = model.int, 
                                        smallModel = model.noint, 
                                        nsim = 100)

compar.interaction.100 <- PBmodcomp(largeModel = model.int, 
                                    smallModel = model.noint,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100
# Bootstrap test; time: 43.99 sec; samples: 100; extremes: 0;
# large : sqrt(F_avg) ~ BA + Plot + BA * Plot + soil_ec_avg + (1 | ID) + 
#   (1 | Year) + (1 | ID:Year)
# sqrt(F_avg) ~ BA + Plot + soil_ec_avg + (1 | ID) + (1 | Year) + 
#   (1 | ID:Year)
# stat df   p.value    
# LRT    21.858  1 2.935e-06 ***
# PBtest 21.858     0.009901 ** 

#Compare to BACI results without EC in model: 
## Bootstrap test; time: 36.60 sec; samples: 100; extremes: 25;
## large : sqrt(F_avg) ~ BA + Plot + BA * Plot + (1 | ID) + (1 | Year) + 
##     (1 | ID:Year)
## sqrt(F_avg) ~ BA + Plot + (1 | ID) + (1 | Year) + (1 | ID:Year)
##         stat df p.value
## LRT    1.361  1  0.2434
## PBtest 1.361     0.2574

#AIC comparison test between BACI models
Cand.set <- list()

Cand.set[[1]] <- glmer(F_avg ~ BA + Plot  + BA*Plot +
                         (1|ID) + (1|Year) + (1|ID:Year),
                       data = tsb_2, family = gaussian)
model.noec <- Cand.set[[1]]

Cand.set[[2]] <- glmer(F_avg ~ BA + Plot + BA*Plot + soil_ec_avg +
                         (1|ID) + (1|Year) + (1|ID:Year),
                       data = tsb_2, family = gaussian)
model.ec <- Cand.set[[2]]

AIC.res.table <- aictab(cand.set = list(Cand.set[[1]], Cand.set[[2]]), 
                        modnames = paste0("Cand.set_", c(1,2)), 
                        second.ord = TRUE)
AIC.res.table

#Says better candidate is the model without ec? 
# Model selection based on AICc:
#   
#   K      AICc Delta_AICc AICcWt Cum.Wt   Res.LL
# Cand.set_1 8 -34002.05       0.00      1      1 17009.08
# Cand.set_2 9 -33965.48      36.57      0      1 16991.82

AIC(model.noec)
AIC(model.ec)

BIC(model.noec)
BIC(model.ec)

##Adding in soil vwc to model? 
model.int.vwc <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot + soil_ec_avg + 
                      soil_vwc_avg +
                     (1|ID) + (1|Year) + (1|ID:Year),
                   data = tsb_2, family = gaussian)
Anova(model.int.vwc, type = "III")
