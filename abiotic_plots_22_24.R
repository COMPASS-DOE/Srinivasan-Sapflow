#Isolate growing season for 2022 and 2023
#May - September, from 11 am to 12 pm 

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpmisc)

#read in complete data (sapflow, soil vwc, par, and temp for 2022, 23, and 24)
full_data <- readRDS("Full_22_24.rds")

#Isolate for control plot, May to September, 11 am - 12 pm, for 2022 and 23
full_data %>%
  mutate(Hour = hour(TIMESTAMP), 
         Month = month(TIMESTAMP), 
         Date = date(TIMESTAMP)) %>%
  filter(Year != 2024,
         Hour >= 11, Hour <= 12,
         Month >= 5, Month <= 9,
         F <= 17500, F >= 0) %>%
  group_by(Date, Species) %>%
  mutate(avg_f = mean(F)) -> gs_data

#Isolate for available growing season data from 2024:
full_data %>%
  mutate(Hour = hour(TIMESTAMP), 
         Month = month(TIMESTAMP),
         Year = year(TIMESTAMP),
         Date = date(TIMESTAMP),
         Day = day(TIMESTAMP)) %>%
  filter(Month == 4 & Day >= 17 | Month == 5 & Day <= 2) %>%
  filter(F <= 17500, F >= 0, 
         Hour >= 11, Hour <= 12) %>%
  group_by(Date, Species) %>%
  mutate(avg_f = mean(F)) -> aprm_data

#Plot growing season data to check: 
#Control Plot
gs_data %>%
#aprm_data %>%
  filter(Plot == "Control") %>%
  ggplot(aes(x = Date, y = avg_f, color = Species)) + 
  geom_point() +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  labs(x = "Month", y = "Average Sap Flux Density", title = "Avg Sap Flux Density for mid April-May in Control Plot")

#Freshwater Plot
gs_data %>%
  filter(Plot == "Control") %>%
  ggplot(aes(x = Date, y = avg_f, color = Species)) + 
  geom_point() +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  labs(x = "Month", y = "Average Sap Flux Density", title = "Avg Sap Flux Density for mid April-May in Control Plot")

#Boxplot of sapflux by treatments and species, same mid Apr-may timeframe

full_data %>%
  mutate(Hour = hour(TIMESTAMP), 
         Month = month(TIMESTAMP),
         Year = year(TIMESTAMP),
         Date = date(TIMESTAMP),
         Day = day(TIMESTAMP)) %>%
  filter(Month == 4 & Day >= 17 | Month == 5 & Day <= 2) %>%
        filter( F <= 17500, F >= 0) -> boxplot_data

ggplot(boxplot_data) + 
  geom_boxplot(aes (x = as.factor(Year), y = F, fill = Species)) +
  facet_wrap (Species~Plot, ncol = 3, nrow = 3, scales = "free") +
  labs(x = "Year", y = "Sap Flux Density", title = "Sap Flux Density over mid Apr-May 2022-24")

#Plots for abiotic factors 
#Soil volumetric water content at 15 cm: 

full_data%>%
  filter(Plot == "Control") %>%
  filter(hour(TIMESTAMP) <= 12, hour(TIMESTAMP) >= 11, 
         F <= 17500, F >= 0, 
         soil_vwc_15cm >=0, soil_vwc_15cm <= 0.7) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  group_by(Date, Species) %>%
  drop_na(PAR, TEMP) %>%
  mutate (avg_f = mean(F), 
          avg_vwc = mean(soil_vwc_15cm), 
          avg_par = mean(PAR), 
          avg_temp = mean(TEMP)) -> ab_data

ab_data%>%
  ggplot(aes(x = avg_vwc, y = avg_f, 
             #color = Plot, group = Plot)) + 
             color = Species, group = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  #facet_wrap(~Plot, scale = "free") +
  facet_wrap(~Species, scale = "fixed") +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.05, label.y = 0.99) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.3, label.y = 0.985) +
  labs(x = "Average Volumetric Soil WC at 15 cm", 
       y = "Average Sap Flux Density", 
       title = "Soil VWC vs Sap Flux Density for Control Plot, 11 AM - 12 PM")
# I think it's safe to say these variables do not have a linear correlation 

#Photosynthetically active radiation: 

ab_data %>%
  ggplot(aes(x = avg_par, y = avg_f, 
             #color = Plot, group = Plot)) + 
             color = Species, group = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm") + 
  facet_wrap(~Species, scale = "fixed") +
  #facet_wrap(~Plot, scale = "free") +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.05, label.y = 0.99) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.3, label.y = 0.985) +
  labs(x = "Average Photosynthetically Active Radiation (PAR) (umol/m^2/s)", 
       y = "Average Sap Flux Density (m^3/s)", 
       title = "PAR vs Sap Flux Density for Control Plot, 11 AM - 12 PM")  

#Air temperature: 

ab_data %>%
  ggplot(aes(x = avg_temp, y = avg_f, 
             #color = Plot, group = Plot)) + 
             color = Species, group = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm") + ylim(0, NA) +
  facet_wrap(~Species, scale = "fixed") +
  #facet_wrap(~Plot, scale = "free") +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.05, label.y = 0.99) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.3, label.y = 0.985) +
  labs(x = "Average Air Temperature (C)", 
       y = "Average Sap Flux Density (m^3/s)", 
       title = "Air Temp vs Sap Flux Density for Control Plot, 11 AM - 12 PM") 

