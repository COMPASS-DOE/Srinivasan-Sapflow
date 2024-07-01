#Plotting Fd against abiotic factors for April 24 - May 2 2024, 11 AM - 12 PM 

#Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

full_data <- readRDS("Full_042424_050224.rds")

full_data %>%
  select(TIMESTAMP, Plot, Grid.Cell, ID, Species, F_tot, soil_vwc_15cm, PAR, TEMP) %>%
  mutate(Hour = hour(TIMESTAMP)) %>%
  filter(Hour >= 11, Hour <= 12) %>%
  group_by(TIMESTAMP) -> full_data

#Plot of Fd vs soil vwc 
full_data %>%
  group_by(TIMESTAMP, Plot, Species, Grid.Cell) %>%
  summarize(avg_vwc = mean(soil_vwc_15cm), 
            avg_fd = mean(F_tot)) %>%
  filter(avg_vwc < 0.8) %>%
  ggplot(aes(x = avg_vwc, y = avg_fd, color = Species)) + 
  geom_jitter()

#Plot of Fd vs PAR
full_data %>%
  filter(Plot == "Freshwater") %>%
  group_by(TIMESTAMP, Species) %>% 
  drop_na(F_tot, PAR) %>%
  summarize(avg_par = mean(PAR), 
            avg_fd = mean(F_tot)) %>%
  ggplot(aes(x = avg_par, y = avg_fd, color = Species)) + 
  geom_jitter()


