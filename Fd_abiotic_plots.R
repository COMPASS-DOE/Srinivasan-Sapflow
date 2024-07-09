#Plotting Fd against abiotic factors for April 24 - May 2 2024, 11 AM - 12 PM 

#Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(ggpmisc)

#Reading in data from timeseries_abiotic.R 
#Note this is only April 24- May 2 2024 and doesn't include vapor pressure yet.
full_data <- readRDS("Full_042424_050224.rds")

full_data %>%
  mutate(Hour = hour(TIMESTAMP)) %>%
  filter(Hour >= 11, Hour <= 12) -> full_data

#Plot of Fd vs soil vwc 
full_data %>%
  group_by(TIMESTAMP, Plot, Species) %>%
  summarize(avg_vwc = mean(soil_vwc_15cm), 
            avg_fd = mean(F_tot)) %>%
  ggplot(aes(x = avg_vwc, y = avg_fd, 
             #color = Plot, group = Plot)) + 
             color = Species, group = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  #facet_wrap(~Species, scale = "free") +
  facet_wrap(~Plot, scale = "free") +
  stat_poly_eq(aes(label = paste(..adj.rr.label.., sep = "~~~")),
               label.x.npc = 'left', label.y.npc = 0.15, 
               formula = y ~ x, parse = TRUE) +
  stat_poly_eq(aes(label = paste(format(..p.value.label.., digits = 3))),
               label.x.npc = 'right', label.y.npc = 0.25, 
               formula = y ~ x, parse = TRUE, label.x = "right") +
  labs(x = "Average Volumetric Soil WC at 15 cm (m^3/m^3)", 
       y = "Average Sap Flux Density (m^3/m^2/s)") +
  theme_light()
#Pretty poor linear fit :((

#Plot of Fd vs PAR

full_data %>%
  group_by(TIMESTAMP, Species, Plot) %>% 
  summarize(avg_par = mean(PAR), 
            avg_fd = mean(F_tot)) %>%
ggplot(aes(x = avg_par, y = avg_fd, 
           color = Plot, group = Plot)) + 
           #color = Species, group = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm") + 
  facet_wrap(~Species, scale = "free") +
  #facet_wrap(~Plot, scale = "free") +
  stat_poly_eq(aes(label = paste(..adj.rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE, label.x = 0.1) +
  stat_poly_eq(aes(label = paste(format(..p.value.label.., digits = 3))),
              formula = y ~ x, parse = TRUE, label.x = 0.3) +
  labs(x = "Average Photosynthetically Active Radiaiton (PAR) (umol/m^2/s)", 
       y = "Average Sap Flux Density (m^3/m^2/s)") +
  theme_light()

#A linear model doesn't look great for this data, any other suggestions? 


#Fd vs temp (with Plot or Species faceted) 
full_data %>%
  group_by(TIMESTAMP, Species, Plot) %>% 
  summarize(avg_temp = mean(TEMP), 
            avg_fd = mean(F_tot)) %>%
  ggplot(aes(x = avg_temp, y = avg_fd, 
             #color = Species, group = Species)) +
             color = Plot, group = Plot)) + 
  geom_jitter() +
  #facet_wrap(~Plot, scale = "free") +
  facet_wrap(~Species, scale = "free") +
  geom_smooth(method = "lm") +
  stat_poly_eq(aes(label = paste(..adj.rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE, label.x = "left", label.y = "top") +
  stat_poly_eq(aes(label = paste(format(..p.value.label.., digits = 3))),
               formula = y ~ x, parse = TRUE, label.x = "center", label.y = "top") + 
  labs(x = "Average Temperature (C)", 
       y = "Average Sap Flux Density (m^3/m^2/s)") +
  theme_light()

#Save as image 
ggsave("Fd_temp_2024_spfacet.jpeg", scale = 2)

