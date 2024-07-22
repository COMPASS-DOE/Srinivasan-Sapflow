#(Hopefully) finalized graphs for paper & presentation 
#Using TEMPEST and GCREW data from 2021-2024:


library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpmisc)

#Import all data
full_data <- readRDS("Full_21_24.rds")

#Store storm events 
Ida_start <- as_date("2021-08-26")
Ida_end <- as_date("2021-09-01")
Elsa_start <- as_date("2021-06-30")
Elsa_end <- as_date("2021-07-09")

#Pre and post-TEMPEST weeks for reference: 
weeks_start <- c("2022-06-15", "2022-06-23", "2023-05-30", "2023-06-08", "2021-06-06", "2021-06-13")
weeks_end <- c("2022-06-21", "2022-06-29", "2023-06-05", "2023-06-14", "2021-06-12", "2021-06-19")
weeks_start<- as_date(weeks_start)
weeks_end<- as_date(weeks_end)

#Pre2022 <- June 15th - June 21
#Post2022 <- June 23rd - 29th
#Pre2023 <- May 30 - June 5th
#Post2023 <- June 8- June 14th
#2021 <- June 6 - June 12, June 13 - June 19

#Full phenology of sap flux density
#Years 2021-24, averaged by week, hrs 11-12

full_data %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  mutate(Week = floor_date(TIMESTAMP, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(F <= 17500, F > 0) %>%
  group_by(Plot, Species, Date) %>% 
  summarise(F_avg = mean(F, na.rm = FALSE), 
            F_error = sd(F, na.rm = FALSE)) %>%
  mutate(F_avg = round(F_avg, digits = 3)) -> sf_plot_avg

ggplot(sf_plot_avg) + 
  geom_point(aes (x = Date, y = F_avg, color = Species)) + 
  #geom_errorbar(aes(ymin = F_avg - F_error, ymax = F_avg + F_error,
  #x = Date, color = Species)) + 
  facet_wrap(~Plot, ncol = 1, scales = "fixed") + 
  annotate(geom = "rect", xmin=Ida_start, xmax=Ida_end, ymin= -Inf, ymax=Inf, alpha=0.6, fill="lightblue") +
  annotate(geom = "rect", xmin=Elsa_start, xmax=Elsa_end, ymin= -Inf, ymax=Inf, alpha=0.6, fill="lightblue") +
  labs(y = expression(Average~Sap~Flux~Density~(m^3/s)), x = "Date", title = "Sap Flux Density Averaged Daily, 11 AM - 12 PM")

ggsave("Daily_Sapflow.jpeg")

#Average air temperature vs average sap flux density
#Years 2021-24, Weeks Pre and Post TEMPEST, hrs 11-12
#Note: temp data not available until March 2022

full_data %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  filter(Plot == "Control") %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(Date >= weeks_start & Date <= weeks_end) %>%
  filter(F <= 17500, F > 0) %>%
  group_by(Species, TIMESTAMP) %>% 
  summarise(F_avg = mean(F, na.rm = FALSE), 
            temp_avg = mean(TEMP, na.rm = FALSE)) %>%
  mutate(F_avg = round(F_avg, digits = 3)) -> temp_plot_avg

temp_plot_avg %>%
  ggplot(aes(x = temp_avg, y = F_avg, 
             color = Species)) +
  geom_point() +
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


#Average PAR vs average sap flux density
#Years 2021-24, Weeks Pre and Post TEMPEST, hrs 11-12
#Note: PAR data not available until March 2022

full_data %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  filter(Plot == "Control") %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(Date >= weeks_start & Date <= weeks_end) %>%
  filter(F <= 17500, F > 0) %>%
  group_by(Species, TIMESTAMP) %>% 
  summarise(F_avg = mean(F, na.rm = FALSE), 
            par_avg = mean(PAR, na.rm = FALSE)) %>%
  mutate(F_avg = round(F_avg, digits = 3)) -> par_plot_avg

par_plot_avg %>%
  ggplot(aes(x = par_avg, y = F_avg, 
             color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") + ylim(0, NA) +
  facet_wrap(~Species, scale = "fixed") +
  #facet_wrap(~Plot, scale = "free") +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.05, label.y = 0.99) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.3, label.y = 0.985) +
  labs(x = "Average PAR", 
       y = "Average Sap Flux Density (m^3/s)", 
       title = "PAR vs Sap Flux Density for Control Plot, 11 AM - 12 PM")

#Average soil volumetric water content vs average sap flux density
#Years 2021-24, Weeks Pre and Post TEMPEST, hrs 11-12

full_data %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  filter(Plot == "Control") %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(Date >= weeks_start & Date <= weeks_end) %>%
  filter(F <= 17500, F > 0) %>%
  group_by(Species, TIMESTAMP) %>% 
  summarise(F_avg = mean(F, na.rm = FALSE), 
            vwc_avg = mean(soil_vwc_15cm, na.rm = FALSE)) %>%
  mutate(F_avg = round(F_avg, digits = 3)) -> vwc_plot_avg

vwc_plot_avg %>%
  ggplot(aes(x = vwc_avg, y = F_avg, 
             color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") + ylim(0, NA) +
  facet_wrap(~Species, scale = "fixed") +
  #facet_wrap(~Plot, scale = "free") +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.05, label.y = 0.99) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, label.x = 0.3, label.y = 0.985) +
  labs(x = "Average Soil Volumetric Water Content", 
       y = "Average Sap Flux Density (m^3/s)", 
       title = "VWC vs Sap Flux Density for Control Plot, 11 AM - 12 PM")

#ANOVA test for difference in means between treatments, species, and their interaction 
#Fd ~ sp x treatment 
#Box plot of sap flux density faceted by plot and species
#Years 2022-24, Weeks pre and Post TEMPEST, hrs 11-12

full_data %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  filter(Hour >= 11, Hour <= 12) %>%
  filter(Date >= weeks_start & Date <= weeks_end) %>%
  filter(year(TIMESTAMP) != 2021,
         F <= 17500, F > 0) -> anova


full_aov <- aov(F ~ Species * Plot, data = anova )

summary(full_aov)

ggplot(anova) + 
  geom_boxplot(aes (x = Plot, y = F, fill = Species)) +
  labs(x = "Plot", 
       y = "Average Sap Flux Density (m^3/s)", 
       title = "Sap Flux Density per Plot, Pre and Post TEMPEST Weeks")
 theme_light()
 
 ggsave("anova_boxplot.jpeg")

