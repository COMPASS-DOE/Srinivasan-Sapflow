#Sapflow Initial Analysis
#Radha Srinivasan (Based on Stephanie Pennington's sapflow.rmd script)
#2024-06-18

#Conversion of sapflow voltage difference to sap flux density (Fd) via Granier (1985) equation
#Average values of Fd for last two weeks of April 2024 from 9-10 AM calculated and plotted
#2-way ANOVA test for statistical significance between treatments and plots conducted

#load in packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#load in sapflow data (already bound into single csv from 
#create-time-series.R script) 

readRDS("sapflow.rds") -> sapflow_data

#start and end times for data
DATA_BEGIN <- as_datetime("2024-04-17 00:00:00", tz = "EST")
DATA_END <- as_datetime("2024-04-30 23:59:59", tz = "EST")

#These values are from 2022's TEMPEST day 1 event
#Edit if needed to highlight any specific timeframe of data
#EVENT_START <- as_datetime("2022-06-22 05:30:00", tz = "EST")
#EVENT_STOP <- as_datetime("2022-06-22 14:30:00", tz = "EST")

#Tidy up data
#(Steph you're a genius for grepl I was doing it the long way!!)
sapflow_data <- sapflow_data %>% 
  mutate(Value = as.numeric(Value),
         Date = date(TIMESTAMP),
         Hour = hour(TIMESTAMP)) %>% 
  filter(TIMESTAMP >= DATA_BEGIN, TIMESTAMP <= DATA_END, 
         Value > 0.02, Value <2, !grepl("D", ID)) 

#Add species column to sapflow_data
#Create new dataframe 
sapflow_data <- merge(species, sapflow_data, by.x = "ID", by.y = "Sensor_ID", all.x = TRUE)

sapflow_data %>% 
  drop_na(ID) %>%
  select(TIMESTAMP, Date, Hour, ID, Species, Plot.y, Location, Value) -> sf_dat

#Ignore this lol
  #mutate(plot = substr(ID,1,1),
         #plot = case_when(plot == "C" ~ "Control",
                          #plot == "S" ~ "Saltwater",
                          #plot == "F" ~ "Freshwater")) -> sf_dat

#From Steph: For dTmax, we're using the max sapflow method, which needs to meet the following criteria:
# (1) Be between the hours of midnight and 5am
# (2) Be the maximum sapflow value

#Filter accordingly: 
sf_dat %>% 
  filter(Hour >= 0, Hour <= 5) %>% 
  group_by(Date, Plot.y, Species, ID) %>% 
  summarise(dTmax = max(Value, na.rm = TRUE), dTmax_Timestamp = TIMESTAMP[which.max(Value)]) -> sapflow_dtmax

#Graph sapflow data with dtmax calculation to double check correct calculation: 
sf_dat %>% 
  filter(Plot.y == "C") %>% 
  ggplot(aes(x = TIMESTAMP, y = Value, group = Species, color= as.factor(Species))) + 
  geom_line() + 
  geom_point(data = filter(sapflow_dtmax, Plot.y == "C"), aes(x = dTmax_Timestamp, y = dTmax), color = "black") +
  facet_wrap(ID~Plot.y, scales = "free") 
#dTmax values are on the peaks of the daily values so looks good!

#Granier 1985 equation + convert to g/m^2/hour
#Fd is sap flux density 
sf_dat %>% 
  left_join(sapflow_dtmax, by = c("Date", "Plot.y", "Species", "ID")) %>% 
  mutate(Fd = 360000 * (0.00011899) * (((dTmax / Value) - 1)^1.231)) %>%
  drop_na(Plot.y) -> sfd_data

#Graph to double check:
ggplot(data = filter(sfd_data), aes(x = TIMESTAMP, y = (Fd), group = Species, color = as.factor(Species))) + 
  geom_line() + 
  geom_point(data = sapflow_dtmax, aes(x = dTmax_Timestamp, y = dTmax), color = "black") +
  facet_wrap(~Plot.y, ncol = 1) 
#Looks good! Because voltage difference and flow rate are inversely proportional, dTmax should be at the valleys of Fd.

#Hours 9-10 AM for last two weeks of April averaged by plot and species
#Note: This is an average every 15 minutes from 9-10 AM, if you just wanted an average for the hour as a whole lmk!
sfd_data %>% 
  filter(Hour <=10, Hour >= 9) %>% 
  group_by(Plot.y, Species, TIMESTAMP) %>% 
  summarise(Fd_avg = mean(Fd, na.rm = TRUE)) %>% 
  mutate(Fd_avg = round(Fd_avg, digits = 3)) -> sfd_plot_avg

ggplot(sfd_plot_avg) + 
  geom_line(aes (x = TIMESTAMP, y = Fd_avg, color = Species)) + 
  facet_wrap(~Plot.y, ncol = 1) +
  scale_x_continuous(breaks = pretty(sfd_data$TIMESTAMP, n = 10))

#ANOVA test to determine if difference in Fd of different treatments and species are statistically significant) 

sapflow_aov <- aov(Fd ~ Species + Plot.y, data = sfd_data)

summary(sapflow_aov)
#The p-value is <<0.05. 
#The differences in Fd between species and treatments are statistically significant. 
