
# This code compiles a complete time series
# of sapflow and related abiotic data from TEMPEST for 2021-2025
# Sapflow, soil EC & vwc at 15 cm, average air temp, & PAR

# To run, user must download ad unzip sources files from COMPASS-FME Level 1 data
# For sapflow, soil VWC, & EC: doi:10.15485/2479200
# For Tair & PAR: doi:10.15485/2439400

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(arrow)

# Change for your setup
L2_DATA_LOCATION <- "/Users/d3x290/Code/data-workflows/pipeline/data/L2"

read_site_variable <- function(site, variable, where = L2_DATA_LOCATION) {
  files <- list.files(path = where,
                      pattern = paste0(site, "_.*(", variable, ").*parquet$"), 
                      recursive = TRUE, 
                      full.names = TRUE)
  
  lapply(files, function(f) {
    message("Reading ", basename(f))
    read_parquet(f)
  }) %>% bind_rows() 
}

# TEMPEST data from 2021-25
read_site_variable("TMP", "sapflow-3.5cm") %>% 
  select(-research_name, -Instrument, -Value_MAC, -Instrument_ID) %>%
  rename(sapflow = Value) %>% 
  # Correction for F19 being mislabeled as F19D in L1 data
  drop_na(Sensor_ID) %>%
  mutate(Sensor_ID = if_else(Sensor_ID == "F19D", "F19", Sensor_ID)) -> 
  sapflow

# Soil data
vwc <- read_site_variable("TMP", "soil-vwc-15cm")
ec <- read_site_variable("TMP", "soil-EC-15cm")

# Weather data
wx_temp <- read_site_variable("GCW", "wx-tempavg15") %>% 
  select(TIMESTAMP, TEMP = Value)
wx_par <- read_site_variable("GCW", "wx-par-den15") %>% 
  select(TIMESTAMP, PAR = Value)

# Join weather with sapflow
message("Joining weather and sapflow data")
sapflow %>% 
  left_join(wx_temp, by = "TIMESTAMP") %>% 
  left_join(wx_par, by = "TIMESTAMP") ->
  sapflow_wx

# At this point we have raw sapflow and the weather data in a single data
# frame (although weather data start only in April 2022)

# Soil VWC and EC are still in their own data frames
# You could use compasstools::nearest_neighbor_TMP() to get local 
# soil conditions for each tree, or compute plot averages and join

