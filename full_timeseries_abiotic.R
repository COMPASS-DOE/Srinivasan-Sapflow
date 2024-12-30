#This code compiles a complete time series of sapflow data and GcREW data from 2021-2024
#Sapflow, soil vwc at 15 cm, average air temp over 15 mins, photosynthetically active radiation
#Note: only sapflow and soilvwc data are available for 2021

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#TEMPEST data from 2022-24
site <- "TMP"
variables <- c("sapflow_2.5cm", "soil_vwc_15cm", "soil_EC_15cm")

pat <- paste0("^", site, ".*csv$")

#Lists of data for different years for TEMPEST
files_T24 <- list.files("C:/Users/srin662/Documents/R/TMP_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_T23 <- list.files("C:/Users/srin662/Documents/R/TMP_2023/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_T22 <- list.files("C:/Users/srin662/Documents/R/TMP_2022/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_T21 <- list.files("C:/Users/srin662/Documents/R/TMP_2021/", pattern = pat, recursive = TRUE, full.names = TRUE)

files_T <- c(files_T24, files_T23, files_T22, files_T21)

f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables | x$Sensor_ID == "F19D",]
}

#Bind together all files 
dat <- lapply(files_T,  f)
dat <- do.call("rbind", dat)

tmp_full <- dat

#Correction for F19 being mislabeled as F19D in L1 data
tmp_full %>%
  drop_na(Sensor_ID) %>%
  mutate(Sensor_ID = ifelse(Sensor_ID == "F19D", "F19", Sensor_ID)) -> tmp_full

saveRDS(tmp_full, "tmp_full.rds")
tmp_full <- readRDS("tmp_full.rds")

#GCREW data from 2022-24
#Note: vappress is all 0 for now until we get that sorted out 
site <- "GCW"
variables <- c("wx_tempavg15", "wx_par_den15", "wx_vappress15", "soil_EC_15cm")

pat <- paste0("^", site, ".*csv$")

#Lists of data for different years for GCREW
files_G24 <- list.files("C:/Users/srin662/OneDrive - PNNL/R/GCW_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_G23 <- list.files("C:/Users/srin662/OneDrive - PNNL/R/GCW_2023/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_G22 <- list.files("C:/Users/srin662/OneDrive - PNNL/R/GCW_2022/", pattern = pat, recursive = TRUE, full.names = TRUE)

files_G <- c(files_G24, files_G23, files_G22)

f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables,]
}

#Bind together all files 
dat <- lapply(files_G,  f)
dat <- do.call("rbind", dat)

gcw_full <- dat
saveRDS(gcw_full, "gcw_full.rds")
gcw_full <- readRDS("gcw_full.rds")

#Combining it all: editing dataframes for variables to match 
species <- readRDS("dbh.rds")

tmp_full %>%
  mutate(Plot = substr(Plot,1,1),
         Plot = case_when(Plot == "C" ~ "Control",
                          Plot == "F" ~ "Freshwater",
                          Plot == "S" ~ "Saltwater", )) -> tmp_full
species %>%
  mutate(Species = substr(spp,1,4),
         Species = case_when(spp == "ACRU" ~ "Red Maple",
                             spp == "LITU" ~ "Tulip Poplar",
                             spp == "FAGR" ~ "Beech")) %>%
  select(Plot, Sapflux_ID, Species) %>%
  filter(!grepl("D", Sapflux_ID)) -> species

#Because different variables are at different spatial resolutions, we have to
#separate variables into dataframes then merge again by timestamp

#Create sapflow-only dataframe with scaled Fd
sapflow <- tmp_full %>% 
  filter(Instrument == "Sapflow",
         Value >= 0.01, Value <=0.7) %>%
  select(Plot, TIMESTAMP, Sensor_ID, Value) %>%
  mutate(sapflow_2.5cm = Value) %>% 
  mutate(Date = date(TIMESTAMP))

#Merge sapflow and species dataframe
sapflow_sp <- 
  merge(species, sapflow, by.x = c("Sapflux_ID", "Plot"), by.y = c("Sensor_ID", "Plot"), all.x = TRUE, all.y = TRUE)

mutate(sapflow_sp, ID = Sapflux_ID) -> sapflow_sp

#Calculate dTmax
sapflow_sp %>% 
  mutate(Date = date(TIMESTAMP)) %>%
  group_by(Date, Plot, Species, ID) %>% 
  summarise(dTmax = max(Value, na.rm = TRUE), 
            dTmax_time = TIMESTAMP[which.max(Value)])-> sapflow_dtmax

#Calculate Fd
# convert the probe raw values (in mV) to sap flux velocity (m/s)
# Granier equation is Fd = ((118 * 10^-6) * K)^1.231

sapflow_sp %>% 
  left_join(sapflow_dtmax, by = c("Plot", "Species", "ID", "Date")) %>% 
  mutate(Fd = ((0.00011899 * (((dTmax / Value) - 1)))^1.231))  -> sfd_data

#Load in dbh data (for scaling) 
inventory <- readRDS("dbh.rds")
inventory %>%
  select(Tree_ID, Sapflux_ID, spp, DBH_2024, DBH_2022, DBH_2023, DBH_2021) -> dbh


#Using allometric equations, scale Fd measurements
#DBH measurements are in cm; scaled to mm 

SA <- function(Species, DBH) {
  case_when(
    Species == "Red Maple" ~ (0.5973*(DBH/100)^2.0743),
    Species == "Tulip Poplar" ~ (0.8086*(DBH/100)^1.8331),
    Species == "Beech" ~ (0.8198*(DBH/100)^1.8635))
}

dbh %>%
  mutate(Species = spp) %>%
  mutate(Species = substr(Species,1,4),
         Species = case_when(Species == "ACRU" ~ "Red Maple",
                             Species == "LITU" ~ "Tulip Poplar",
                             Species == "FAGR" ~ "Beech")) %>%
  mutate(across(starts_with("DBH_"), ~SA(Species, .), .names = "SA_{str_extract(.col, '[0-9]{4}')}")) -> sa

sa %>% 
  pivot_longer(cols = starts_with("SA_"),
               names_to = "Year",
               names_prefix = "SA_",
               values_to = "SA") %>%
  mutate(Year = as.numeric(Year)) -> sa_long

mutate(sfd_data, Year = year(TIMESTAMP)) -> sfd_data

scaled <- merge(sfd_data, sa_long, by.x = c("ID", "Year", "Species"), 
                by.y = c("Sapflux_ID", "Year", "Species"), all.x = TRUE)

scaled %>%
  select(ID, Year, Species, Plot, TIMESTAMP, Fd, SA) %>%
  mutate(F = SA * Fd) -> sf_scaled

#Now let's make some plots to double check 
#Filtering out outliers F<17500

sf_scaled %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  mutate(monthyr = floor_date(TIMESTAMP, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(F <= 2e-06, F >= 0) %>%
  group_by(Plot, Species, Date) %>% 
  summarise(F_avg = mean(F, na.rm = TRUE)) -> sf_plot_avg

ggplot(sf_plot_avg) + 
  geom_point(aes (x = Date, y = F_avg, color = Species)) + 
  #geom_errorbar(aes(ymin = F_avg - F_error, ymax = F_avg + F_error,
                    #x = Month, color = Species)) + 
  facet_wrap(~Plot, ncol = 1, scales = "fixed") + 
  labs(y = "Avg Sap Flux Density", x = "Date", title = "Sap Flux Density Averaged Daily, 11 AM - 12 PM")
  
  ggsave("Full_sapflow.jpeg")

#Let's also save this new complete sap flux data as an rds:
  saveRDS(scaled, "Sapflow_21_24.rds")

#Now we add in our abiotic data
  #Create soil vwc dataframe
  #Take average value of all soil vwc measurements in each plot 
swc_15 <- tmp_full %>%
    filter(research_name == "soil_vwc_15cm") %>%
    group_by(TIMESTAMP, Plot) %>%
    drop_na(Value) %>%
    summarize(soil_vwc_15cm = mean(Value)) 

tmp_data <- 
  left_join(sf_scaled, swc_15, by = c("Plot", "TIMESTAMP"))  


#Now use gcrew data 
#Note: only freshwater (wetland) will have these variables, but we can extrapolate to other plots
#All vapor pressure values are currently 0, so filter out for now
#Note: first few months of 2022 don't have PAR or temp values, look into this later 

gcw_full %>%
  mutate(Plot = substr(Plot,1,2),
         Plot = case_when(Plot == "W" ~ "Freshwater",)) %>%
  select(Plot, TIMESTAMP, Value, research_name) %>%
  filter(research_name != "wx_vappress15") -> gcw


gcw %>%
  filter(research_name == "wx_par_den15") %>%
  mutate(PAR = Value) %>% 
  select(TIMESTAMP, PAR) -> par

gcw %>%
  filter(research_name == "wx_tempavg15") %>%
  mutate(TEMP = Value) %>% 
  select(TIMESTAMP, TEMP) -> temp

gcw %>%
  filter(research_name == "soil_EC_15cm") %>%
  mutate(EC = Value) %>%
  select(TIMESTAMP, EC) -> ec

full_data <- 
  merge(tmp_data, par, by.x = c("TIMESTAMP"), 
        by.y = c("TIMESTAMP"), all = TRUE)

full_data <- 
  merge(full_data, temp, by.x = c("TIMESTAMP"), 
        by.y = c("TIMESTAMP"), all.x = TRUE) 

full_data <- 
  merge(full_data, ec, by.x = c("TIMESTAMP"), 
        by.y = c("TIMESTAMP"), all.x = TRUE) 

#Now we have a full time series for 2021-2024!

saveRDS(full_data,"Full_21_24.rds")


