#Create time series with following variables: 
#sapflow, soil volumetric water content, PAR, VPD, air temperature
#From Jan-May 2024 (Timescale can be modified as needed)

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#Note: only sapflow and soil vwc are actually included in this dataset
site <- "TMP"
variables <- c("sapflow_2.5cm", "sapflow_5cm", 
               "soil_vwc_5cm", "soil_vwc_10cm", "soil_vwc_15cm", "soil_vwc_30cm", 
               "wx_tempavg15", "wx_par_den24", "wx_vpd15")

# Construct a "regular expression" to find the files we want: in this case,
# CSV files starting with the site code above
pat <- paste0("^", site, ".*csv$")

# Get the names of the files we need. Note this assumes that your
# working directory is the main directory of the L1 data
files <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/TMP_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)

#Function to read files and only include variables in the above vector
f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables,]
}

#Apply function and bind files
dat <- lapply(files, f)
dat <- do.call("rbind", dat)

#New dataframe
var_full <- dat

#saveRDS(var_full, file = "sapflow_abiotic_complete.rds")

#------ using that rds:
data <- readRDS("sapflow_abiotic_complete.rds")
species <- read.csv("TEMPEST_TreeChamberInstallation_11272023.csv")


DATA_BEGIN <- as_datetime("2024-04-24 00:00:00")
#DATA_END is implied as end of dataset

#Filter timestamp and edit plot names to match across datasets
data %>%
  mutate(Plot = substr(Plot,1,1),
         Plot = case_when(Plot == "C" ~ "Control",
                          Plot == "F" ~ "Freshwater",
                          Plot == "S" ~ "Saltwater", )) %>%
  filter(TIMESTAMP >= DATA_BEGIN)-> data

species %>%
  mutate(Plot = ifelse(Plot == "Seawater", "Saltwater", Plot)) -> species

#create sapflow dataframe only 
sapflow <- data %>% 
  filter(Instrument == "Sapflow",
         Value >= 0.01, Value <=1, !grepl("D", Sensor_ID)) 

#Merge species and sapflow dataframe
sapflow_sp <- 
  merge(species, sapflow, by.x = c("ID", "Plot"), by.y = c("Sensor_ID", "Plot"), all.x = TRUE)

#Filter to necessary columns 
sapflow_sp %>%
  mutate(Date = date(TIMESTAMP)) %>%
  drop_na(Value) %>%
  select(TIMESTAMP, ID, Species, Plot, Value, Grid.Cell, Date) -> sapflow_sp

#Calculate dTmax
sapflow_sp %>% 
  group_by(Date, Plot, Species, ID) %>% 
  summarise(dTmax = max(Value, na.rm = TRUE), 
            dTmax_time = TIMESTAMP[which.max(Value)])-> sapflow_dtmax

#Calculate Fd
sapflow_sp %>% 
  left_join(sapflow_dtmax, by = c("Date", "Plot", "Species", "ID")) %>% 
  mutate(Fd = 360000 * (0.00011899) * (((dTmax / Value) - 1)^1.231)) %>%
  drop_na(Plot) -> sfd_data

#-----
#Scale Fd measurements to tree dbh
inventory <- readRDS("dbh.rds")

#Filter (for now) to 2024
inventory %>%
  select(Tag, Tree_Code, Species, DBH_2024) -> dbh
         #DBH_2019, DBH_2020, DBH_2021, DBH_2022, DBH_2023, 


#Using allometric equations, scale Fd measurements
#NOte: C19, C8, and F12 are currently being problematic to scale. They'll be filtered out for now 
dbh %>%
  mutate(Species = substr(Species,1,4),
         Species = case_when(Species == "ACRU" ~ "Red Maple",
                             Species == "LITU" ~ "Tulip Poplar",
                             Species == "FAGR" ~ "Beech")) %>%
  mutate(SA_2024 = case_when(
    Species == "Red Maple" ~ (0.5973*(DBH_2024)^2.0743),
    Species == "Tulip Poplar" ~ (0.8086*(DBH_2024)^1.8331 ),
    Species == "Beech" ~ (0.8198*(DBH_2024)^1.8635)
  )) -> dbh

scaled <- merge(sfd_data, dbh, by.x = c("ID", "Species"), by.y = c("Tree_Code", "Species"), all.x = TRUE)

scaled %>%
  mutate(F_tot = Fd * SA_2024) -> sapflow_scaled
#---

#Create soil vwc dataframe
#Take average value of all soil vwc measurements in each plot 
swc_15 <- data %>%
  filter(research_name == "soil_vwc_15cm") %>%
  group_by(TIMESTAMP, Plot) %>%
  drop_na(Value) %>%
  summarize(soil_vwc_15cm = mean(Value)) 

#merge sapflow, species, and swc data 
tmp_data <- 
  merge(sapflow_scaled, swc_15, by.x = c("Plot", "TIMESTAMP"), 
        by.y = c("Plot", "TIMESTAMP"), all.x = TRUE)

tmp_data %>%
  drop_na(F_tot) %>%
  select(Plot, TIMESTAMP, ID, Species, F_tot, soil_vwc_15cm) -> tmp_data

#now import gcrew data 
library(readr)

site <- "GCW"
variables <- c("wx_tempavg15", "wx_par_den15", "wx_vappress15")

# Construct a "regular expression" to find the files we want: in this case,
# CSV files starting with the site code above
pat <- paste0("^", site, ".*csv$")

# Get the names of the files we need. Note this assumes that your
# working directory is the main directory of the L1 data
files <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/GCW_2024", pattern = pat, recursive = TRUE, full.names = TRUE)

#Function to read files and only include variables in the above vector
f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables,]
}

#Apply function and bind files
dat <- lapply(files, f)
dat <- do.call("rbind", dat)

#New dataframe
gcw <- dat

#Now we can parse through gcw and add the necessary variables 
gcw %>%
  mutate(Plot = substr(Plot,1,2),
         Plot = case_when(Plot == "UP" ~ "Control",
                          Plot == "W" ~ "Freshwater",
                          Plot == "TR" ~ "Saltwater", )) %>%
  filter(TIMESTAMP >= DATA_BEGIN) %>%
  select(Plot, TIMESTAMP, Value, research_name) -> gcw
#It looks like we only have these measurements for freshwater, but we can extrapolate these
#measurements to the other plots 

#It also looks like vapor pressure deficit only has 0 values, so let's ignore that for now: 
gcw %>%
  filter(research_name != "wx_vappress15") -> gcw

gcw %>%
  filter(research_name == "wx_par_den15") %>%
  mutate(PAR = Value) %>% 
  select(TIMESTAMP, PAR) -> par

gcw %>%
  filter(research_name == "wx_tempavg15") %>%
  mutate(TEMP = Value) %>% 
  select(TIMESTAMP, TEMP) -> temp

full_data <- 
  merge(tmp_data, par, by.x = c("TIMESTAMP"), 
        by.y = c("TIMESTAMP"), all.x = TRUE)

full_data <- 
  merge(full_data, temp, by.x = c("TIMESTAMP"), 
        by.y = c("TIMESTAMP"), all.x = TRUE) 
#Now we have a full time series for the last 2 weeks of 2024 data!

saveRDS(full_data,"Full_042424_050224.rds")