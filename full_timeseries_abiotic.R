library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#TEMPEST data from 2022-24
site <- "TMP"
variables <- c("sapflow_2.5cm", "soil_vwc_15cm")

pat <- paste0("^", site, ".*csv$")

#Lists of data for different years for TEMPEST
files_T24 <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/TMP_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_T23 <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/TMP_2023/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_T22 <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/TMP_2022/", pattern = pat, recursive = TRUE, full.names = TRUE)

files_T <- c(files_T24, files_T23, files_T22)

f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables,]
}

#Bind together all files 
dat <- lapply(files_T,  f)
dat <- do.call("rbind", dat)

tmp_full <- dat

#GCREW data from 2022-24
#Note: vappress is all 0 for now until we get that sorted out 
site <- "GCW"
variables <- c("wx_tempavg15", "wx_par_den15", "wx_vappress15")

pat <- paste0("^", site, ".*csv$")

#Lists of data for different years for GCREW
files_G24 <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/GCW_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_G23 <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/GCW_2023/", pattern = pat, recursive = TRUE, full.names = TRUE)
files_G22 <- list.files("C:/Users/srin662/OneDrive - PNNL/Documents/R/GCW_2022/", pattern = pat, recursive = TRUE, full.names = TRUE)

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

#Combining it all: editing dataframes for variables to match 
species <- read.csv("TEMPEST_TreeChamberInstallation_11272023.csv")

tmp_full %>%
  mutate(Plot = substr(Plot,1,1),
         Plot = case_when(Plot == "C" ~ "Control",
                          Plot == "F" ~ "Freshwater",
                          Plot == "S" ~ "Saltwater", )) -> tmp_full
species %>%
  mutate(Plot = ifelse(Plot == "Seawater", "Saltwater", Plot)) %>%
  select(Plot, ID, Species) -> species

#Because different variables are at different spatial resolutions, we have to
#separate variables into dataframes then merge again by timestamp

#Create sapflow-only dataframe with scaled Fd
sapflow <- tmp_full %>% 
  filter(research_name == "sapflow_2.5cm",
         Value >= 0.01, Value <=1) %>%
  select(Plot, TIMESTAMP, Sensor_ID, Value) %>%
  mutate(sapflow_2.5cm = Value) %>% 
  mutate(Date = date(TIMESTAMP))

#Merge sapflow and species dataframe
sapflow_sp <- 
  merge(species, sapflow, by.x = c("ID", "Plot"), by.y = c("Sensor_ID", "Plot"), all.y = TRUE)

#Calculate dTmax
sapflow_sp %>% 
  mutate(Date = date(TIMESTAMP)) %>%
  group_by(Date, Plot, Species, ID) %>% 
  summarise(dTmax = max(Value, na.rm = TRUE), 
            dTmax_time = TIMESTAMP[which.max(Value)])-> sapflow_dtmax

#Calculate Fd
sapflow_sp %>% 
  left_join(sapflow_dtmax, by = c("Date", "Plot", "Species", "ID")) %>% 
  mutate(Fd = 360000 * (0.00011899) * (((dTmax / Value) - 1)^1.231)) -> sfd_data

#Load in dbh data (for scaling) 
inventory <- readRDS("dbh.rds")
inventory %>%
  select(Tag, Tree_Code, Species, DBH_2024, DBH_2022, DBH_2023) -> dbh


#Using allometric equations, scale Fd measurements

SA <- function(Species, DBH) {
  case_when(
    Species == "Red Maple" ~ (0.5973*(DBH)^2.0743),
    Species == "Tulip Poplar" ~ (0.8086*(DBH)^1.8331 ),
    Species == "Beech" ~ (0.8198*(DBH)^1.8635))
}

dbh <- dbh %>%
  mutate(Species = substr(Species,1,4),
         Species = case_when(Species == "ACRU" ~ "Red Maple",
                             Species == "LITU" ~ "Tulip Poplar",
                             Species == "FAGR" ~ "Beech")) %>%
  mutate(across(starts_with("DBH_"), ~SA(Species, .), .names = "SA_{str_extract(.col, '[0-9]{4}')}"))

scaled <- merge(sfd_data, dbh, by.x = c("ID", "Species"), by.y = c("Tree_Code", "Species"), all.x = TRUE)



