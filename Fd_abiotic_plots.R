#Plotting Fd against abiotic factors for April 24 - May 2 2024, 11 AM - 12 PM 

#Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#load in data
data <- readRDS("sapflow_abiotic_complete.rds")
species <- read.csv("TEMPEST_TreeChamberInstallation_11272023.csv")


DATA_BEGIN <- as_datetime("2024-04-24 00:00:00", tz = "EST")
#DATA_END is implied as end of dataset

data %>%
  mutate(Plot = substr(Plot,1,1),
         Plot = case_when(Plot == "C" ~ "Control",
                          Plot == "F" ~ "Freshwater",
                          Plot == "S" ~ "Saltwater", )) %>%
  filter(TIMESTAMP >= DATA_BEGIN) -> data

#create sapflow dataframe only 
sapflow <- data %>% 
  filter(Instrument == "Sapflow",
         Value >= 0.01, Value <=1, !grepl("D", Sensor_ID)) 

species %>%
  mutate(Plot = ifelse(Plot == "Seawater", "Saltwater", Plot)) -> species

sapflow_sp <- 
  merge(species, sapflow, by.x = c("ID", "Plot"), by.y = c("Sensor_ID", "Plot"), all.x = TRUE)

sapflow_sp %>%
  mutate(Date = date(TIMESTAMP)) %>%
  drop_na(Value) %>%
  select(TIMESTAMP, ID, Species, Plot, Value, Grid.Cell, Date) -> sapflow_sp

sapflow_sp %>% 
  group_by(Date, Plot, Species, ID) %>% 
  summarise(dTmax = max(Value, na.rm = TRUE), 
            dTmax_time = TIMESTAMP[which.max(Value)])-> sapflow_dtmax

sapflow_sp %>% 
  left_join(sapflow_dtmax, by = c("Date", "Plot", "Species", "ID")) %>% 
  mutate(Fd = 360000 * (0.00011899) * (((dTmax / Value) - 1)^1.231)) %>%
  drop_na(Plot) -> sfd_data

inventory <- readRDS("dbh.rds")

inventory %>%
  select(Tag, Tree_Code, Species, 
         #DBH_2019, DBH_2020, DBH_2021, DBH_2022, DBH_2023, 
         DBH_2024) -> dbh

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

swc_15 <- data %>%
  filter(research_name == "soil_vwc_15cm") %>%
  mutate(soil_vwc_15cm = Value) %>%
  select(TIMESTAMP, Location, Plot, soil_vwc_15cm) %>%
  drop_na(soil_vwc_15cm)

#merge sapflow, species, and swc data 
tmp_data <- 
  merge(sapflow_scaled, swc_15, by.x = c("Grid.Cell", "TIMESTAMP", "Plot"), 
        by.y = c("Location", "TIMESTAMP", "Plot"), all.y = TRUE)
drop_na(tmp_data, Value) -> tmp_data

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
#It looks like we only have these measurements for freshwater
#not sure if these variables can be extrapolated to the other two plots? For now we'll just focus on freshwater

#It also looks like vapor pressure deficit only has 0 values, so let's ignore that as well: 
gcw %>%
  filter(research_name != "wx_vappress15") -> gcw

gcw %>%
  filter(research_name == "wx_par_den15") %>%
  mutate(PAR = Value) %>% 
  select(Plot, TIMESTAMP, PAR) -> par

gcw %>%
  filter(research_name == "wx_tempavg15") %>%
  mutate(TEMP = Value) %>% 
  select(Plot, TIMESTAMP, TEMP) -> temp

full_data <- 
  merge(tmp_data, par, by.x = c("TIMESTAMP", "Plot"), 
        by.y = c("TIMESTAMP", "Plot"), all.x = TRUE)

full_data <- 
  merge(full_data, temp, by.x = c("TIMESTAMP", "Plot"), 
        by.y = c("TIMESTAMP", "Plot"), all.x = TRUE) 
#--------------
#Now we can make some plots! 

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


