

library(dplyr)
library(lubridate)
library(ggplot2)
library(neonUtilities)

#
##Get par data from SERC's NEON tower
#Trying to 
zipsByProduct(dpID = "DP1.00024.001", site = "SERC",
              startdate = "2021-06", enddate = "2021-06") #just June for now
zipsByProduct(dpID = "DP1.00024.001", site = "SERC",
              startdate = "2025-06", enddate = "2025-06",
              include.provisional = TRUE)

# Unzipping all downloaded files
zip_files <- c("filesToStack00024/NEON.D02.SERC.DP1.00024.001.2021-06.basic.20230127T120753Z.RELEASE-2025.zip",
               "filesToStack00024/NEON.D02.SERC.DP1.00024.001.2025-06.basic.20250702T202948Z.PROVISIONAL.zip")
output_dir <- "par_data"

# Unzip each file into the specified directory
for (file in zip_files) {
  unzip(zipfile = file, exdir = output_dir)
}

# Combine all unzipped files into a single dataframe, extract 30 min average
PAR_stacked <- stackByTable(filepath = "par_data")

##
##
##
#Start here if stacked data already exsist
PAR_data <- read.csv("par_data/stackedFiles/PARPAR_30min.csv")

# Fix timestamps in PAR_data
PAR_data <- PAR_data %>%
  mutate(
    startDateTime = if_else(
      str_detect(startDateTime, "^\\d{4}-\\d{2}-\\d{2}$"),   # Check if timestamp is only in "YYYY-MM-DD" format
      paste0(startDateTime, " 00:00:00"),                    # Add "00:00:00" for midnight
      startDateTime                                          # Keep as is for full timestamps
    ),
    startDateTime = ymd_hms(startDateTime)                   # Convert to POSIXct format
  )

#
#here you need the RDS dataframe from the main markdown
#data <- readRDS("Sapflow_BACI.rds")

# Join NEON data with COMPASS-FME data
matched_data <- data %>%
  filter(month(TIMESTAMP) == 6) %>%
  left_join(
    PAR_data %>% dplyr::select(startDateTime, PARMean, PARMinimum, PARMaximum),
    by = c("TIMESTAMP" = "startDateTime"))  # Match using TIMESTAMP & startDateTime

# Check to make sure the PAR data is similar to PAR data from the GCREW met station
# this should create roughly a 1:1 line
matched_data %>%
  filter(Year != 2021) %>% #no PAR data for GCREW in 2021
  dplyr::select(TIMESTAMP, PAR, PARMean) %>%
  distinct() %>%
  ggplot(aes(PAR, PARMean)) + geom_point()

#but it does not!

#TODO figure out why it does not
#TODO once that is resolved, the following code can be used to compare
#the response of sapflow to par

# find days with low PAR
low_PARdays <- PAR_data %>%
  mutate(
    Hour = hour(startDateTime),
    Date = as_date(startDateTime),
    Year = year(startDateTime)) %>%
  group_by(Date) %>%
  filter(max(PARMean) < 900) %>%
  dplyr::select(Date) %>%
  distinct(.)

#dataset with PAR from NEON towers (line 52 above)
matched_data %>%
dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`,
              ec_avg, swc_avg, TEMP, PARMean) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  filter(!Date %in% low_PARdays$Date, #remove low PAR days
         Hour <= 14, Hour >= 11, #same window/filtering as for main markdown
         F <= 0.005, F > 0) -> clean_data

# Hourly plot sapflow averages for each species
clean_data %>%
  mutate(Year = as.factor(Year),
         ID = as.factor(ID),
         Plot = as.factor(Plot)) %>%
  filter(! Year %in% c(2022, 2023, 2024),
         Species %in% c("Beech", "Tulip Poplar", "Red Maple")) %>%
  group_by(Date, Hour, Plot, Species, Year) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE)) -> F_avg

# Hourly PAR average for each day
clean_data %>%
  mutate(Year = as.factor(Year)) %>%
  filter(! Year %in% c(2022, 2023, 2024)) %>%
  dplyr::select(Date, Hour, Year, PARMean) %>%
  group_by(Date, Hour, Year) %>%
  summarise(par_avg = mean(PARMean, na.rm = TRUE)) -> par_avg

# combine
combined <- full_join(par_avg, F_avg)

# Regression plot
ggplot(combined, aes(x = par_avg, y = F_avg, color = Year)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group = interaction(Species, Plot, Year))) +
  facet_wrap(~ Species + Plot, scales = "free") +  # Facet by Species and Plot
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Linear Regression Between Sapflow (F) and PAR Mean",
    x = "Photosynthetically Active Radiation (PARMean)",
    y = "Sapflow (F)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))
