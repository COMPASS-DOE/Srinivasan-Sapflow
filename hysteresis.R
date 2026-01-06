
.packages = c("tidyr", "ggplot2", "dplyr", "lme4",
              "AICcmodavg", "MuMIn", "pbkrtest", "readr",
              "lubridate", "car", "parallel", "data.table",
              "blmeco", "lsmeans", "patchwork", "viridis",
              "forcats", "ggpmisc", "stringr", "e1071",
              "ggpubr", "ggstance", "zoo")

# library(dplyr)
# library(ggplot2)
# library(ggpubr)
# library(viridis)
# library(purrr)

#Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

#Attach packages
sapply(.packages, require, character.only=TRUE)

abiotic_final <- readRDS("met_station.rds")
sapflow <- readRDS("Sapflow_21_25.rds")

#TEMPEST EVENTS
tempest_events <- bind_rows(
  tibble( Year = 2021, flood_start = "2021-06-12", flood_end = "2021-06-12"), #average date = June 12
  tibble( Year = 2022, flood_start = "2022-06-22", flood_end = "2022-06-22"), #June 22
  tibble( Year = 2023, flood_start = "2023-06-06", flood_end = "2023-06-07"), #June 6, 7
  tibble( Year = 2024, flood_start = "2024-06-11", flood_end = "2024-06-13"), #June 11, 12, 13
  tibble( Year = 2025, flood_start = "2025-06-12", flood_end = "2025-06-12")) #average date = June 12

tempest_events %>%
  mutate(flood_start = ymd(flood_start),
         flood_end = ymd(flood_end)) -> events

#window of data to look at for BACI analysis
window <- days(15)

abiotic_final %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         doy = yday(Date),
         Year = year(Date),
         Month = month(Date),
         Hour = hour(TIMESTAMP)) %>%
  group_by(Year) %>%
  filter(doy > 99 & doy < 301) %>%
  ungroup() -> clean_A_data

cloudy_days <- clean_A_data %>%
  group_by(Date) %>%
  filter(max(PAR) < 600) %>%
  dplyr::select(Date)

sapflow %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         doy = yday(Date),
         Year = year(Date),
         Month = month(Date),
         Hour = hour(TIMESTAMP)) %>%
  group_by(Year) %>%
  filter(doy > 99 & doy < 301,
         F <= 0.005, F >= 0) %>%
  ungroup() -> clean_S_data

#Calculate VPD
clean_A_data <- clean_A_data %>%
  mutate( es = 0.6108 * exp((17.27 * TEMP) / (TEMP + 237.3)),  
          ea = es * RH / 100, 
          VPD_calc = es - ea )

hysteresis_A_data <- clean_A_data %>%
  filter(!is.na(VPD_calc),
         !Date %in% cloudy_days$Date) 

hysteresis_S_data <- clean_S_data %>%
  filter(!is.na(`F`),
         !is.na(Species),
         !Date %in% cloudy_days$Date) 

hysteresis_data <- left_join(hysteresis_A_data, hysteresis_S_data,
                             relationship = "many-to-many")

filtered <- hysteresis_data %>%
  arrange(ID, Date, desc(TIMESTAMP)) %>%
  group_by(ID, Date) %>%
  mutate(rolled_sap = rollmean(`F`, k=3, align = "left", fill = NA),
         rolled_vpd = rollmean(VPD_calc, k=3, align = "left", fill = NA)) %>%
  ungroup() %>%
  filter(Hour >= 5, Hour <= 21,
         ! ID %in% c("C8", "C12", "S9"))


#messy ACRU's
# C8, C12, S9

#messy LITU's
#none

#messy FAGR's
#none

rescaled <- filtered %>%
  group_by(Year, Month, Date, doy, Hour, ID, Species, Plot) %>% 
  arrange(ID, Date, desc(Hour)) %>% 
  summarise(mean_sapflow = mean(rolled_sap),
            mean_VPD = mean(rolled_vpd)) %>%
  group_by(ID, Date) %>%
  mutate(
    relative_sapflow = scales::rescale(mean_sapflow, to = c(0, 1), na.rm = TRUE),
    relative_vpd = scales::rescale(mean_VPD, to = c(0, 1), na.rm = TRUE)) %>%
  ungroup() %>%
  na.omit() %>%
  left_join(events, by = "Year") %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         BA = factor(BA, levels = c("Before", "After")))

rescaled %>%
  filter(ID == "F14",
         doy == 113) %>%
  ggplot(aes(x = relative_vpd, y = relative_sapflow,
             color = Hour)) +
  geom_path(linewidth = 1.2) +
  facet_wrap(Year ~ Date) +
  scale_color_viridis_c(name = "Hour") +
  labs(x = "VPD (normalized)", 
       y = "Sapflow (normalized)", 
       title = "Example, F14 on Day 158") +
  theme_minimal()

rescaled %>%
  group_by(Year, BA, Species, ID, Plot) %>%
  summarise(n = length(Date)) -> rescaled_summary

rescaled %>%
  group_by(Year) %>%
  filter(Date > flood_start - window &
           Date < flood_end + window) %>%
  ungroup() %>%
  group_by(Year, BA, Date, Plot, Species) -> rescaled_short

species_plot_avg <- rescaled_short %>%
  group_by(Year, BA, Plot, Species, Hour) %>%
  summarise(
    mean_vpd = mean(relative_vpd, na.rm = TRUE),
    mean_sapflow = mean(relative_sapflow, na.rm = TRUE),
    .groups = "drop" )


rescaled_short %>%
  group_by(Year, Species, Plot, Hour) %>%
  mutate(vpd_iqr_threshold = IQR(relative_vpd, na.rm = TRUE),
         sapflow_iqr_threshold = IQR(relative_sapflow, na.rm = TRUE)) %>%
  filter(relative_vpd >= quantile(relative_vpd, 0.25, na.rm = TRUE) - 1.5 * vpd_iqr_threshold,
         relative_vpd <= quantile(relative_vpd, 0.75, na.rm = TRUE) + 1.5 * vpd_iqr_threshold,
         relative_sapflow >= quantile(relative_sapflow, 0.25, na.rm = TRUE) - 1.5 * sapflow_iqr_threshold,
         relative_sapflow <= quantile(relative_sapflow, 0.75, na.rm = TRUE) + 1.5 * sapflow_iqr_threshold) %>%
  ungroup() -> rescaled_short_filtered

rescaled_short_filtered %>%
  group_by(Year, BA, Plot, Species, Hour) %>%
  summarise(
    mean_vpd = mean(relative_vpd, na.rm = TRUE),
    mean_sapflow = mean(relative_sapflow, na.rm = TRUE),
    .groups = "drop"
  ) -> species_plot_avg


#Plot for each species
rescaled_short_filtered %>%
  filter(Species == "Red Maple",
         Year != 2025) %>%
  ggplot() + geom_path(aes(x = relative_vpd,
                           y = relative_sapflow,
                         group = interaction(ID, Date)),
                     color = "lightgray", linewidth = 0.3, alpha = 0.3) +
  geom_path(data = species_plot_avg %>%
              filter(Species == "Red Maple", Year != 2025),
            aes(x = mean_vpd, y = mean_sapflow,
                group = Plot, color = Plot), linewidth = 1.2) +
  facet_wrap(BA ~ Year) +
  scale_color_viridis_d(option = "inferno", , begin = 0.3,
                        end = 0.9, name = "Plot") +
  labs(
    title = "Red Maple",
    x = "Relative VPD",
    y = "Relative Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) -> acru
ggsave("acru.jpg")

rescaled_short_filtered %>%
  filter(Species == "Beech",
         Year != 2025) %>%
  ggplot() + geom_path(aes(x = relative_vpd,
                           y = relative_sapflow,
                           group = interaction(ID, Date)),
                       color = "lightgray", linewidth = 0.3, alpha = 0.3) +
  geom_path(data = species_plot_avg %>%
              filter(Species == "Beech", Year != 2025),
            aes(x = mean_vpd, y = mean_sapflow,
                group = Plot, color = Plot), linewidth = 1.2) +
  facet_wrap(BA ~ Year) +
  scale_color_viridis_d(option = "inferno", , begin = 0.3,
                        end = 0.9, name = "Plot") +
  labs(
    title = "American Beech",
    x = "Relative VPD",
    y = "Relative Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) -> fagr
ggsave("fagr.jpg")

rescaled_short_filtered %>%
  filter(Species == "Tulip Poplar",
         Year != 2025) %>%
  ggplot() + geom_path(aes(x = relative_vpd,
                           y = relative_sapflow,
                           group = interaction(ID, Date)),
                       color = "lightgray", linewidth = 0.3, alpha = 0.3) +
  geom_path(data = species_plot_avg %>%
              filter(Species == "Tulip Poplar", Year != 2025),
            aes(x = mean_vpd, y = mean_sapflow,
                group = Plot, color = Plot), linewidth = 1.2) +
  facet_wrap(BA ~ Year) +
  scale_color_viridis_d(option = "inferno", , begin = 0.3,
                        end = 0.9, name = "Plot") +
  labs(
    title = "Tulip Poplar",
    x = "Relative VPD",
    y = "Relative Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) -> litu
ggsave("litu.jpg")



rescaled_short_filtered %>%
  filter(Year %in% c(2022, 2025),
         BA == "Before",
         Plot != "Freshwater") %>%
  ggplot() + geom_path(aes(x = relative_vpd,
                           y = relative_sapflow,
                           group = interaction(ID, Date)),
                       color = "lightgray", linewidth = 0.3,
                       alpha = 0.3, linetype = "solid") +
  geom_path(data = species_plot_avg %>%
              filter(Year %in% c(2022, 2025),
                     BA == "Before",
                     Plot != "Freshwater"),
            aes(x = mean_vpd, y = mean_sapflow,
                group = Plot,
                color = Plot),
            linewidth = 1.2) +
  facet_wrap(Year ~ Species) +
  scale_color_viridis_d(option = "viridis", , begin = 0.3,
                        end = 0.8, name = "Plot") +
  labs(
    title = "Long-Term BACI",
    x = "Relative VPD",
    y = "Relative Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) 



rescaled_short_filtered %>%
  filter(Year == 2022,
         Species == "Tulip Poplar"
         ) %>%
  group_by(Date, Hour, Plot) %>%
  summarise(
    mean_vpd = mean(relative_vpd, na.rm = TRUE),
    mean_sapflow = mean(relative_sapflow, na.rm = TRUE)) %>%
  ggplot() + geom_path(aes(x = mean_vpd,
                           y = mean_sapflow,
                       color = Plot),
                       linewidth = 1.2) +
  facet_wrap(~ Date) +
  scale_color_viridis_d(option = "viridis", , begin = 0.3,
                        end = 0.8, name = "Plot") +
  labs(
    title = "Tulip Poplar",
    x = "Relative VPD",
    y = "Relative Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) 



# Define species and years for automation
species_list <- c("Tulip Poplar", "Red Maple", "Beech")
years <- 2022:2024

# Function to create and save plots
save_hysteresis_plots <- function(species, year) {
  # Generate the plot
  plot <- rescaled_short_filtered %>%
    filter(Year == year, Species == species) %>%
    group_by(Date, Hour, Plot) %>%
    summarise(
      mean_vpd = mean(relative_vpd, na.rm = TRUE),
      mean_sapflow = mean(relative_sapflow, na.rm = TRUE),
      .groups = "drop"  # Avoid warning in summarise when grouping is dropped
    ) %>%
    ggplot() +
    geom_path(aes(x = mean_vpd, y = mean_sapflow, color = Plot), linewidth = 1.2) +
    facet_wrap(~ Date) +
    scale_color_viridis_d(option = "viridis", begin = 0.3, end = 0.8, name = "Plot") +
    labs(
      title = paste(species, "-", year),
      x = "Relative VPD",
      y = "Relative Sapflow"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 13, face = "bold")
    )
  
  # Save the plot with a descriptive file name
  file_name <- paste0("Hysteresis_", species, "_Year_", year, ".png")
  ggsave(filename = file_name, plot = plot, dpi = 300, width = 14, height = 7)
  
  return(plot)  # Optional: Return the plot (useful for reviewing)
}

# Apply the function for all combinations of species and years
plots <- cross2(species_list, years) %>%
  map(~ save_hysteresis_plots(.x[[1]], .x[[2]]))
