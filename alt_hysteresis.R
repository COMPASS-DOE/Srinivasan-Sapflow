
.packages = c("tidyr", "ggplot2", "dplyr", "lme4",
              "AICcmodavg", "MuMIn", "pbkrtest", "readr",
              "lubridate", "car", "parallel", "data.table",
              "blmeco", "lsmeans", "patchwork", "viridis",
              "forcats", "ggpmisc", "stringr", "e1071",
              "ggpubr", "ggstance", "zoo", "purr")


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
  distinct() %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         doy = yday(Date),
         Year = year(Date),
         Month = month(Date),
         Hour = hour(TIMESTAMP)) %>%
  mutate( es = 0.6108 * exp((17.27 * TEMP) / (TEMP + 237.3)),  
          ea = es * RH / 100, 
          VPD_calc = es - ea ) %>% #Calculate VPD
  group_by(Year) %>%
  filter(doy > 99 & doy < 301) %>%
  ungroup() -> clean_A_data

not_these_days <- clean_A_data %>%
  group_by(Date) %>%
  mutate(rainfall = sum(precip)) %>%
  filter(max(PAR) < 600 |
         rainfall > 0.5 |
           VPD_calc >= 2.5) %>%
  dplyr::select(Date) %>%
  distinct()

sapflow %>%
  distinct() %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         doy = yday(Date),
         Year = year(Date),
         Month = month(Date),
         Hour = hour(TIMESTAMP)) %>%
  group_by(Year) %>%
  filter(doy > 99 & doy < 301,
         F <= 0.005, F >= 0) %>%
  ungroup() -> clean_S_data

hysteresis_A_data <- clean_A_data %>%
  filter(!is.na(VPD_calc),
         !Date %in% not_these_days$Date) 

hysteresis_S_data <- clean_S_data %>%
  filter(!is.na(`F`),
         !is.na(Species),
         !Date %in% not_these_days$Date) 

hysteresis_data <- left_join(hysteresis_A_data, hysteresis_S_data,
                             relationship = "many-to-many")

rm(abiotic_final)
rm(sapflow)

hysteresis_data %>%
  filter(ID == "F14",
         week(Date) == 23) %>%
  ggplot(aes(x = VPD_calc, y = `F`,
             color = Hour)) +
  geom_path(linewidth = 1.2) +
  facet_wrap(as.factor(Year) ~ Date) +
  scale_color_viridis_c(name = "Hour") +
  labs(x = "VPD_calculated", 
       y = "Sapflow", 
       title = "F14") +
  theme_minimal()

smoothed <- hysteresis_data %>%
  arrange(ID, Date, desc(TIMESTAMP)) %>%
  group_by(ID, Date) %>%
  mutate(rolled_sap = rollmean(`F`, k=4, align = "left", fill = NA),
         rolled_vpd = rollmean(VPD_calc, k=4, align = "left", fill = NA)) %>%
  ungroup() %>%
  filter(Hour >= 5, Hour <= 21,
         ! ID %in% c("C8", "C12", "S9"))

smoothed %>%
  filter(ID == "F14",
         week(Date) == 23) %>%
  ggplot(aes(x = rolled_vpd, y = rolled_sap,
             color = Hour)) +
  geom_path(linewidth = 1.2) +
  facet_wrap(as.factor(Year) ~ Date) +
  scale_color_viridis_c(name = "Hour") +
  labs(x = "VPD_calculated", 
       y = "Sapflow", 
       title = "F14_smooth") +
  theme_minimal()

#messy ACRU's
# C8, C12, S9

#messy LITU's
#none

#messy FAGR's
#none

ready2plot <- smoothed %>%
  left_join(events, by = "Year") %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         BA = factor(BA, levels = c("Before", "After")),
         clock_time = format(smoothed$TIMESTAMP, "%H:%M"))

ready2plot %>%
  group_by(Year, BA, Species, ID, Plot) %>%
  summarise(n = length(Date)) -> r2p_summary

ready2plot %>%
  group_by(Year, BA, Species, Plot, clock_time) %>%
  mutate(vpd_iqr_threshold = IQR(VPD_calc),
         sapflow_iqr_threshold = IQR(`F`)) %>%
  filter(VPD_calc >= quantile(VPD_calc, 0.25, na.rm = TRUE) - 1.5 * vpd_iqr_threshold,
         VPD_calc <= quantile(VPD_calc, 0.75, na.rm = TRUE) + 1.5 * vpd_iqr_threshold,
         `F` >= quantile(`F`, 0.25, na.rm = TRUE) - 1.5 * sapflow_iqr_threshold,
         `F` <= quantile(`F`, 0.75, na.rm = TRUE) + 1.5 * sapflow_iqr_threshold) %>%
  ungroup() -> ready2plot_filtered

#too few days for '22 and '23 when just +/- 2 weeks is isolated
# ready2plot %>%
#   filter(Date > flood_start - window &
#            Date < flood_end + window) %>%
#   ungroup() -> r2p_short

species_plot_avg <- ready2plot %>%
  ungroup() %>%
  group_by(Year, BA, Plot, Species, clock_time) %>%
  summarise(
    mean_vpd = mean(VPD_calc),
    mean_sapflow = mean(`F`))

#Plot for each species
ready2plot %>%
  filter(Species == "Red Maple",
         Year != 2025) %>%
  ggplot() + geom_path(aes(x = rolled_vpd,
                           y = rolled_sap,
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
    x = "VPD",
    y = "Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) -> acru
ggsave("acru.jpg")

ready2plot %>%
  filter(Species == "Beech",
         Year != 2025) %>%
  ggplot() + geom_path(aes(x = rolled_vpd,
                           y = rolled_sap,
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
    x = "VPD",
    y = "Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) -> fagr
ggsave("fagr.jpg")

ready2plot %>%
  filter(Species == "Tulip Poplar",
         Year != 2025) %>%
  ggplot() + geom_path(aes(x = rolled_vpd,
                           y = rolled_sap,
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
    x = "VPD",
    y = "Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")) -> litu
ggsave("litu.jpg")




ready2plot_filtered %>%
  filter(Year %in% c(2022, 2025),
         BA == "Before",
         Plot != "Freshwater") %>%
  ggplot() + geom_path(aes(x = VPD_calc,
                           y = `F`,
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


# Define species and years for automation
species_list <- c("Tulip Poplar", "Red Maple", "Beech")
years <- 2022:2025

# Function to create and save plots
save_hysteresis_plots <- function(species, year) {
  # Generate the plot
  plot <- ready2plot_filtered %>%
    filter(Year == year, Species == species) %>%
    group_by(Date, clock_time, Plot) %>%
    summarise(
      mean_vpd = mean(VPD_calc, na.rm = TRUE),
      mean_sapflow = mean(`F`, na.rm = TRUE),
      .groups = "drop"  # Avoid warning in summarise when grouping is dropped
    ) %>%
    ggplot() +
    geom_path(aes(x = mean_vpd, y = mean_sapflow, color = Plot), linewidth = 1.2) +
    facet_wrap(~ Date) +
    scale_color_viridis_d(option = "viridis", begin = 0.3, end = 0.8, name = "Plot") +
    labs(
      title = paste(species, "-", year),
      x = "VPD",
      y = "Sapflow"
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



ready2plot %>%
  filter(Year == 2024, Species == "Tulip Poplar",
         Month %in% c(9, 10)) %>%
  group_by(Date, clock_time, Plot) %>%
  summarise(
    mean_vpd = mean(VPD_calc, na.rm = TRUE),
    mean_sapflow = mean(`F`, na.rm = TRUE),
    .groups = "drop"  # Avoid warning in summarise when grouping is dropped
  ) %>%
  ggplot() +
  geom_path(aes(x = mean_vpd, y = mean_sapflow, color = Plot), linewidth = 1.2) +
  facet_wrap(~ Date) +
  scale_color_viridis_d(option = "viridis", begin = 0.3, end = 0.8, name = "Plot") +
  labs(
    title = "Late '24 LITU",
    x = "VPD",
    y = "Sapflow"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 13, face = "bold"))

    