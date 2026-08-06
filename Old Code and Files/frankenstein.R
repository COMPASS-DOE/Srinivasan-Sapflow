
# ---- Packages ----
library(readr)      # for read_rds in legacy lines; not strictly required
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(arrow)
# library(compasstools)  # optional, if you want nearest_neighbor_TMP(), not used below

# ---- Paths ----
L2_DATA_LOCATION <- file.path(getwd(), "Data", "Level2")
stopifnot(dir.exists(L2_DATA_LOCATION))

# ---- Robust reader with schema bridge ----
# Adds `research_name = <variable>` so old filters keep working.
# Leaves Instrument, Sensor_ID, Plot, TIMESTAMP, Value, etc., intact.
read_site_variable <- function(site, variable, where = L2_DATA_LOCATION) {
  # Escape for regex safety
  site_rx <- str_replace_all(site, "([\\^$.|?*+()\\[\\]{}])", "\\\\\\1")
  var_rx  <- str_replace_all(variable, "([\\^$.|?*+()\\[\\]{}])", "\\\\\\1")
  pat <- paste0("^", site_rx, "_.*", var_rx, ".*\\.parquet$")
  
  files <- list.files(path = where, pattern = pat, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) stop(sprintf("No parquet files for site='%s' variable='%s'. Pattern: %s", site, variable, pat))
  
  purrr::map_dfr(files, function(f) {
    message("Reading ", basename(f))
    df <- read_parquet(f)
    # TIMESTAMP safety (arrow usually preserves it; coerce if needed)
    if (!inherits(df$TIMESTAMP, "POSIXct")) {
      df <- mutate(df, TIMESTAMP = as.POSIXct(TIMESTAMP, tz = "UTC"))
    }
    # Bridge column for legacy filters
    df <- mutate(df, research_name = variable)
    # Ensure Value is numeric (guard against character)
    if (!is.numeric(df$Value)) {
      df <- mutate(df, Value = suppressWarnings(as.numeric(Value)))
    }
    df
  })
}

# ---- Utility: filter QC if present ----
filter_qc0_if_present <- function(df, flags = c("F_OOB", "F_OOS")) {
  for (fl in flags) {
    if (fl %in% names(df)) df <- dplyr::filter(df, .data[[fl]] == 0)
  }
  df
}

# ============================================================
# 1) TMP variables (sapflow, soil-vwc-15cm, soil-EC-15cm)
# ============================================================
variables_TMP <- c("sapflow-3.5cm", "soil-vwc-15cm", "soil-EC-15cm")

tmp_full <- purrr::map_dfr(
  variables_TMP,
  ~ read_site_variable(site = "TMP", variable = .x, where = L2_DATA_LOCATION)
)

# Correction for F19 being mislabeled as F19D in L1 data (keep, conservative)
tmp_full <- tmp_full %>%
  drop_na(Sensor_ID) %>%
  mutate(Sensor_ID = if_else(Sensor_ID == "F19D", "F19", Sensor_ID))

# Plot labels to match your analysis
tmp2_full <- tmp_full %>%
  mutate(
    Plot = substr(Plot, 1, 1),
    Plot = case_when(
      Plot == "C" ~ "Control",
      Plot == "F" ~ "Freshwater",
      Plot == "S" ~ "Saltwater",
      TRUE ~ Plot
    )
  )

# ============================================================
# 2) GCW weather variables (wx-tempavg15, wx-par-den15)
# ============================================================
variables_GCW <- c("wx-tempavg15", "wx-par-den15")

gcw_full <- purrr::map_dfr(
  variables_GCW,
  ~ read_site_variable(site = "GCW", variable = .x, where = L2_DATA_LOCATION)
)

# Map GCW plot label (legacy code used "W" -> Freshwater)
gcw <- gcw_full %>%
  mutate(
    Plot = substr(Plot, 1, 2),
    Plot = case_when(Plot == "W" ~ "Freshwater", TRUE ~ Plot)
  ) %>%
  dplyr::select(Plot, TIMESTAMP, Value, research_name)

# Split GCW into PAR and TEMP
par <- gcw %>%
  filter(research_name == "wx-par-den15") %>%
  mutate(PAR = Value) %>%
  dplyr::select(TIMESTAMP, PAR)

temp <- gcw %>%
  filter(research_name == "wx-tempavg15") %>%
  mutate(TEMP = Value) %>%
  dplyr::select(TIMESTAMP, TEMP)

# ============================================================
# 3) Tree inventory (unchanged)
# ============================================================
# Must exist in your project root; same fields as before.
tree_dat <- readRDS("inventory.rds")

species <- tree_dat %>%
  mutate(Species = substr(spp, 1, 4),
         Species = case_when(
           spp == "ACRU" ~ "Red Maple",
           spp == "LITU" ~ "Tulip Poplar",
           spp == "FAGR" ~ "Beech"
         )) %>%
  dplyr::select(Plot, Sapflux_ID, Species)

# ============================================================
# 4) Sapflow-only dataframe, dTmax, Granier F, sapwood scaling
# ============================================================
sapflow <- tmp2_full %>%
  # Either by Instrument or by research_name; keep both for robustness
  filter(
    research_name == "sapflow-3.5cm" | Instrument == "Sapflow"
  ) %>%
  filter_qc0_if_present(flags = c("F_OOB", "F_OOS")) %>%
  dplyr::select(Plot, TIMESTAMP, Sensor_ID, Value) %>%
  mutate(raw = Value, Date = date(TIMESTAMP))

# Merge sapflow with species by plot + sensor ID
sapflow_sp <- merge(
  species, sapflow,
  by.x = c("Sapflux_ID", "Plot"),
  by.y = c("Sensor_ID", "Plot"),
  all.x = TRUE, all.y = TRUE
) %>%
  mutate(ID = Sapflux_ID)

# dTmax from nightly window
sapflow_dtmax <- sapflow_sp %>%
  mutate(Date = date(TIMESTAMP),
         Hour = hour(TIMESTAMP)) %>%
  filter(Hour >= 0, Hour <= 4) %>%
  group_by(Date, Plot, Species, ID) %>%
  summarise(dTmax = max(Value, na.rm = TRUE),
            dTmax_time = TIMESTAMP[which.max(Value)],
            .groups = "drop")

# Granier F (cm3/cm2/s)
sf_data <- sapflow_sp %>%
  left_join(sapflow_dtmax, by = c("Plot", "Species", "ID", "Date")) %>%
  mutate(F = 0.011899 * (((dTmax / Value) - 1)^1.231))

# DBH & sapwood area (SA)
dbh <- tree_dat %>%
  dplyr::select(Tree_ID, Sapflux_ID, spp,
                DBH_2024, DBH_2023, DBH_2022, DBH_2021)

SA <- function(Species, DBH) {
  case_when(
    Species == "Red Maple"   ~ (0.5973 * (DBH)^2.0743),
    Species == "Tulip Poplar"~ (0.8086 * (DBH)^1.8331),
    Species == "Beech"       ~ (0.8198 * (DBH)^1.8635)
  )
}

sa <- dbh %>%
  mutate(Species = spp) %>%
  mutate(Species = substr(Species, 1, 4),
         Species = case_when(
           Species == "ACRU" ~ "Red Maple",
           Species == "LITU" ~ "Tulip Poplar",
           Species == "FAGR" ~ "Beech")) %>%
  mutate(across(starts_with("DBH_"), ~ SA(Species, .),
                .names = "SA_{str_extract(.col, '[0-9]{4}')}"))

sa_long <- sa %>%
  pivot_longer(cols = starts_with("SA_"),
               names_to = "Year",
               names_prefix = "SA_",
               values_to = "SA") %>%
  mutate(Year = as.numeric(Year))

sf_data <- mutate(sf_data, Year = year(TIMESTAMP))

scaled <- merge(sf_data, sa_long,
                by.x = c("ID", "Year", "Species"),
                by.y = c("Sapflux_ID", "Year", "Species"),
                all.x = TRUE)

sf_scaled <- scaled %>%
  dplyr::select(ID, Year, Species, Plot, TIMESTAMP, F, SA) %>%
  mutate(Fd = SA * F)  # cubic meters per second if SA in m2; adjust units if needed

# ============================================================
# 5) Soil VWC & EC plot averages (with QC if present)
# ============================================================
swc_15raw <- tmp2_full %>% filter(research_name == "soil-vwc-15cm")
ec_15raw  <- tmp2_full %>% filter(research_name == "soil-EC-15cm")

swc_15 <- swc_15raw %>%
  filter_qc0_if_present(flags = c("F_OOB", "F_OOS")) %>%
  drop_na(Value) %>%
  group_by(TIMESTAMP, Plot) %>%
  mutate(swc_avg = mean(Value), swc_sd = sd(Value)) %>%
  ungroup() %>%
  arrange(TIMESTAMP, Plot)

ec_15 <- ec_15raw %>%
  filter_qc0_if_present(flags = c("F_OOB", "F_OOS")) %>%
  drop_na(Value) %>%
  group_by(TIMESTAMP, Plot) %>%
  mutate(ec_avg = mean(Value), ec_sd = sd(Value)) %>%
  ungroup() %>%
  arrange(TIMESTAMP, Plot)

swc_15clean <- swc_15 %>%
  dplyr::select(swc_avg, swc_sd, Plot, TIMESTAMP) %>%
  distinct()

ec_15clean <- ec_15 %>%
  dplyr::select(ec_avg, ec_sd, Plot, TIMESTAMP) %>%
  distinct()

abiotic_data <- left_join(ec_15clean, swc_15clean, by = c("Plot", "TIMESTAMP"))

final_tmp_data <- left_join(abiotic_data, sf_scaled, by = c("Plot", "TIMESTAMP"))

# ============================================================
# 6) Add weather (TEMP, PAR) and save
# ============================================================
abiotic_weather <- merge(temp, par, by = c("TIMESTAMP"), all = TRUE)
final_data <- merge(final_tmp_data, abiotic_weather, by = c("TIMESTAMP"), all.x = TRUE)

saveRDS(final_data, "Sapflow_BACI.rds")
