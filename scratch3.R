
# First chunk tests BACI across the 3 by 3
# three species and three treatment plots
# using only 2021 (before) and 2025 (after) data
# currently we only have sapflow data through June of '25

library(ggstance) #for dodging points later on
library(neonUtilities) #for PAR data

zipsByProduct(dpID = "DP1.00024.001", site = "SERC",
              startdate = "2021-06", enddate = "2021-06")
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
unzip(zipfile = file, exdir = output_dir, overwrite = FALSE)

# Combine all unzipped files into a single dataframe, extract 30 min average
PAR_stacked <- stackByTable(filepath = "par_data")
PAR_data <- read.csv("par_data/stackedFiles/PARPAR_30min.csv")

#filter
low_PARdays <- PAR_data %>%
  mutate(
    startDateTime = ymd_hms(endDateTime),
    Date = as_date(startDateTime),
    Year = year(startDateTime),
    Hour = hour(startDateTime)) %>%
  group_by(Date) %>%
  filter(!max(PARMean) < 800) %>%
  distinct() %>%
  dplyr::select(Date)

data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`,
                ec_avg, swc_avg, TEMP, PAR) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  group_by(Year) %>%
  filter(month(Date) == 6, #just June
         Hour <= 15, Hour >= 11, #same window as for main markdown
         F <= 0.006, F > 0) %>% #same filtering as for main markdown
  ungroup() -> clean_data

clean_data %>%
  group_by(Year) %>%
  mutate(Year = as.factor(Year),
         ID = as.factor(ID),
         Plot = as.factor(Plot)) %>%
  group_by(Date, Plot, Species, ID, Year) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            soil_ec_avg = mean(ec_avg), 
            soil_vwc_avg = mean(swc_avg),
            par_avg = mean(PAR),
            temp_avg = mean(TEMP),
            n = n()) %>%
  ungroup() -> cd_1

cd_1 %>%
  filter(! Year %in% c(2022, 2023, 2024),
         Species %in% c("Beech", "Tulip Poplar", "Red Maple")) -> cd_2

#base model, here Year is acting as "BA", and we're allowing all levels of plot and species to interact
#random effect of ID allows for different trees to have different intercepts
model_3way <- lmer(F_avg ~ Year*Plot*Species + (1|ID),
                   data = cd_2)
#calculate estimated marginal means
model_emm <- emmeans(model_3way, ~ Plot|Species*Year)
emm_3way <- as.data.frame(model_emm)
#shows pairwise tests of 2021 vs 2025 data for each species x plot group
contrasts <- contrast(model_emm, interaction = "pairwise", by = c("Species", "Plot"))
summary(contrasts)

#setting a width for offsets below
dodge_width <- 0.25

#plot of 21 vs 25 EMM with 95% CI
ggplot(emm_3way, aes(x = Year, y = emmean, color = Plot)) +
  facet_grid(~ Species) +
  geom_point(size = 3, position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,  
    position = position_dodge(width = dodge_width)) +
  geom_line(
    aes(group = interaction(Species, Plot)),
    position = position_dodge(width = dodge_width),
    linetype = "dashed") +
  theme_minimal() +
  labs(
    y = "Estimated Marginal Mean (Response)",
    x = "June Midday Sapflow",
    title = "Estimated Marginal Means, 2021 vs 2025 Sapflow") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))


#similar visual for the long-term

library(zoo) #to calculate rolling means

data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`, Fd, PAR) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         PAR > 800,
         F <= 0.005, F > 0) %>%
  ungroup() -> sapflow

sapflow %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  group_by(Date, Plot, Year, Species, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  distinct() -> sapflow_1

sapflow_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) %>%
  mutate(BA = as.character(BA)) %>%
  na.omit() -> sapflow_2

# Option 1, 'normalized' to Control plot
sapflow_2_long <- sapflow_2 %>%
  group_by(Year, Day, BA, Plot, Species) %>% 
  summarize(Species_avg = mean(F_avg, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = Plot, values_from = Species_avg) %>% 
  mutate(Freshwater_Control = Freshwater - Control,
         Saltwater_Control = Saltwater - Control) %>%
  pivot_longer(cols = starts_with(c("Freshwater_Control", "Saltwater_Control")),
               names_to = "Plot_Normalized",
               values_to = "Normalized_avg")

# Compute rolling mean
sapflow_rolling <- sapflow_2_long %>%
  group_by(Species, Plot_Normalized, Year) %>%
  arrange(Day) %>%
  mutate(Rolling_avg = rollmean(Normalized_avg, k = 3, fill = NA, align = "left")) %>%
  ungroup()

# Graph both datasets
sapflow_2_long %>%
  ggplot(aes(x = Day, y = Normalized_avg, color = Species, shape = BA)) +
  geom_point(size = 2.5, alpha = 0.45) +
  facet_grid(Plot_Normalized ~ Year) +
  geom_hline(yintercept = 0, color = 'red') +
  geom_line(data = sapflow_rolling,
            aes(x = Day, y = Rolling_avg, color = Species),
            linewidth = 1.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Normalized Sapflow with Rolling Average (3 Days)",
       x = "Flood Day (or stand-in) +/- Two Weeks",
       y = "Species Avg, Trt - Control") +
  theme_minimal()

# Option 2, raw NOT 'normalized'

# Compute rolling mean
sapflow_rolling2 <- sapflow_2 %>%
  group_by(Species, Plot, Year) %>%
  arrange(Day) %>%
  mutate(Rolling_avg = rollmean(F_avg, k = 3, fill = NA, align = "left")) %>%
  ungroup()

# Step 1: Calculate the 2021 average sapflow (F_avg) for each species
species_avg_2021 <- sapflow_2 %>%
  filter(Year == "2021") %>%         
  group_by(Species, Plot) %>%              
  summarize(F_avg_mean = mean(F_avg, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Add calculated hlines to the plot
sapflow_2 %>%
  ggplot(aes(x = Day, y = F_avg, color = Plot, shape = BA)) +
  geom_point(size = 2.5, alpha = 0.45) +
  facet_grid(Species ~ Year) +
  geom_line(data = sapflow_rolling2, 
            aes(x = Day, y = Rolling_avg, color = Plot), 
            linewidth = 1.25) +
  geom_hline(data = species_avg_2021, 
             aes(yintercept = F_avg_mean, color = Plot), 
             linetype = "dashed", 
             linewidth = 0.8) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Sapflow with Rolling Average (3 Days) and 2021 Average Lines",
       x = "Flood Day (or stand-in) +/- Two Weeks",
       y = "Species and Plot Avg") +
  theme_minimal()

