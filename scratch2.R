
library(dplyr)
library(tidyr)
library(zoo)

# Compute rolling mean
sapflow_rolling <- sapflow_2_long %>%
  group_by(Species, Plot_Normalized, Year) %>%  # Group by Species, Plot, and Year
  arrange(Day) %>%                 # Ensure data is in order by Date
  mutate(Rolling_avg = rollmean(Normalized_avg, k = 4, fill = NA, align = "left")) %>%  # Compute rolling mean
  ungroup()

# Summarize, normalize, and convert to long-form using previous steps
sapflow_2_long <- sapflow_2 %>%
  group_by(Year, Day, BA, Plot, Species) %>% 
  summarize(Species_avg = mean(F_avg, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = Plot, values_from = Species_avg) %>% 
  mutate(Freshwater_Control = Freshwater - Control,
         Saltwater_Control = Saltwater - Control) %>%
  pivot_longer(cols = starts_with(c("Freshwater_Control", "Saltwater_Control")),
               names_to = "Plot_Normalized",
               values_to = "Normalized_avg")

sapflow_2_long %>%
  ggplot(aes(x = Day, y = Normalized_avg, color = Species, shape = BA)) +  # aes goes inside ggplot
  geom_point(size = 2.5) +  # Points layer
  facet_grid(Plot_Normalized ~ Year) +  # Facet by normalized plot and year
  geom_hline(yintercept = 0, color = 'red') +  # Horizontal reference line
  geom_line(data = sapflow_rolling, aes(x = Day, y = Rolling_avg, color = Species)) +  # Line layer for rolling mean
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Normalized Sapflow with Rolling Average (4 Days)",
       x = "Flood Day (or stand-in) +/- Two Weeks",
       y = "Species Avg, Trt - Control") +  # Labels for the plot
  theme_minimal()  # Minimal theme

data %>% 
  dplyr::select(TIMESTAMP, ec_avg, ec_sd, Plot, Year) %>%
  mutate(Hour = hour(TIMESTAMP),
         Date = date(TIMESTAMP)) %>%
  filter(Hour == 11) %>%
  distinct() -> ec_data


ec_data %>%
  group_by(Plot) %>%
  arrange(Date) %>%
  mutate(ec_roll = rollmean(ec_avg, k = 4, fill = NA, align = "left")) %>%
  # above computes rolling mean
  ungroup() ->ec_rollmean

ec_data %>%
  group_by(Plot) %>%
  sample_frac(0.2) -> ec_sub

ec_sub %>%
ggplot(aes(x = Date, y = ec_avg, color = Plot, group = Plot)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = ec_avg-ec_sd, ymax = ec_avg+ec_sd,
                    color = Plot, group = Plot), alpha = 0.2) +
  geom_line(data = ec_rollmean, aes(x = Date, y = ec_roll,
                             color = Plot, group = Plot)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()


ec_sub %>%
  ggplot(aes(x = Date, y = log(ec_avg+10), color = Plot, group = Plot)) +
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = log(ec_avg-ec_sd+10), ymax = log(ec_avg+ec_sd+10),
                    color = Plot, group = Plot), alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()




