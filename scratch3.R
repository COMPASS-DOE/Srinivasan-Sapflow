

model.int.ec.alt <- lmer(sqrt(F_avg) ~ BA*Plot*Year + soil_ec_avg + (1|ID:Year),
                     data = tsb_2)
emmeans(model.int.ec.alt, specs = pairwise ~ BA*Plot|Year)

data %>% 
  mutate(Hour = hour(TIMESTAMP)) %>%
  mutate(Date = date(TIMESTAMP)) %>%
  mutate(monthyr = floor_date(TIMESTAMP, unit = "week")) %>%
  filter(Hour >= 11, Hour <= 12) %>% 
  filter(`F` <= 2, `F` >= 0) %>%
  group_by(Plot, Species, Date) %>% 
  summarise(F_avg = mean(`F`, na.rm = TRUE)) -> sf_plot_avg

data %>%
  mutate(Hour = hour(TIMESTAMP),
         Date = date(TIMESTAMP),
         Year = year(TIMESTAMP)) %>%
  filter(Hour >= 11, Hour <= 12,
         `F` <= 2.25, `F` >= 0,
         month(Date) > 5 & month(Date) < 8,
         PAR > 400) %>%
  ungroup() %>% group_by(Plot, Species, Year) %>%
  summarize(F_avg = mean(`F`, na.rm = TRUE)) -> species_plot_avg

species_plot_avg %>%
  #filter(Species == "Tulip Poplar") %>%
  pivot_wider(names_from = c("Year"), values_from = F_avg) -> yearly_averages

data %>%
  mutate(Hour = hour(TIMESTAMP),
    Date = date(TIMESTAMP),
    Year = year(TIMESTAMP)) %>%
  filter(Hour >= 11, Hour <= 12,
         `F` <= 2.25, `F` >= 0,
         month(Date) > 5 & month(Date) < 8,
         PAR > 400) %>%
  ungroup() %>% group_by(Plot, Species, Year) %>%
ggplot() + 
  geom_boxplot(aes (x = Plot, y = `F`, fill = Plot)) + 
  facet_wrap(Species~Year, scales = "fixed") + 
  labs(y = "Avg Sap Flux Density", x = "Date",
       title = "Sap Flux Density Averaged Daily, 11 AM - 12 PM")

               