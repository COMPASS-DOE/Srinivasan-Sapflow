library(emmeans)

data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`, Fd,
                `soil-vwc-15cm`, `soil-ec-15cm`) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Hour <= 14, Hour >= 11,
         F <= 0.005, F >= 0) %>%
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
  ungroup() -> sapflow_1

sapflow_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) %>%
  mutate(BA = as.character(BA)) %>%
  na.omit() -> sapflow_2 #%>%
  # mutate(BA = ifelse(Year == 2021, "Before", BA),
  #        BA = ifelse(Year == 2025, "After", BA)) -> tsb_2

ggplot(sapflow_2, aes(Day, F_avg, color = Species, shape = BA)) +
  geom_point(size = 2.5) + facet_grid(Year~Plot, scales = "free")

sapflow_2_long <- sapflow_2 %>%
  group_by(Year, Day, BA, Plot, Species) %>% 
  summarize(Species_avg = mean(F_avg, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = Plot, values_from = Species_avg) %>% 
  mutate(Freshwater_Control = Freshwater - Control,
         Saltwater_Control = Saltwater - Control) %>%
  pivot_longer(cols = starts_with(c("Freshwater_Control", "Saltwater_Control")),
               names_to = "Plot_Normalized",
               values_to = "Normalized_avg")

ggplot(sapflow_2_long, aes(Day, Normalized_avg, color = Species, shape = BA)) +
  geom_point(size = 2.5) + facet_grid(Plot_Normalized~Year) +
  geom_hline(yintercept = 0, color = 'red')



data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`, Fd,
                `soil-vwc-15cm`, `soil-ec-15cm`) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  filter(Year == 2024,
         month(Date) == 5,
         Hour <= 14, Hour >= 11,
         F <= 0.005, F >= 0) %>%
  group_by(Date, Plot, Species, ID) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n()) %>%
  ungroup() -> anova

simple <- aov(F_avg ~ Species*Plot, data = anova)
emm <- emmeans(simple, ~ Plot|Species)
pairwise_comparisons <- pairs(emm, adjust = "tukey")
summary(pairwise_comparisons)
