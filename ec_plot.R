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
         month(Date) == 4,
         day(Date) >= 15, day(Date) <= 30,  # Last two weeks of April
         Hour <= 14, Hour >= 11,
         F <= 0.005, F >= 0
         ) %>%
  group_by(Date, Plot, Species, ID) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n()) %>%
  ungroup() -> anova

simple <- aov(F_avg ~ Species*Plot, data = anova)
emm <- emmeans(simple, ~ Plot|Species)
pairwise_comparisons <- pairs(emm, adjust = "tukey")
summary(pairwise_comparisons)

# Extract estimated marginal means
emm_data <- as.data.frame(emm)

ggplot(emm_data, aes(x = Plot, y = emmean, color = Species, group = Species)) +
  geom_point(size = 3) +  # Points for estimated marginal means
  geom_line(linewidth = 1) +   # Lines to connect species across plots
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25) +  # Error bars
  labs(title = "Historical Recreation of Radha's Initial Anova",
       x = "Plot",
       y = "Estimated Marginal Mean of F_avg") +
  theme_minimal() + scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`, Fd,
                `soil-vwc-15cm`, `soil-ec-15cm`) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  filter(Year == 2025,
         month(Date) == 6,
         Hour <= 14, Hour >= 11,
         F <= 0.005, F >= 0
  ) %>%
  group_by(Date, Plot, Species, ID) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n()) %>%
  ungroup() -> anova2

simple2 <- aov(F_avg ~ Species*Plot, data = anova2)
emm2 <- emmeans(simple2, ~ Plot|Species)
pairwise_comparisons2 <- pairs(emm2, adjust = "tukey")
summary(pairwise_comparisons2)

# Extract estimated marginal means
emm_data2 <- as.data.frame(emm2)

ggplot(emm_data2, aes(x = Plot, y = emmean, color = Species, group = Species)) +
  geom_point(size = 3) +  # Points for estimated marginal means
  geom_line(linewidth = 1) +   # Lines to connect species across plots
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.25) +  # Error bars
  labs(title = "Interaction Between Species and Plot",
       x = "Plot",
       y = "Estimated Marginal Mean of F_avg") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
