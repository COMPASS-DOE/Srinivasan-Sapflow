
data %>%
  dplyr::select(Year, TIMESTAMP, Plot,
                swc_avg, swc_sd, ec_avg, ec_sd) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  filter(Hour <= 14, Hour >= 11) %>%
  na.omit() %>%
  group_by(Year, Date, Plot) %>%
  summarise(soil_ec_avg = mean(ec_avg),
            ec_error = sd(ec_avg),
            soil_vwc_avg = mean(swc_avg),
            vwc_error = sd(swc_avg)) -> soil_data

ggplot(soil_data, aes(Date, soil_ec_avg, color = Plot)) +
  geom_point() + #facet_wrap(~Year, scales = "free") +
  geom_errorbar(aes(ymin = soil_ec_avg - ec_error, ymax = soil_ec_avg + ec_error))

ggplot(soil_data, aes(Date, soil_vwc_avg, color = Plot)) +
  geom_point() + #facet_wrap(~Year, scales = "free") +
  geom_errorbar(aes(ymin = soil_vwc_avg - vwc_error, ymax = soil_vwc_avg + vwc_error))

soil_data %>%
  filter(! Year %in% c(2022, 2023, 2024),
         month(Date) == 6) -> soil_BACI_data

#base model, here Year is acting as "BA", and we're allowing all levels of plot and species to interact
ec_2way <- aov(soil_ec_avg ~ Year*Plot,
                   data = soil_BACI_data)
#calculate estimated marginal means
ec_emm <- emmeans(ec_2way, ~ Plot|Year)
emm_2way <- as.data.frame(ec_emm)
#shows pairwise tests of 2021 vs 2025 data for each species x plot group
contrasts <- contrast(ec_emm, interaction = "pairwise")
summary(contrasts)

#setting a width for offsets below
dodge_width <- 0.25

#plot of 21 vs 25 EMM with 95% CI
ggplot(emm_2way, aes(x = Year, y = emmean, color = Plot)) +
  geom_point(size = 3, position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,  
    position = position_dodge(width = dodge_width)) +
  geom_line(
    aes(group = Plot),
    position = position_dodge(width = dodge_width),
    linetype = "dashed") +
  theme_minimal() +
  labs(
    y = "Estimated Marginal Mean (Response)",
    x = "June Soil EC",
    title = "Estimated Marginal Means, 2021 vs 2025 Soil EC") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

#base model, here Year is acting as "BA", and we're allowing all levels of plot and species to interact
vwc_2way <- aov(soil_vwc_avg ~ Year*Plot,
               data = soil_BACI_data)
#calculate estimated marginal means
vwc_emm <- emmeans(vwc_2way, ~ Plot|Year)
emm_2way <- as.data.frame(vwc_emm)
#shows pairwise tests of 2021 vs 2025 data for each species x plot group
contrasts <- contrast(vwc_emm, interaction = "pairwise")
summary(contrasts)

#setting a width for offsets below
dodge_width <- 0.25

#plot of 21 vs 25 EMM with 95% CI
ggplot(emm_2way, aes(x = Year, y = emmean, color = Plot)) +
  geom_point(size = 3, position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,  
    position = position_dodge(width = dodge_width)) +
  geom_line(
    aes(group = Plot),
    position = position_dodge(width = dodge_width),
    linetype = "dashed") +
  theme_minimal() +
  labs(
    y = "Estimated Marginal Mean (Response)",
    x = "June Soil VWC",
    title = "Estimated Marginal Means, 2021 vs 2025 Soil VWC") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

