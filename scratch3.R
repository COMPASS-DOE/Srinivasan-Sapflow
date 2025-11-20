
# First chunk tests BACI across the 3 by 3
# three species and three treatment plots
# using only 2021 (before) and 2025 (after) data
# currently we only have sapflow data through June of '25

library(ggstance) #for dodging points later on

data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`,
                ec_avg, swc_avg, TEMP, PAR) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  group_by(Year) %>%
  filter(month(Date) == 6, #just June
         #should add more data once available
         Hour <= 14, Hour >= 11, #same window as for main markdown
         F <= 0.005, F > 0) %>% #same filtering as for main markdown
  ungroup() -> BACI_data

BACI_data %>%
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
  ungroup() %>%
  filter(! Year %in% c(2022, 2023, 2024),
         Species %in% c("Beech", "Tulip Poplar", "Red Maple")) -> avg_BACI_data

#base model, here Year is acting as "BA", and we're allowing all levels of plot and species to interact
#random effect of ID allows for different trees to have different intercepts
model_3way <- lmer(F_avg ~ Year*Plot*Species + (1|ID),
                   data = avg_BACI_data)
#calculate estimated marginal means
model_emm <- emmeans(model_3way, ~ Species|Plot*Year)
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
