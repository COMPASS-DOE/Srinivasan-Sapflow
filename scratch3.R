
data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`,
                ec_avg, swc_avg, TEMP, PAR) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
         Date < data_end & Date > flood_end,
         Hour <= 14, Hour >= 11,
         F <= 0.005, F > 0) %>%
  ungroup() -> clean_data

clean_data %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  group_by(Date, Plot, Species, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            soil_ec_avg = mean(ec_avg), 
            soil_vwc_avg = mean(swc_avg),
            par_avg = mean(PAR),
            temp_avg = mean(TEMP),
            n = n()) %>%
  ungroup() -> cd_1

cd_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) %>%
  mutate(BA = as.character(BA),
         BA = ifelse(Year == 2021, "Before", BA),
         BA = ifelse(Year == 2025, "After", BA)) -> cd_2

cd_2 %>%
  filter(! Year %in% c(2022, 2023, 2024),
         Species %in% c("Beech", "Tulip Poplar", "Red Maple")) %>%
  mutate(BA = factor(BA, levels = c("Before", "After"))) -> cd_3

model_3way <- lmer(F_avg ~ BA*Plot*Species + (1|ID),
                   data = cd_3)
Anova(model_3way, type = "III")
summary(model_3way)

model_emm <- emmeans(model_3way, ~ Plot|Species*BA)
summary(model_emm)
contrasts <- contrast(model_emm, interaction = "pairwise", by = c("Species", "Plot"))
summary(contrasts)

model_emm <- as.data.frame(emmeans(model_3way, ~ Species|Plot*BA))

# Plot estimated marginal means
ggplot(model_emm, aes(x = BA, y = emmean, color = Species)) +
  facet_grid(~ Plot) +
  geom_point() +
  geom_line(aes(group = interaction(Species, Plot))) +
  theme_minimal() +
  labs(y = "Estimated Marginal Mean (Response)", x = "Before / After")

ggplot(model_emm, aes(x = BA, y = emmean, color = Plot)) +
  facet_grid(~ Species) +
  geom_point() +
  geom_line(aes(group = interaction(Species, Plot))) +
  theme_minimal()


ggplot(model_emm, aes(x = BA, y = emmean, color = Plot)) +
  facet_grid(~ Species) +  # Facet by Plot
  geom_point(size = 3) +  # Add points for EMMs
  geom_line(aes(group = interaction(Species, Plot)), linetype = "dashed") + # Add dashed trend lines
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +  # Add error bars
  theme_minimal() +  # Minimal theme
  labs(
    y = "Estimated Marginal Mean (Response)",
    x = "Before / After",
    title = "Estimated Marginal Means with Error Bars"
  ) +
  theme(
    strip.text = element_text(face = "bold"), # Enhance facet labels
    legend.title = element_text(face = "bold") # Enhance legend title
  )


library(ggstance) # Optional for better control over position adjustments

# Define jitter width for points and error bars
jitter_width <- 0.25

# Plot with jittered points and error bars
ggplot(model_emm, aes(x = BA, y = emmean, color = Plot)) +
  facet_grid(~ Species) +  # Facet by Plot
  geom_point(size = 3, position = position_dodge(width = jitter_width)) +  # Jittered points
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0,  # Make the error bar straight (no horizontal width)
    position = position_dodge(width = jitter_width)  # Jitter error bars
  ) +
  geom_line(
    aes(group = interaction(Species, Plot)),
    position = position_dodge(width = jitter_width), # Jitter trend lines (optional)
    linetype = "dashed"
  ) +
  theme_minimal() +  # Minimal theme
  labs(
    y = "Estimated Marginal Mean (Response)",
    x = "Before / After",
    title = "Estimated Marginal Means, 2021 vs 2025 Sapflow"
  ) +
  theme(
    strip.text = element_text(face = "bold"), # Enhance facet labels
    legend.title = element_text(face = "bold") # Enhance legend title
  )

####
####
#inverse

cd_2 %>%
  filter(Year %in% c(2022, 2023, 2024),
         Species %in% c("Beech", "Tulip Poplar", "Red Maple")) %>%
  mutate(BA = factor(BA, levels = c("Before", "After"))) -> cd_4

alt_model_3way <- lmer(F_avg ~ BA*Plot*Species + (1|ID) + (1|Year),
                   data = cd_4)
Anova(alt_model_3way , type = "III")
summary(alt_model_3way )

model_emm <- emmeans(alt_model_3way , ~ Species|Plot*BA)
summary(model_emm)
contrasts <- contrast(model_emm, interaction = "pairwise", by = c("Species", "Plot"))
summary(contrasts)

model_emm <- as.data.frame(emmeans(alt_model_3way, ~ Species|Plot*BA))

# Plot estimated marginal means
ggplot(model_emm, aes(x = BA, y = emmean, color = Species)) +
  facet_grid(~ Plot) +
  geom_point() +
  geom_line(aes(group = interaction(Species, Plot))) +
  theme_minimal() +
  labs(y = "Estimated Marginal Mean (Response)", x = "Before / After")

ggplot(model_emm, aes(x = BA, y = emmean, color = Plot)) +
  facet_grid(~ Species) +
  geom_point() +
  geom_line(aes(group = interaction(Species, Plot))) +
  theme_minimal()


ggplot(alt_tsb_2, aes(par_avg, F_avg, color = Species)) +
  geom_point() +
  facet_grid(Plot ~ Year)


# Step 1: Calculate slopes for each Species-Plot-Year combination
slopes <- alt_tsb_2 %>%
  group_by(Species, Plot, Year, BA) %>%
  summarize(slope = coef(lm(F_avg ~ par_avg))[2],  # Extract slope (coefficient for par_avg)
            intercept = coef(lm(F_avg ~ par_avg))[1], # Extract intercept
            .groups = "drop")  # Drop grouping for clean output

# Step 2: Create the plot with trend lines and slope annotations
ggplot(alt_tsb_2, aes(par_avg, F_avg, color = Plot,
                      group = interaction(Plot, BA),
                      shape = Plot)) +
  geom_jitter(alpha = 0.5) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, aes(linetype = BA)) +  # Linear trend lines without confidence intervals
  facet_grid(Species ~ Year, scales = "free") +  # Facet by Plot and Year
  # geom_text(
  #   data = slopes,
  #   aes(
  #     x = Inf,  # Position slope annotation to the far right of each facet
  #     y = Inf,  # Position slope annotation at the top of each facet
  #     label = paste0("Slope: ", round(slope, 4)) # Format slope values
  #   ),
  #   inherit.aes = FALSE,  # Ensure text doesn't inherit aesthetics like `color`
  #   hjust = 1,  # Adjust text alignment
  #   vjust = 1,  # Adjust text alignment
  #   size = 3    # Size of the text
  # ) +
  labs(
    title = "Linear Trend Lines with Slope Annotations",
    x = "PAR Average (par_avg)", 
    y = "F_avg (Response)"
  ) +
  theme_minimal()

###
###

tsb_2 %>%
  filter(Species %in% c("Beech", "Tulip Poplar", "Red Maple")) %>%
  mutate(BA = factor(BA, levels = c("Before", "After"))) -> alt_tsb_2

slopes <- alt_tsb_2 %>%
  group_by(Species, Plot, Year, BA) %>%
  summarize(
    slope = ifelse(n() > 1, coef(lm(F_avg ~ par_avg))[2], NA),  # Skip slope for groups with insufficient data
    intercept = ifelse(n() > 1, coef(lm(F_avg ~ par_avg))[1], NA),
    .groups = "drop"
  )


# Step 2: Create the plot with trend lines and slope annotations
ggplot(alt_tsb_2, aes(par_avg, F_avg, color = Plot,
                      group = interaction(Plot, BA),
                      shape = Plot)) +
  geom_jitter(alpha = 0.5) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, aes(linetype = BA)) +  # Linear trend lines without confidence intervals
  facet_grid(Species ~ Year, scales = "free") +  # Facet by Plot and Year
  labs(
    title = "Linear Trend Lines with Slope Annotations",
    x = "PAR Average (par_avg)", 
    y = "F_avg (Response)"
  ) +
  theme_minimal()
