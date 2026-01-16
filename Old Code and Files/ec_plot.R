
# packages from main Rmd already loaded
library(zoo)

# here data is the main dataframe from the main Rmd
# data <- readRDS("Sapflow_BACI.rds")

# isolate soil data and summarise
data %>%
  dplyr::select(Year, TIMESTAMP, Plot,
                ec_avg, ec_sd,
                swc_avg) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  filter(Hour <= 14, Hour >= 11) %>%
  na.omit() %>%
  group_by(Year, Date, Plot) %>%
  summarise(soil_ec_avg = mean(ec_avg),
            ec_error = sd(ec_avg),
            soil_vwc_avg = mean(swc_avg)) %>%
  ungroup() -> soil_data

# created rolled mean of EC for plotting
soil_data %>%
  dplyr::select(Year, Plot, Date, soil_ec_avg) %>%
  group_by(Plot) %>%
  arrange(Date) %>%
  mutate(ec_rolled = rollmean(soil_ec_avg,
                              k = 3,
                              align = "center",
                              fill = NA)) -> ec_rolled
# pretty plot of EC data
soil_data %>%
  filter(Date > "2021-04-01") %>%
  #optional, gets ride of little squiggle at the begining of '21
ggplot(aes(Date, soil_ec_avg, color = Plot)) +
  geom_point(alpha=0.5) +
  geom_errorbar(aes(ymin = soil_ec_avg - ec_error,
                    ymax = soil_ec_avg + ec_error)) +
  geom_line(data = ec_rolled %>%
              filter(Date > "2021-04-01"),
            aes(Date, ec_rolled)) +
  scale_color_viridis_d(option = "mako",
                        begin = 0.3,
                        end = 0.7) + theme_bw() +
  theme(legend.position = "bottom") +
  ylab("Soil Electrical Conductivity (\u03bcS cm\u207b\u00b9)")

# analyze long-term BACI impact on EC and VWC
soil_data %>%
  filter(! Year %in% c(2022, 2023, 2024),
         month(Date) == 6) -> soil_BACI_data

# here Year is acting as "BA"
ec_2way <- aov(soil_ec_avg ~ Year*Plot,
                   data = soil_BACI_data)
#calculate estimated marginal means
ec_emm <- emmeans(ec_2way, ~ Plot|Year)
emm_2wayA <- as.data.frame(ec_emm)
#shows pairwise tests of 2021 vs 2025
contrasts <- contrast(ec_emm, interaction = "pairwise")
summary(contrasts)

#setting a width for offsets below
dodge_width <- 0.25

#plot of 21 vs 25 EMM with 95% CI for soil EC
ggplot(emm_2wayA, aes(x = Year, y = emmean, color = Plot)) +
  geom_point(size = 3, position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,  
    position = position_dodge(width = dodge_width)) +
  geom_line(
    aes(group = Plot),
    position = position_dodge(width = dodge_width),
    linetype = "dashed") +
  scale_color_viridis_d(option = "mako",
                        begin = 0.3,
                        end = 0.7) +
  theme_minimal() +
  labs(
    y = "EMM",
    x = "Year",
    title = "Long-Term BACI for Soil EC") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

# same for moisture content
vwc_2way <- aov(soil_vwc_avg ~ Year*Plot,
               data = soil_BACI_data)

vwc_emm <- emmeans(vwc_2way, ~ Plot|Year)
emm_2wayB <- as.data.frame(vwc_emm)

contrasts <- contrast(vwc_emm, interaction = "pairwise")
summary(contrasts)

# VWC plot
ggplot(emm_2wayB, aes(x = Year, y = emmean, color = Plot)) +
  geom_point(size = 3, position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,  
    position = position_dodge(width = dodge_width)) +
  geom_line(
    aes(group = Plot),
    position = position_dodge(width = dodge_width),
    linetype = "dashed") +
  scale_color_viridis_d(option = "mako",
                        begin = 0.3,
                        end = 0.7) +
  theme_minimal() +
  labs(
    y = "EMM",
    x = "Year",
    title = "Long-Term BACI for Soil VWC") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))

