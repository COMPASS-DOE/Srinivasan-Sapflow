
filtered_data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`,
                ec_avg, swc_avg, TEMP, PAR) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP),
         Year = as.factor(Year)) %>%
  group_by(Date, Year) %>%
  filter(max(PAR) > 500) %>%
  ungroup() %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Plot != "Saltwater",
         Species == "Beech",
         
  ) %>%
  ungroup() -> beech_fresh

beech_fresh %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  group_by(Date, Plot, Species, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            soil_ec_avg = mean(ec_avg), 
            soil_vwc_avg = mean(swc_avg, na.rm = TRUE),
            par_avg = mean(PAR),
            temp_avg = mean(TEMP),
            n = n()) %>%
  ungroup() -> bfb_1

bfb_1 %>%
  filter(! is.na(bfb_1$Species),
         ! Year %in% c(2021, 2025)) %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))
  ) %>%
  mutate(BA = as.character(BA),
         # BA = ifelse(Year == 2021, "Before", BA),
         # BA = ifelse(Year == 2025, "After", BA)
  ) -> bfb_2

ggplot(bfb_2, aes(Day, F_avg, color = ID, shape = BA)) +
  geom_point(size = 2.5) + facet_grid(Plot~Year, scales = "free")

current_skewness <- skewness(bfb_2$F_avg)
print(paste("Current Skewness:", current_skewness))

ggplot(bfb_2, aes(F_avg)) + geom_histogram(fill = '#31688EFF') +
  facet_grid(Plot~.) +
  labs(y = NULL, x = "Average Sapflow", title = "Untransformed Data")

#initial BACI
model.int.og <- lmer(F_avg ~ BA*Plot +
                       (1|ID) + (1|Year) + (1|ID:Year),
                     data = bfb_2 )

model.noint.og <- lmer(F_avg ~ BA + Plot +
                         (1|ID) + (1|Year) + (1|ID:Year),
                       data = bfb_2 )

og.refdist <- PBrefdist(largeModel = model.int.og, 
                        smallModel = model.noint.og, 
                        nsim = 100)

og.compar <- PBmodcomp(largeModel = model.int.og, 
                       smallModel = model.noint.og,
                       ref = og.refdist)

og.compar
Anova(model.int.og, type = "III")

#with VWC
model.int.vwc <- lmer(F_avg ~ BA*Plot + soil_vwc_avg +
                       (1|ID) + (1|Year) + (1|ID:Year),
                     data = bfb_2 )

model.noint.vwc <- lmer(F_avg ~ BA + Plot + soil_vwc_avg +
                         (1|ID) + (1|Year) + (1|ID:Year),
                       data = bfb_2 )

vwc.refdist <- PBrefdist(largeModel = model.int.vwc, 
                        smallModel = model.noint.vwc, 
                        nsim = 100)

vwc.compar <- PBmodcomp(largeModel = model.int.ec, 
                       smallModel = model.noint.ec,
                       ref = vwc.refdist)
vwc.compar
Anova(model.int.vwc, type = "III")
