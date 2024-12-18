##Estimating BACI differences (Roon et al. 2021)

#From paper: 
#We estimated the BACI differences and 95% confidencintervals. 
#If 95% confidence intervals did not overlap 0, we considered the effect to be 
#statistically significant. We checked the residuals for all BACI models to 
#make sure we met assumptions of constant variance and normality.

#(Post salt-flood - pre salt-flood) - (post control - pre control) 

tsb_2 %>%
  mutate(group = paste0(BA, Plot)) %>%
  group_by(group, Year) %>%
  summarize(mean = mean(F_avg)) %>%
  spread(group, mean) -> summ

summ %>%
  group_by(Year) %>%
  summarize(BACI_diff = ((AfterSaltwater - BeforeSaltwater) - 
                           (AfterControl - BeforeControl))) -> BACI_diff
#Year      BACI_diff
#<fct>         <dbl>
#1 2021   0.0000000973
#2 2022   0.0000000330
#3 2023  -0.0000000645
#4 2024   0.000000140 

ci <- confint(model.int_sr, level = 0.95)
ci
#                        2.5 %       97.5 %
#.sig01                  9.184904e-05 1.547343e-04
#.sig02                  1.036203e-04 2.697054e-04
#.sig03                  6.151734e-05 3.107798e-04
#.sigma                  1.450212e-04 1.570696e-04
#(Intercept)             5.944312e-04 9.965082e-04
#BABefore               -1.508037e-05 3.307362e-05
#PlotSaltwater          -3.352092e-04 9.209868e-05
#BABefore:PlotSaltwater -5.238767e-05 1.521799e-05

##Isolating different species & '21 vs '24
full_data %>%
  dplyr::select(Year, Species, ID, TIMESTAMP, Plot, `F`) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>%
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Hour < 14, Hour >= 11, 
         Plot != "Freshwater",
         Species == "Red Maple") %>%
  ungroup() -> maple_salt

maple_salt %>%
  group_by(Year) %>%
  mutate(BA = ifelse(Date > flood_end, "After", "Before"),
         Year = as.factor(Year),
         ID = as.factor(ID),
         BA = as.factor(BA),
         Plot = as.factor(Plot)) %>%
  filter(`F` < 4e-07) %>% #remove outliers, ggplot below for confirmation
  group_by(Date, Plot, ID, Year, BA, flood_start, flood_end) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            n = n()) %>%
  ungroup()-> msb_1

ggplot(maple_salt[maple_salt$`F` < 4e-07,],
       aes(Date, `F`, color = ID)) +
  geom_point() + facet_grid(Plot~Year, scales = "free")

msb_1 %>%
  group_by(Year) %>%
  mutate(Day = ifelse(BA == "Before",
                      - as.numeric(flood_start - Date),
                      as.numeric(Date - flood_end))) -> msb_2

#Create same model as for tulip poplars, assuming same transformation is applicable
model.int_sr_maple <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot +
                                (1|ID) + (1|Year) + (1|Year:ID),
                              data = msb_2, family = gaussian)

model.noint_sr_maple <- glmer(sqrt(F_avg) ~ BA + Plot +
                          (1|ID) + (1|Year) + (1|Year:ID),
                        data = msb_2, family = gaussian)

refdist.pb.100.interaction <- PBrefdist(largeModel = model.int_sr_maple, 
                                        smallModel = model.noint_sr_maple, 
                                        nsim = 100, seed = 1989)

compar.interaction.100 <- PBmodcomp(largeModel = model.int_sr_maple, 
                                    smallModel = model.noint_sr_maple,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100

#Tulip Poplars, pre 21- post 24
tsb_2 %>%
  filter(Year == "2021" & BA == "Before" | 
         Year == "2024" & BA == "After") -> tsb_3

model.int_sr_tsb3 <- glmer(sqrt(F_avg) ~ BA + Plot + BA*Plot +
                              (1|ID) + (1|Year) + (1|Year:ID),
                            data = tsb_3, family = gaussian)

model.noint_sr_tsb3 <- glmer(sqrt(F_avg) ~ BA + Plot +
                             (1|ID) + (1|Year) + (1|Year:ID),
                           data = tsb_3, family = gaussian)

###
refdist.pb.100.interaction <- PBrefdist(largeModel = model.int_sr_tsb3, 
                                        smallModel = model.noint_sr_tsb3, 
                                        nsim = 100, seed = 1989)

compar.interaction.100 <- PBmodcomp(largeModel = model.int_sr_tsb3, 
                                    smallModel = model.noint_sr_tsb3,
                                    ref = refdist.pb.100.interaction)
compar.interaction.100

