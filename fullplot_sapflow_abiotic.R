#Full plot of sapflow, soil VWC, soil EC, and PAR 
#Midday, control plot average, over all 4 years 

library(patchwork)

full_data <- readRDS("Full_21_24_updated.rds")

full_data %>%
  group_by(TIMESTAMP) %>%
  mutate(PAR = ifelse(is.na(PAR), PAR[Plot == "Freshwater"], PAR)) %>%
  ungroup() -> dat1

dat1 %>%
  mutate(Hour = hour(TIMESTAMP), 
         Date = date(TIMESTAMP)) %>%
  filter(Hour < 14, Hour >= 11, 
         Plot == "Control",
         `F` < 1.5e-06,
          soil_vwc_15cm > 0.25,
          (PAR > 480 | is.na(PAR))) %>%
  group_by(Hour, Date, Species) %>%
  summarize(F_avg = mean(`F`), 
            swc_avg = mean(soil_vwc_15cm),
            par_avg = mean(PAR), 
            sec_avg = mean(soil_ec_15cm)) -> fulldata_plot

fulldata_plot %>% 
  pivot_longer(cols = c(F_avg, swc_avg, par_avg, sec_avg), 
               names_to = "Variable") -> long_plot

sapflow_plot <- 
  ggplot(data = fulldata_plot, aes(x = Date, y = F_avg, color = Species)) + 
  geom_point() + 
  guides(color = "none") + 
  labs(x = NULL, y = "Sap Flux Density, g/m^3")

sapflow_plot

par_plot <- 
  ggplot(data = fulldata_plot, aes(x = Date, y = par_avg)) + 
  geom_point() + 
  labs(x = NULL, y = "PAR, umol/m^2/s")

swc_plot <- 
  ggplot(data = fulldata_plot, aes(x = Date, y = swc_avg)) + 
  geom_point() +
  labs(x = NULL, y = "Soil VWC, m^3/m^3")

sec_plot <- 
  ggplot(data = fulldata_plot, aes(x = Date, y = sec_avg)) + 
  geom_point() + 
  labs(x = NULL, y = "Soil EC, uS/cm")

combined_plot <- (sapflow_plot | par_plot) / (swc_plot | sec_plot)

combined_plot + 
  plot_annotation( title = "Sapflow and Abiotic Variables in Control Plot, 
                   averaged 11 AM - 2 PM") 

ggplot(data = long_plot, aes(x= Date, y = value, color = Species)) + 
  geom_point(aes(color = ifelse(Variable == "F_avg", as.character(Species), NA))) + 
  facet_wrap(~Variable, scales = "free", labeller = label_both) 
  
  
  
  