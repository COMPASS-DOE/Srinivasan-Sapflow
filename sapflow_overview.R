#Creating single-day and growing season plots of sapflow
#To determine optimal filter times 

final_tmp_data %>%
  dplyr::select(ID, Year, Species, Plot, TIMESTAMP, Fd) %>%
  filter(Year == "2023") %>%
  drop_na(Fd) %>%
  filter(Fd != Inf) %>%
  filter(Fd < 1.5) %>%
  group_by(TIMESTAMP) %>%
  summarise(Fd = mean(Fd),
            TIMESTAMP = TIMESTAMP) -> daily_sapflow

daily_sapflow %>%
  mutate(Date = date(TIMESTAMP)) %>%
  group_by(Date) %>%
  summarise(Fd = mean(Fd), 
            TIMESTAMP = TIMESTAMP) -> daily_sapflow

ggplot(daily_sapflow) +
  geom_point(aes(x = TIMESTAMP, y = Fd)) + 
  labs(y = "Avg Sap Flux Density cm3/cm2/s", x = "Date",
       title = "Sap Flux Density Averaged")

ggsave("daily_sapflow.jpg")

#Potentially April-Nov? 

final_tmp_data %>%
  dplyr::select(ID, Year, Species, Plot, TIMESTAMP, Fd) %>%
  mutate(Month = month(TIMESTAMP), 
         Hour = hour(TIMESTAMP)) %>%
  drop_na(Fd) %>%
  filter(Fd != Inf,
         Fd < 1.5) %>%
  filter(Month <= 11, Month >= 4) %>%
  group_by(Hour) %>%
  summarise(Fd = mean(Fd), 
            Hour = Hour) -> hourly_sapflow

ggplot(hourly_sapflow) +
  geom_point(aes(x = Hour, y = Fd)) +
  labs(y = "Fd", x = "Hour",
     title = "Sap Flux Density Averaged Hourly")
  
ggsave("hourly_sapflow.jpg")

#ANOVA test justifying species choice 
#Species, species x plot

#cleaning up data 
#filtering for post-flood period (to test difference in flood effects between species and plot)

final_tmp_data %>%
  dplyr::select(ID, Year, Species, Plot, TIMESTAMP, Fd) %>%
  drop_na(Fd) %>%
  filter(Fd != Inf) %>%
  filter(Fd <= 1.5, Fd >= 0) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>% 
  group_by(Year) %>%
  mutate(data_start = flood_start,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Hour <= 19, Hour >= 7) %>%
  ungroup() -> postflood

current_skewness <- skewness(postflood$Fd)

log_transformed_response <- log(postflood$Fd + 1)

sqrt_transformed_response <- sqrt(postflood$Fd)

inv_postflood <- postflood %>%
  filter(Fd != 0)

inv_transformed_response <- 1 / inv_postflood$Fd

log_skewness <- skewness(log_transformed_response)
sqrt_skewness <- skewness(sqrt_transformed_response)
inv_skewness <- skewness(inv_transformed_response)

#Model
mod <- aov(Fd  ~ Species * Plot,
           data = postflood)

summary(mod)

ggplot(postflood) +
  aes(x = Species, y = Fd, fill = Plot) +
  geom_boxplot()

final_tmp_data %>%
  dplyr::select(ID, Year, Species, Plot, TIMESTAMP, Fd) %>%
  drop_na(Fd) %>%
  filter(Fd != Inf) %>%
  filter(Fd <= 1.5, Fd >= 0) %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  left_join(events) %>% 
  group_by(Year) %>%
  mutate(data_start = flood_start - window,
         data_end = flood_end + window) %>%
  filter(Date > data_start & Date < flood_start |
           Date < data_end & Date > flood_end,
         Hour <= 14, Hour >= 11) %>%
  mutate(BA = case_when(Date < flood_start ~ "Before",
                             Date > flood_end ~ "After" )) %>%
  ungroup() -> prepostflood

prepostflood %>%
  filter(Plot == "Saltwater") %>%
  dplyr::select(ID, Year, Species, Fd, BA) %>%
  group_by(Year, BA, Species) %>%
  summarise(Fd_avg = mean(Fd)) %>%
  pivot_wider(
    names_from = BA,
    values_from = Fd_avg) %>%
  mutate(Difference = After - Before) -> temp

prepostflood %>%
  filter(Year == "2022") %>%
  ggplot(aes(x = Date, y = Fd, color = Species)) +
  geom_jitter()
  
  
