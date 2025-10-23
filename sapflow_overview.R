#Creating single-day and growing season plots of sapflow
#To determine optimal filter times 

final_tmp_data %>%
  dplyr::select(ID, Year, Species, Plot, TIMESTAMP, Fd) %>%
  filter(Year == "2023") %>%
  drop_na(Fd) %>%
  filter(Fd != Inf) %>%
  filter(Fd < 3) %>%
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



