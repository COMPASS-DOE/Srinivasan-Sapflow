#Betel's salt_water analysis
#Summer 2025

#read in data and visualize
library(readr)
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

sapflux_salt <-read_csv("Data/TMP_S_20240101-20241231_sapflow-3.5cm_L1_v2-0.csv")

#Read in Control plot data to visualize a comparison between salt plot and freswater plot.
sapflux_control<-read_csv("Data/TMP_C_20240101-20241231_sapflow-3.5cm_L1_v2-0.csv")

#Read inc Freshwater plot for caparison between the other treatments
sapflux_fresh<-read_csv("Data/TMP_F_20240101-20241231_sapflow-3.5cm_L1_v2-0.csv")
sapflux_fresh <- sapflux_fresh %>%
  mutate(Plot = "F")
sapflux_fresh<- sapflux_fresh%>% mutate(Plot = as.character(Plot))

#Bind all the treatments in one data
sapflux <- bind_rows(sapflux_salt, sapflux_control, sapflux_fresh)

library(ggplot2)
ggplot(sapflux, aes(x=TIMESTAMP, y=Value))+geom_point()


#original data
#sapflux <- read_csv("Data/TMP_S_20240101-20241231_sapflow-3.5cm_L1_v2-0.csv")


# Read in weather data and join with sapflow data
humidity <-read_csv("Data/GCW_W_20240101-20241231_wx-rh15_L1_v2-0.csv")

# The humidity data have duplicated rows and we average by Timestamp
humidity <- humidity%>% group_by(TIMESTAMP) %>% summarise(RH=mean(Value), .groups = "drop")
sapflux_join<- sapflux %>% left_join(humidity,by="TIMESTAMP")

#Read in weather data and join with sapflow data
sunlight<-read_csv("Data/GCW_W_20240101-20241231_wx-par-den15_L1_v2-0.csv")

# The sunlight data have duplicated rows and we average by Timestamp
sunlight <- sunlight%>% group_by(TIMESTAMP) %>% summarise(PAR=mean(Value))
sapflux_join<-sapflux_join %>%left_join(sunlight,by="TIMESTAMP",relationship = "many-to-one")

#Read in weather data and join with sapflow data
temp<-read_csv("Data/GCW_W_20240101-20241231_wx-tempavg15_L1_v2-0.csv")

# The temperature data have duplicated rows and we average by Timestamp
temp<-temp %>% group_by(TIMESTAMP) %>% summarise(temp=mean(Value))
sapflux_join<-sapflux_join %>% left_join(temp, by="TIMESTAMP",relationship = "many-to-one")

#Read in soil data to visualize a hysteresis loop between wet and dry soil
soil<-read_csv("Data/TMP_S_20240101-20241231_soil-vwc-5cm_L1_v2-0.csv")

# The soil data have duplicated rows and we average by Timestamp
soil<-soil %>% group_by(TIMESTAMP) %>% summarise(soil=mean(Value))
sapflux_join<-sapflux_join %>% left_join(soil, by= "TIMESTAMP", relationship = "many-to-one")


#End of data reading

# Filter for legitimate values (0-1 range)
# For now, we focus on July and daylight only
sapflux_join %>%
  filter(Value > 0, Value <= 1) %>%
  mutate(Month = month(TIMESTAMP),
         Hour = hour(TIMESTAMP),
         Day = date(TIMESTAMP)) %>%
  filter(Month == 7) -> sapflux_july
View(sapflux_july)

ggplot(sapflux_july,aes(x=TIMESTAMP, y=Value))+geom_point()

#Calculate VPD
sapflux_july <- sapflux_july %>%
  mutate( es = 0.6108 * exp((17.27 * temp) / (temp + 237.3)),  
          ea = es * RH / 100, 
          VPD = es - ea )

#Filter for daylight, summaries to one value per tree/day/hour, and rescale values
library(scales)
sapflow_hourly <- sapflux_july %>% 
  # filter for daylight only
  filter(Hour >= 5, Hour <= 20) %>%
  # summaries to one value per hour
  group_by(Month, Day, Hour, Sensor_ID, Plot) %>% 
  summarise(mean_sapflow = mean(Value),
            mean_PAR=mean(PAR),
            mean_soil_vwc=mean(soil),
            mean_VPD = mean(VPD), .groups = "drop") %>% 
  # compute relative VPD and sapflow values
  group_by(Sensor_ID, Day) %>%
  mutate(relative_sapflow = rescale(mean_sapflow, to= c(1,0)),  
         relative_vpd= rescale(mean_VPD, to=c(0,1))) %>%
  ungroup()

# END OF DATA PROCESSING


#filter just for salt plot
sapflow_hourly_S <- sapflow_hourly %>%
  filter(Plot == "S")
#Plot daily hysteresis loop for all trees and all days of July
ggplot(sapflow_hourly_S, aes(x = relative_vpd, y = relative_sapflow, 
                           group = Day, color = Hour)) +
  geom_path(size = 0.8, alpha = 0.9) +
  facet_wrap(~ Sensor_ID, nrow = 5, ncol = 4) +
  scale_color_viridis_c(option="inferno") +
  labs(title = "Daily Hysteresis (July 2024)",
       x = "VPD (kPa)",
       y = "Relative Sapflow" ) + theme_minimal()

#Based on the above graph drop S5 because it's bizarre
sapflow_hourly<-sapflow_hourly %>% 
  filter(Sensor_ID !="S5")
#Plot hysteresis for only one Tree_ID
selected_trees <- sapflow_hourly %>%
  filter(Sensor_ID %in% c("S1"))

#calculate average for each hour
selected_trees %>% 
  group_by(Hour) %>% 
  summarise(relative_vpd=mean(relative_vpd),
            relative_sapflow=mean(relative_sapflow)) ->
  selected_trees_avg


#Plot daily hysteresis loops (sapflow vs. VPD) for each selected tree, with color indicating hour of day
library(viridis)
ggplot(selected_trees, aes(x = relative_vpd, y = relative_sapflow)) +
  geom_path(size = 0.5, color="lightgray",aes(group=Day)) + 
  geom_path(data=selected_trees_avg, size=1.5, aes(color=Hour))+
  facet_wrap(~ Sensor_ID) +
  scale_color_viridis(option="inferno") +
  labs(title = "Hysteresis Loops Under Salt Stress",
       x = "Relative VPD", y = "Relative Sapflow") +
  theme_minimal()+ theme(plot.title = element_text(face = "bold", size = 13))
ggsave("S1_July24_Hysteresis.png")

#Filter for one tree and one day, and only daylight hours (5 AM–8 PM)
daylight_data <- sapflow_hourly %>%
  filter(Sensor_ID == "S1",
         Day == as.Date("2024-07-15"))


#Plot hysteresis loop
ggplot(daylight_data, aes(x = relative_vpd, y = relative_sapflow, color = Hour)) +
  geom_path(size = 1.2) +
  scale_color_viridis_c(option = "inferno") +
  labs( title = "Hysteresis Loop (Daylight Only) – Tree S1 on July 15",
        x = "VPD (kPa)",
        y = "Normalized Sapflow (0–1)",
        color = "Hour" ) + theme_minimal()

#Filter for 3 days, one tree, and daylight hours
three_days <- sapflow_hourly %>%
  filter(Sensor_ID == "S3",Day %in% as.Date(c("2024-07-10", "2024-07-11", "2024-07-15")))

#Plot hysteresis for 3 days (separate panels)
ggplot(three_days, aes(x = relative_vpd, y = relative_sapflow, group = Day, color = Hour)) +
  geom_path(size = 1.2) +
  facet_wrap(~ Day) +
  scale_color_viridis_c(option = "inferno") +
  labs(title = "Hysteresis Loops – Tree S3 on 3 Days (5 AM to 8 PM)",
       x = "VPD (kPa)",
       y = "Normalized Sapflow (0–1)",
       color = "Hour") + theme_minimal()


#To create hysteresis plot that include polygon and arrows
ggplot(three_days, aes(x = relative_vpd, y = relative_sapflow, group = Day)) +
  geom_polygon(aes(group = Day), fill = "yellow", alpha = 0.4, color = NA) +  
  geom_path(arrow = arrow(type = "closed", length = unit(0.15, "inches")),
            color = "blue", size = 1.1) +                                     
  geom_point(color = "black", size = 2) +                                     
  geom_text(data = three_days %>% filter(Hour %in% c(5, 10, 15, 20)),
            aes(label = paste0(Hour, ":00")),
            color = "red", size = 3,nudge_y = 0.05, vjust = -0.05) +                          
  facet_wrap(~ Day) +
  labs( title = "Hysteresis Loops – Tree S3 (5 AM to 8 PM, July 10,11,15)",
        x = "VPD / VPDmax",
        y = "Sapflow / SFmax") + coord_fixed() + theme_minimal()

#Plot 3 different tree in 3 different days
three_tree_days <- sapflow_hourly %>%
  filter((Sensor_ID == "S2" & Day == as.Date("2024-07-10"))|
      (Sensor_ID == "S3" & Day == as.Date("2024-07-11")) |
      (Sensor_ID == "S4" & Day == as.Date("2024-07-15")) )


#Plot the hysteresis loop
ggplot(three_tree_days, aes(x = relative_vpd, y = relative_sapflow)) +
  geom_path( aes(group = interaction(Sensor_ID, Day), color = as.factor(Hour)),
    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
    size = 1) + geom_point(aes(color = as.factor(Hour)), size = 2) +
  scale_color_viridis_d(name = "Hour") +
  facet_wrap(~ Sensor_ID + Day, nrow = 1) +
  labs( title = "Hysteresis Loops for S2, S3, and S4 (Different Days)",
    x = "Relative VPD (0–1)", y = "Relative Sapflow (0–1)") + theme_minimal() +
  theme( strip.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12) )


#Three different trees across three different days
selected_days <- as.Date(c("2024-07-20", "2024-07-21", "2024-07-23"))
selected_trees <- c("S4", "S6", "S7")
tree_3 <- sapflow_hourly %>%
  filter(Sensor_ID %in% selected_trees,
         Day %in% selected_days)
         

#Plot the hysteresis
ggplot(tree_3, aes(x = relative_vpd, y = relative_sapflow, color = Hour, group = interaction(Day, Sensor_ID))) +
  geom_polygon(aes(group = Day), fill = "Pink", alpha = 0.4, color = NA) +  
  geom_path(size = 1, alpha = 0.9) +
  facet_grid(Sensor_ID ~ Day) +
  scale_color_viridis_c(name = "Hour", option = "inferno") +
  labs(title = "Individual Tree Responses Under Salt Stress",
       x = "Relative VPD",
       y = "Relative Sapflow") +
  theme_minimal() + theme( strip.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14))
ggsave("3trees_3days_July24.png",width = 8, height = 4.5)

#Three trees across three different days line plot(no loop)
ggplot(tree_3, aes( x = relative_vpd, y = relative_sapflow, color = Hour,
  group = interaction(Day, Sensor_ID))) +
  geom_line(size = 1.2, alpha = 0.9) +  
  facet_grid(Sensor_ID ~ Day) +
  scale_color_viridis_c(name = "Hour", option = "plasma") +
  labs(title = "Sapflow vs VPD for Trees S5–S7",
    x = "Relative VPD",
    y = "Relative Sapflow") + theme_minimal() +theme(strip.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14))

#Filter just for Tulip tree (sensor 1-6), daylight hours(5am-8pm)
#To see the variability between Tulip trees
tulip_daylight <- sapflow_hourly %>%
  filter(Sensor_ID %in% paste0("S", 1:6))


#Plot the hysteresis loop that show the variability between Tulip trees
ggplot(tulip_daylight, aes(x = relative_vpd, y = relative_sapflow,color = Hour,
                           group = interaction(Day, Sensor_ID))) +
  geom_path(alpha = 0.8, size = 1) +
  facet_wrap(~ Sensor_ID, ncol = 2) +
  scale_color_viridis_c(option = "inferno", name = "Hour of Day") +
  labs(title = "Hysteresis Loops for Tulip Trees (S1–S6):",
    x = "Relative VPD (0–1)",
    y = "Relative Sapflow (0–1)") +theme_minimal()+ theme(strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12))

#This is to figure out the days that were sunny or cloudy 
#Compute average PAR and assign "Sunny" or "Cloudy"
july_sun <- sapflow_hourly %>%
  group_by(Day) %>%
  summarise(daily_PAR = mean(mean_PAR, na.rm = TRUE)) %>%
  mutate(
    sunny_cloudy = if_else(
      daily_PAR >= median(daily_PAR, na.rm = TRUE),
      "Sunny", "Cloudy" ))


# Merge the sunny/cloudy information back into the sapflux data
sapflow_hourly <- sapflow_hourly %>%
  left_join(july_sun, by = "Day")

#Calculate daily average soil moisture and label each day
july_soil <- sapflow_hourly %>%
  group_by(Day) %>%
  summarise(daily_soil = mean(mean_soil_vwc, na.rm = TRUE)) %>%
  mutate(wet_dry = if_else(
      daily_soil >= median(daily_soil, na.rm = TRUE),
      "Wet","Dry"))

# Merge back into sapflux hourly data
sapflow_hourly <- sapflow_hourly %>%
  left_join(july_soil, by = "Day")

#Add the new columns sunny_cloudy and wet_dry to tulip trees data to plot a loop.
tulip_daylight <- tulip_daylight %>%
  left_join( sapflow_hourly %>% 
               select(Day, sunny_cloudy, wet_dry) %>% distinct(),
    by = "Day") %>%
  drop_na(relative_sapflow, relative_vpd, sunny_cloudy, wet_dry)

#Plot the loop to visualize how the trees react in different environmental condition
ggplot(tulip_daylight, aes(x = relative_vpd, y = relative_sapflow, 
  color = Sensor_ID, group = interaction(Day, Sensor_ID))) +
  geom_path(alpha = 0.6) +
  facet_grid(sunny_cloudy ~ wet_dry) +
  labs( title = "Hysteresis Loops of Sapflow vs VPD in Tulip Trees",
    x = "Vapor Pressure Deficit (VPD)",
    y = "Sapflow (Value)",
    color = "Sensor ID") +
  theme_minimal() +
  theme( strip.text = element_text(face = "bold"),
    legend.position = "bottom")

#calculate average for each hour
avg_tulip <- tulip_daylight %>%
  group_by(sunny_cloudy, wet_dry, Hour) %>%
  summarise(
    relative_vpd = mean(relative_vpd, na.rm = TRUE),
    relative_sapflow = mean(relative_sapflow, na.rm = TRUE),
    .groups = "drop" ) %>%
  mutate(Sensor_ID = "avg")


#Plot the average of tulip trees with individuals in the back
ggplot(tulip_daylight,
       aes(x = relative_vpd, y = relative_sapflow,
           group = Sensor_ID)) +
  geom_path(color = "gray", size = 0.4, alpha = 0.3) +
  geom_path(data = avg_tulip,
            aes(color = Hour), size = 1.2) +
  facet_grid(sunny_cloudy ~ wet_dry) +
  scale_color_viridis(option = "inferno", name = "Hour") +
  labs(
    title = "Lag Drivers in Sap Flow of Salt-Stressed Tulip Trees",
    x = "Vapor Pressure Deficit (VPD)",
    y = "Normalized Sapflow (0–1)") +
  theme_minimal()+theme(
    plot.title = element_text(face = "bold", size = 13))
  ggsave("wetdry_cloudysunny_July24.png")
 

#filter just for Tulip trees in control plot
  control_july<-sapflow_hourly %>% 
    filter(Sensor_ID %in% paste0("C", 1:6))
  
#Plot the trees to see if there is a tree that is bizarre like S5
ggplot(control_july, aes(x = relative_vpd, y = relative_sapflow,color = Hour,
                             group = interaction(Day, Sensor_ID))) +
    geom_path(alpha = 0.8, size = 1) +
    facet_wrap(~ Sensor_ID, ncol = 2) +
    scale_color_viridis_c(option = "inferno", name = "Hour of Day") +
    labs(title = "Hysteresis Loops for Tulip Trees (C1–C6):",
         x = "Relative VPD (0–1)",
         y = "Relative Sapflow (0–1)") +
  theme_minimal()+ theme(strip.text = element_text(size = 10, face = "bold"),
           plot.title = element_text(size = 14, face = "bold"),
           axis.title = element_text(size = 12))

#filter just for tulip trees in fresh plot
fresh_july<-sapflow_hourly %>% 
  filter(Sensor_ID %in%  paste0("F",1:6))

#Plot the trees to see if there is a tree that is bizarre like S5
ggplot(fresh_july, aes(x = relative_vpd, y = relative_sapflow,color = Hour,
                         group = interaction(Day, Sensor_ID))) +
  geom_path(alpha = 0.8, size = 1) +
  facet_wrap(~ Sensor_ID, ncol = 2) +
  scale_color_viridis_c(option = "inferno", name = "Hour of Day") +
  labs(title = "Hysteresis Loops for Tulip Trees (F1–F6):",
       x = "Relative VPD (0–1)",
       y = "Relative Sapflow (0–1)") +
  theme_minimal()+ theme(strip.text = element_text(size = 10, face = "bold"),
                         plot.title = element_text(size = 14, face = "bold"),
                         axis.title = element_text(size = 12))

#Based on the above graph drop F2 because it's bizarre
sapflow_hourly<-sapflow_hourly %>% 
  filter(Sensor_ID !="F2")
  

#filter for wet soil
sunny_wet <- sapflow_hourly %>%
  filter(sunny_cloudy == "Sunny", wet_dry == "Wet")

#filter just for tulip trees for both plot
tulip_july_SC <- sunny_wet %>%
  filter(Sensor_ID %in% c(paste0("S", 1:6), paste0("C", 1:6),paste0("F",1:6)))

#calculate the average of tulip in sunny and wet for both plot
tulip_avg_SC <- tulip_july_SC %>%
  group_by(Plot, Hour) %>%
  summarise(
    mean_vpd = mean(relative_vpd, na.rm = TRUE),
    mean_sapflow = mean(relative_sapflow, na.rm = TRUE),
    .groups = "drop")
  
#label the plot by hours
library(grid)
label_hours <- c(6,8,16,20)
#join it to tulip average
tulip_labels <- tulip_avg_SC %>%
  filter(Hour %in% label_hours)

# Plot individual loops in gray, averages in bright color, faceted by Sensor_ID
library(grid)
ggplot() + geom_path(data = tulip_july_SC,
    aes(x = relative_vpd, y = relative_sapflow,
    group = interaction(Sensor_ID, Day), color = Plot),
    size = 0.4, alpha = 0.2) + 
  geom_path(data = tulip_avg_SC, aes(x = mean_vpd, y = mean_sapflow, 
  group = Plot, color = Plot),
  arrow = arrow(type = "closed", length = unit(0.15, "inches")), size = 1.5) +
  geom_text(data = tulip_labels,
            aes(x = mean_vpd, y = mean_sapflow, label = paste0(Hour, ":00"), color = Plot),
            size = 3, vjust = -0.5, nudge_y = 0.03, show.legend = FALSE) +
  scale_color_manual(values = c("S" = "orange", "C" = "purple","F"="lightblue")) +
  labs(
    title = "Salt stress reduces tree water use",
    x = "Relative VPD",
    y = "Relative Sapflow",
    color = "Plot") +
  theme_minimal(base_size = 11) + theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14))
ggsave("Salt_Control_Fresh_july24.png")  

#Filter out the species in salt plot to compare them
sapflow_hourly <- sapflow_hourly %>%
  mutate(Species = case_when(
    Sensor_ID %in% paste0("S", 1:6) ~ "Tulip",
    Sensor_ID %in% paste0("S", 7:12) ~ "Maple",
    Sensor_ID %in% paste0("S", 13:20) ~ "Beech"))

#Create a new data frame and add them
tulip_hourly <- filter(sapflow_hourly, Species == "Tulip")
maple_hourly <- filter(sapflow_hourly, Species == "Maple")
beech_hourly <- filter(sapflow_hourly, Species == "Beech")

#Combine all the species
combined_species <- bind_rows(
  tulip_hourly,
  maple_hourly,
  beech_hourly)

#Calculate the average
species_avg <- combined_species %>%
  group_by(Species, Hour) %>%
  summarise(
    mean_vpd = mean(relative_vpd, na.rm = TRUE),
    mean_sapflow = mean(relative_sapflow, na.rm = TRUE),
    .groups = "drop" )

#Plot the species to see the variability between them.
ggplot() + geom_path(data = combined_species,
            aes(x = relative_vpd, y = relative_sapflow,
            group = interaction(Sensor_ID, Day)),
            color = "lightgray", size = 0.3, alpha = 0.3) +
         geom_path(data = species_avg,
            aes(x = mean_vpd, y = mean_sapflow,
            group = Species,color = Hour), size = 1.2) +
  facet_wrap(~ Species) +
  scale_color_viridis_c(option = "inferno", name = "Hour") +
  labs(
    title = "Sapflow–VPD difference response between species",
    x = "Relative VPD",
    y = "Relative Sapflow") + theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 13, face = "bold"))
ggsave("Species_july24.png")

