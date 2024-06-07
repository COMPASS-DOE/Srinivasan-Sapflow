
#June 2nd 2024
#A short script for getting oriented with TEMPEST's sapflow data
#initial author: K. A. Morris
#for: R. Srinivasan

library(dplyr)
library(stringr)
library(lubridate)

#saved as an R data file
sapflow_data <- readRDS("sapflow.rds")
#from create timeseries script
#sapflow_data <- bind_rows(sap2.5, sap5)

#write.csv(sapflow_data, "TEMPEST_2024_sapflow.csv")
#sapflow_data <- read.csv("TEMPEST_2024_sapflow.csv")
#results in timestamp issues




#let's see what we've got
str(sapflow_data)

#targeting spring of this year as an initial orientation
sapflow_data %>%
  mutate(month = month(TIMESTAMP)) %>%
  filter(month %in% c(3:5)) -> spring_sapflow

#removing full data file for memory
rm(sapflow_data)

#deep probe data
spring_sapflow %>%
  filter(research_name == "sapflow_5cm") -> deep_sap

str(deep_sap)
#tree id's have "D" behind them

#this creates a new column with the tree id  
deep_sap$tree_id <- str_sub(deep_sap$Sensor_ID, 1, str_length(deep_sap$Sensor_ID) - 1)

#create a list of trees with deep probes
#this can be used for filtering the shallow probe data
#as some trees have both
two_probe_trees <- unique(deep_sap$tree_id)

#shallow probe data
spring_sapflow %>%
  filter(research_name == "sapflow_2.5cm") %>%
  mutate(tree_id = Sensor_ID) -> shallow_sap

#create a dataframe with both
shallow_sap %>%
  filter(tree_id %in% two_probe_trees) %>%
  bind_rows(deep_sap) -> sap_spring2p

str(sap_spring2p)
#looks pretty good

library(ggplot2)

ggplot(sap_spring2p, aes(TIMESTAMP, Value,
                shape = research_name,
                color = tree_id)) +
  geom_point() + facet_grid(Plot~month, scales = "free_x")

#wow! That's a lot of data
#May doesn't have much, let's try just one month

ggplot(sap_spring2p[sap_spring2p$month == 4,], aes(TIMESTAMP, Value,
                         shape = research_name,
                         color = tree_id)) +
  geom_point() + facet_grid(Plot~., scales = "free_x")



#hold for now: Next Steps
#now that we have moderately clean data
#try working through the TREX vignette
#https://github.com/the-Hull/TREX