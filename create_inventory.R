#Script matching tree dbh with sapflow data

#Load packages
library(tidyr)
library(readr)
library(dplyr)
library(stringr)

#Load dataframes
sapflow_inventory <- read.csv("C:/Users/srin662/OneDrive - PNNL/Documents/R/TEMPEST-SULI-2024/sapflow_inventory.csv")
dbh_inventory <- read.csv("C:/Users/srin662/OneDrive - PNNL/Documents/R/inventory.csv")

#Edit format to match dataframes
sapflow_inventory %>%
  mutate(Plot = ifelse(Plot == "SW", "Saltwater", Plot)) %>%
  mutate(Plot = ifelse(Plot == "FW", "Freshwater", Plot)) -> sapflow_inventory

#For some reason, merging by Plot as well caused lots of matching issues. 
inventory <- merge(sapflow_inventory, dbh_inventory, 
                   by.x = c("Tree_ID"), 
                   by.y = c("Tag"), all.x = TRUE, all.y = TRUE)
inventory %>%
  mutate(Plot = Plot.x) %>%
  drop_na(Sapflux_ID) %>%
  filter(!grepl("D", Sapflux_ID)) -> inventory

saveRDS(inventory, file = "dbh.rds")
