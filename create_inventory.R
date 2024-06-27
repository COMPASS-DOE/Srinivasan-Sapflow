#Script matching tree dbh with sapflow data

#Load packages
library(tidyr)
library(readr)
library(dplyr)
library(stringr)

#Load dataframes
sapflow_inventory <- read.csv("sapflow_inventory.csv")
dbh_inventory <- read.csv("inventory.csv")

inventory <- merge(sapflow_inventory, dbh_inventory, 
                   by.x = "Tag", by.y = "Tag", all.x = TRUE, all.y = FALSE)
inventory %>%
  drop_na(Grid_Square) %>%
  filter(!grepl("D", Tree_Code)) -> inventory

saveRDS(inventory, file = "dbh.rds")
