library(readxl)
library(ggplot2)
library(dplyr)
library(sf)
library(geodata)

# https://www.youtube.com/watch?v=Qbkx3LCguyg
# Load in Data
col_names <- c("region", "health_facilities", "others", "not_stated")
site_death <- read_excel("death/death_data.xlsx", 
                         sheet = "T9-Region", range = "A7:E23", 
                         col_names = FALSE)
colnames(site_death) <- col_names

# Get geographic data
gadm(country="PHL", path="./death/geo/", level=2)
geo_data <- readRDS(paste(getwd(), "/death/geo/gadm/gadm41_PHL_1_pk.rds", sep=""))

# https://stackoverflow.com/questions/73825468/converting-spatvector-objects-to-data-frames-for-use-in-ggplot2
geo_data_df <- st_as_sf(geo_data)

test <- sample(1:100, nrow(geo_data_df), replace=TRUE)
geo_data_df$test <- test

ggplot(geo_data_df) +
  geom_sf(aes(fill=test), color="black")




