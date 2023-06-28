library(readxl)
library(ggplot2)
library(dplyr)
library(sf)
library(geodata)

# https://www.youtube.com/watch?v=Qbkx3LCguyg
# Load in Data
col_names <- c("area_name", "health_facilities", "home", "others", "not_stated")
site_death <- read_excel("death/death_data.xlsx", 
                         sheet = "T9-modified", range = "A7:E122", col_names = FALSE, 
                         na = "-")
colnames(site_death) <- col_names
site_death <- filter(site_death, !is.na(site_death$area_name))
# Include total
site_death <- site_death %>%
                mutate(total=health_facilities+home+others+not_stated)
# Reformat the city names
site_death$area_name <- gsub("^City of (.*)", "\\1 City", site_death$area_name)


# Get geographic data
for (i in 1:3){
  gadm(country="PHL", path="./death/geo/", level=i)
}
geo_data_level_1 <- readRDS(paste(getwd(), "/death/geo/gadm/gadm41_PHL_1_pk.rds", sep="")) # Province
geo_data_level_2 <- readRDS(paste(getwd(), "/death/geo/gadm/gadm41_PHL_2_pk.rds", sep="")) # Municipality/City
geo_data_level_3 <- readRDS(paste(getwd(), "/death/geo/gadm/gadm41_PHL_3_pk.rds", sep="")) # Village

# https://stackoverflow.com/questions/73825468/converting-spatvector-objects-to-data-frames-for-use-in-ggplot2
geo_data_level_1_df <- st_as_sf(geo_data_level_1)
geo_data_level_2_df <- st_as_sf(geo_data_level_2)
geo_data_level_3_df <- st_as_sf(geo_data_level_3)

# Rename respective NAME_* to area_name
names(geo_data_level_1_df)[names(geo_data_level_1_df) == "NAME_1"] <- "area_name"
names(geo_data_level_2_df)[names(geo_data_level_2_df) == "NAME_2"] <- "area_name"
names(geo_data_level_3_df)[names(geo_data_level_3_df) == "NAME_3"] <- "area_name"

# Merge respective data into the data frame (i.e. add the data to be graphed to the geo_data)
# By adding geometry to the data in excel
# This assumes that if there is a larger unit in the column, then no smaller unit it contains will also contain in that column
# (i.e. If Province Abra is in the column, then Municipality Bangued can't be in the column,
# however, Municipality Buenavista is allowed is it is not in Abra)
merge_df <- function(df1, df2, name){
  # Only work on="geometry"
  # Merge the data
  df3 <- left_join(df1, df2, by=name, multiple="any")
  # Merge the columns
  common <- intersect(names(df1), names(df2))
  if (length(common) > 1){
    # https://r-spatial.org/r/2017/03/19/invalid.html
    # for st_is_empty
    df3 <- df3 %>%
            mutate(geometry=if_else(!st_is_empty(geometry.y), geometry.y, geometry.x)) %>% 
            select(-starts_with("geometry."))
  }
  return (df3)
}

# Start with smaller so that it would be replaced by something more popular/bigger/most likely to be unique
site_death_map <- site_death %>%
          merge_df(select(geo_data_level_3_df, area_name, geometry), name="area_name") %>%
          merge_df(select(geo_data_level_2_df, area_name, geometry), name="area_name") %>%
          merge_df(select(geo_data_level_1_df, area_name, geometry), name="area_name")
# I know there are still some values that were actually included in the gadm data
# But were not included in the site_death_map
# For _now_, let's just continue with it

# Can you find anomalies on the number of deaths on site per region?
# Explain possible reason

# Map chart of Deaths at Health Facilities
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=health_facilities, geometry=geometry)) +
  scale_fill_gradient(name="Deaths at Health Facilities", low="yellow", high="red", na.value="grey50")
# What happened at Cavite???
# Are there more hospitals there?
# Was there an isolation event in there which caused mass death? Like COVID

# Map chart of Deaths at Home
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=home, geometry=geometry)) +
  scale_fill_gradient(name="Deaths at Home", low="yellow", high="red", na.value="grey50")

# Map chart of Deaths at Other sites
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=others, geometry=geometry)) +
  scale_fill_gradient(name="Deaths at Other sites", low="yellow", high="red", na.value="grey50")

# Map chart of Deaths at Not Stated sites
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=not_stated, geometry=geometry)) +
  scale_fill_gradient(name="Deaths at Not Stated sites", low="yellow", high="red", na.value="grey50")

# Map chart of Total Deaths
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=total, geometry=geometry)) +
  scale_fill_gradient(name="Total Deaths", low="yellow", high="red", na.value="grey50")


