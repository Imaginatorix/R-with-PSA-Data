library(readxl)
library(ggplot2)
library(dplyr)
library(sf)
library(geodata)

# https://www.youtube.com/watch?v=Qbkx3LCguyg
# Load in Data
col_names <- c("area_name", "private", "public", "hospital", "others", "unattended", "not_stated")
site_death <- read_excel("death/death_data.xlsx", 
                         sheet = "T17-modified", range = "A7:G122", col_names = FALSE, 
                         na = "-")
colnames(site_death) <- col_names
site_death <- filter(site_death, !is.na(site_death$area_name))
# Include attended total
site_death <- site_death %>%
  mutate(attended=private+public+hospital+others)
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

# Difference in number of deaths in attended vs not attended?
# Compare death difference each profession that attended
# Can you find anomalies on the deaths by attention per region?

# Map chart of Attended Deaths
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=attended, geometry=geometry)) +
  scale_fill_gradient(name="Attended Deaths", low="yellow", high="red", na.value="grey50")

# Map chart of Unattended Deaths
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=unattended, geometry=geometry)) +
  scale_fill_gradient(name="Unattended Deaths", low="yellow", high="red", na.value="grey50")

# Map chart of Not stated deaths
ggplot(st_as_sf(site_death_map)) +
  geom_sf(aes(fill=not_stated, geometry=geometry)) +
  scale_fill_gradient(name="Not stated deaths", low="yellow", high="red", na.value="grey50")

# Bar chart of deaths by attended-ness
attendedness <- c("attended", "unattended", "not_stated")
total_deaths <- 
