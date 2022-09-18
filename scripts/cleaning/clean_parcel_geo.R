library(tidyverse)
library(vroom)
library(janitor)
library(hrbrthemes)

theme_set(theme_ipsum())

#options(scipen = 999, digits = 4)

#from https://data.wprdc.org/dataset/parcel-centroids-in-allegheny-county-with-geographic-identifiers/resource/adf1fd38-c374-4c4e-9094-5e53bd12419f
#ned to see why this is not returning 0 rows. double precision?
vroom("data/raw/big/parcelcentroid2022_08wgs.csv") %>% 
  clean_names() %>% 
  distinct(longitude, latitude, intptlon10, intptlat10) %>% 
  filter(longitude != intptlon10)

parcel_geo <-  vroom("data/raw/big/parcelcentroid2022_08wgs.csv") %>% 
  clean_names() %>% 
  select(pin, intptlon10, intptlat10) %>% 
  rename(longitude = intptlon10,
         latitude = intptlat10) %>% 
  drop_na()

glimpse(parcel_geo)

parcel_geo %>% 
  write_csv("data/cleaned/big/clean_parcel_geo.csv")

parcel_geo %>%
  ggplot(aes(longitude, latitude)) +
  geom_density_2d_filled()
