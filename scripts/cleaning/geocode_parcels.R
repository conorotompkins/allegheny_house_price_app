library(tidyverse)
library(janitor)
library(vroom)
library(sf)
library(leaflet)
library(smoothr)

theme_set(theme_bw())

assessments_valid <- vroom("data/cleaned/big/clean_assessment_data.csv")

assessments_valid %>% 
  count(school_desc, sort = T)

assessments_valid %>% 
  filter(is.na(school_desc)) %>% 
  nrow()

#join parcel data against unified geo_ids
parcel_geo <- read_csv("data/cleaned/big/clean_parcel_geo.csv")

glimpse(parcel_geo)

#read in unified shape files for each school district + council district
unified_geo_id_crosswalk <- st_read("data/cleaned/big/unified_geo_ids/unified_geo_ids.shp") %>% 
  st_transform(crs = "NAD83") %>% 
  rename(school_district = schl_ds,
         council_district = cncl_ds,
         ward_name = ward_nm,
         municipality = mncplt_)

glimpse(unified_geo_id_crosswalk)

unified_geo_id_crosswalk %>%
  st_drop_geometry() %>% 
  count(geo_id, sort = T)

#create sf of geo_id boundaries
unified_geo_ids <- unified_geo_id_crosswalk %>% 
  group_by(geo_id) %>% 
  summarize()

unified_geo_ids %>%
  ggplot() +
  geom_sf()

unified_geo_ids %>%
  leaflet() %>%
  addPolygons(popup = ~geo_id)


joined_geo <- assessments_valid %>% 
  #create coordinates from longitude and latitude
  st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83") %>% 
  #join against unified geo id crosswalk based on intersection
  st_join(unified_geo_ids, join = st_intersects)

#find cases where st_join failed
missing_geo <- joined_geo %>% 
  filter(is.na(geo_id))

#map failures
missing_geo %>% 
  ggplot() +
  geom_sf(data = unified_geo_ids) +
  geom_sf()

#st_join based on nearest sf feature
missing_geo <- missing_geo %>% 
  select(-geo_id) %>% 
  st_join(unified_geo_ids, join = st_nearest_feature)

missing_geo %>% 
  st_drop_geometry() %>% 
  count(geo_id, sort = T)

#drop rows with missing geo_id from joined_geo
joined_geo <- joined_geo %>% 
  anti_join(st_drop_geometry(missing_geo), by = "par_id")
  
#add the rows back, left_join to get geo_id
joined_geo <- joined_geo %>% 
  bind_rows(missing_geo)
  
joined_geo %>% 
  st_drop_geometry() %>% 
  count(geo_id, sort = T)

joined_geo %>% 
  st_drop_geometry() %>% 
  filter(is.na(geo_id)) %>% 
  nrow()

joined_geo <- joined_geo %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(-center) %>% 
  st_drop_geometry()

glimpse(joined_geo)

joined_geo %>% 
  count(geo_id, sort = T) %>% 
  left_join(unified_geo_ids) %>%
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c()

joined_geo %>% 
  count(geo_id, sort = T) %>% 
  left_join(unified_geo_ids) %>%
  st_sf() %>% 
  leaflet() %>% 
  addPolygons(popup = ~geo_id)

joined_geo %>% 
  write_csv("data/cleaned/big/clean_assessment_data_geocoded.csv")