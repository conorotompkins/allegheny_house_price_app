#get school district shapefile
#get city ward shapefile
#merge them

library(tidyverse)
library(janitor)
library(sf)
library(leaflet)
library(smoothr)

theme_set(theme_bw())

#assessments
assessments_valid <- read_csv("data/cleaned/big/clean_assessment_data.csv") %>% 
  distinct(school_desc) %>% 
  mutate(school_desc = str_squish(school_desc))

#school districts
# from https://data.wprdc.org/dataset/allegheny-county-school-district-boundaries
school_districts <- st_read("data/raw/big/alcogisallegheny-county-school-district-boundaries/schooldistricts.shp") %>% 
  st_transform(4326) %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(school_district = SCHOOLD, geometry)

school_districts %>% 
  ggplot() +
  geom_sf()

#council districts
#from https://data.wprdc.org/dataset/city-council-districts-2012
council_districts <- st_read("data/raw/big/council_districs/Council_Districts.shp") %>% 
  st_transform(4326) %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(council_district = council_di, geometry) %>% 
  mutate(council_district = str_c("City Council District", council_district, sep = " "))


#enclave municipalities
#from https://data.wprdc.org/dataset/allegheny-county-municipal-boundaries
municipalities <- st_read("data/raw/big/alcogisallegheny-county-municipal-boundaries/LandRecords_LANDRECORDS_OWNER_Municipalities.shp") %>% 
  st_transform(4326) %>% 
  clean_names() %>% 
  rename(municipality_name = label) %>% 
  select(municipality_name, geometry)

mt_oliver <- municipalities %>% 
  filter(municipality_name == "Mount Oliver Borough")

#wards
#from
wards <- st_read("data/raw/big/pittsburghpawards/Wards.shp") %>% 
  st_transform(4326) %>% 
  clean_names() %>% 
  select(ward_count, ward) %>% 
  rename(ward_name = ward_count) %>% 
  mutate(ward_centroid = st_point_on_surface(geometry))

#confirm centroids exist in correct wards
wards %>% 
  ggplot() +
  geom_sf() +
  geom_sf(aes(geometry = ward_centroid))

wards %>% 
  st_drop_geometry() %>% 
  st_set_geometry("ward_centroid") %>% 
  st_join(council_districts) %>% 
  ggplot() +
  geom_sf(data = council_districts) +
  geom_sf(aes(color = council_district))

#join wards to council districts
wards <- wards %>% 
  st_drop_geometry() %>% 
  st_set_geometry("ward_centroid") %>% 
  st_join(council_districts) %>% 
  st_drop_geometry() %>% 
  select(-ward)

municipalities %>%
  distinct(municipality_name) %>%
  st_drop_geometry() %>%
  View()

school_districts %>%
  glimpse()

council_districts %>%
  glimpse()

school_districts %>%
  filter(school_district != "City of Pittsburgh") %>%
  ggplot() +
  geom_sf(fill = "black", alpha = .6) +
  geom_sf(data = council_districts, fill = "black", alpha = .6)

school_districts %>%
  leaflet() %>%
  addPolygons()

council_districts %>%
  leaflet() %>%
  addPolygons()

city_school <- school_districts %>%
  filter(school_district == "City of Pittsburgh") %>%
  summarize()

council_districts %>%
  filter(council_district == "City Council District 3" | council_district == "City Council District 4") %>%
  summarize() %>%
  st_difference(city_school) %>%
  ggplot() +
  geom_sf()

school_districts %>%
  filter(school_district != "City of Pittsburgh") %>%
  leaflet() %>%
  addPolygons(popup = ~school_district) %>%
  addPolygons(data = council_districts,
              popup = ~council_district)

school_districts %>%
  filter(school_district != "City of Pittsburgh") %>%
  bind_rows(council_districts) %>%
  bind_rows(mt_oliver) %>%
  st_drop_geometry() %>%
  View()

unified_geo_ids <- school_districts %>% 
  filter(school_district != "City of Pittsburgh") %>% 
  bind_rows(council_districts) %>% 
  bind_rows(mt_oliver) %>% 
  left_join(wards) %>% 
  mutate(geo_id = case_when(
    
    #when council_district AND label are missing, use school_district
    is.na(council_district) & is.na(municipality_name) ~ school_district,
    
    #when school_district AND label are missing, use council_district
    is.na(school_district) & is.na(municipality_name) ~ council_district,
    
    #when school_district and council_district are missing, use label
    is.na(school_district) & is.na(council_district) ~ municipality_name)
    
  ) %>% 
  select(geo_id, school_district, council_district, ward_name, municipality_name, geometry) %>% 
  mutate(geo_id = str_squish(geo_id),
         ward_name = str_replace(ward_name, "PITTSBURGH", "Pittsburgh"),
         ward_name = str_remove(ward_name, "-"),
         ward_name = str_squish(ward_name)) %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2))

unified_geo_ids %>% 
  st_drop_geometry() %>% 
  distinct(ward_name)

unified_geo_ids %>% 
  group_by(geo_id) %>% 
  summarize() %>% 
  leaflet() %>% 
  addPolygons(popup = ~geo_id)

unified_geo_ids %>% 
  st_drop_geometry() %>% 
  View()

unlink("data/cleaned/big/unified_geo_ids", recursive = T)

dir.create("data/cleaned/big/unified_geo_ids")

unified_geo_ids %>% 
  st_write("data/cleaned/big/unified_geo_ids/unified_geo_ids.shp")
