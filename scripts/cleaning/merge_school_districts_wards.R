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
school_districts <- st_read("data/raw/big/Allegheny_County_School_District_Boundaries-shp/schooldistricts.shp") %>% 
  st_transform(4326) %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(school_district = SCHOOLD, geometry)

#council districts
council_districts <- st_read("data/raw/big/City_Council_Districts_2012-shp/Council_Districts.shp") %>% 
  st_transform(4326) %>% 
  mutate(center = st_centroid(geometry),
         lng = map_dbl(center, 1),
         lat = map_dbl(center, 2)) %>% 
  select(council_district = council_di, geometry) %>% 
  mutate(council_district = str_c("City Council District", council_district, sep = " "))


#enclave municipalities
municipalities <- st_read("data/raw/big/Allegheny_County_Municipal_Boundaries-shp/LandRecords_LANDRECORDS_OWNER_Municipalities.shp") %>% 
  st_transform(4326) %>% 
  clean_names() %>% 
  select(label, geometry)

mt_oliver <- municipalities %>% 
  filter(label == "Mount Oliver Borough")

#wards

wards <- st_read("data/raw/big/Wards-shp/Wards.shp") %>% 
  st_transform(4326) %>% 
  clean_names() %>% 
  select(ward_count, ward) %>% 
  rename(ward_name = ward_count) %>% 
  mutate(ward_centroid = st_point_on_surface(geometry))

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

wards <- 
  wards %>% 
  st_drop_geometry() %>% 
  st_set_geometry("ward_centroid") %>% 
  st_join(council_districts) %>% 
  st_drop_geometry() %>% 
  select(-ward)

# municipalities %>% 
#   distinct(label) %>% 
#   st_drop_geometry() %>% 
#   View()
# 
# school_districts %>% 
#   glimpse()
# 
# council_districts %>% 
#   glimpse()
# 
# school_districts %>% 
#   filter(school_district != "City of Pittsburgh") %>% 
#   ggplot() +
#   geom_sf(fill = "black", alpha = .6) +
#   geom_sf(data = council_districts, fill = "black", alpha = .6)
# 
# school_districts %>% 
#   leaflet() %>% 
#   addPolygons()
# 
# council_districts %>% 
#   leaflet() %>% 
#   addPolygons()
# 
# city_school <- school_districts %>% 
#   filter(school_district == "City of Pittsburgh") %>% 
#   summarize()
# 
# council_districts %>% 
#   filter(council_district == "City Council District 3" | council_district == "City Council District 4") %>% 
#   summarize() %>% 
#   st_difference(city_school) %>%
#   ggplot() +
#   geom_sf()
# 
# school_districts %>% 
#   filter(school_district != "City of Pittsburgh") %>% 
#   leaflet() %>% 
#   addPolygons(popup = ~school_district) %>% 
#   addPolygons(data = council_districts,
#               popup = ~council_district)
# 
# school_districts %>% 
#   filter(school_district != "City of Pittsburgh") %>% 
#   bind_rows(council_districts) %>% 
#   bind_rows(mt_oliver) %>% 
#   st_drop_geometry() %>% 
#   View()

unified_geo_ids <- school_districts %>% 
  filter(school_district != "City of Pittsburgh") %>% 
  bind_rows(council_districts) %>% 
  bind_rows(mt_oliver) %>% 
  left_join(wards) %>% 
  mutate(geo_id = case_when(
    
    #when council_district AND label are missing, use school_district
    is.na(council_district) & is.na(label) ~ school_district,
    
    #when school_district AND label are missing, use council_district
    is.na(school_district) & is.na(label) ~ council_district,
    
    #when school_district and council_district are missing, use label
    is.na(school_district) & is.na(council_district) ~ label)
    
  ) %>% 
  select(geo_id, school_district, council_district, ward_name, label, geometry) %>% 
  mutate(geo_id = str_squish(geo_id),
         ward_name = str_replace(ward_name, "PITTSBURGH", "Pittsburgh"),
         ward_name = str_remove(ward_name, "-"),
         ward_name = str_squish(ward_name))

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

list.files("data/cleaned/big/unified_geo_ids", full.names = T) %>% 
  map(file.remove)

unified_geo_ids %>% 
  st_write("data/cleaned/big/unified_geo_ids/unified_geo_ids.shp")
