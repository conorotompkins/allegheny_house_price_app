library(tidyverse)
library(janitor)
library(vroom)
library(sf)
library(leaflet)
library(smoothr)

theme_set(theme_bw())

assessments_valid <- vroom("data/cleaned/big/clean_assessment_data.csv")

assessments_valid %>% 
  count(school_desc, sort = T) %>% 
  View()

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
         municipality_enclave = label)

glimpse(unified_geo_id_crosswalk)

unified_geo_id_crosswalk %>% 
  count(ward_name)

unified_geo_id_crosswalk %>%
  st_drop_geometry() %>% 
  count(geo_id, sort = T) %>% 
  View()

unified_geo_ids <- unified_geo_id_crosswalk %>% 
  group_by(geo_id) %>% 
  summarize()

unified_geo_ids %>%
  ggplot() +
  geom_sf()

unified_geo_ids %>%
  leaflet() %>%
  addPolygons(popup = ~geo_id)

#sales of houses that aren't in the parcel geometry file
missing_geo_1 <- assessments_valid %>% 
  anti_join(parcel_geo, by = c("par_id" = "pin"))

missing_geo_1 %>% 
  count(school_desc, sort = T)

#sales of houses that are in the parcel geometry file
joined_geo <- assessments_valid %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  drop_na(longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83") %>% 
  st_join(unified_geo_ids) %>% 
  st_drop_geometry()

#sales of houses with missing geo_ids
missing_geo_2 <- joined_geo %>% 
  filter(is.na(geo_id)) %>% 
  select(-geo_id)

#remove sales of houses with missing geo_ids from joined_geo
joined_geo %>% 
  semi_join(missing_geo_1, by = "par_id") %>% 
  nrow() #should be 0

joined_geo %>% 
  semi_join(missing_geo_2, by = "par_id") %>% 
  nrow() #should be whatever had missing geo_id in  joined_geo

joined_geo <- joined_geo %>% 
  anti_join(missing_geo_1, by = "par_id") %>% 
  anti_join(missing_geo_2, by = "par_id")

unified_geo_id_crosswalk %>% 
  st_drop_geometry() %>% 
  distinct(ward_name)

missing_geo_combined <- bind_rows(missing_geo_1, missing_geo_2)

missing_geo_combined <- missing_geo_combined %>% 
  left_join(unified_geo_id_crosswalk %>% 
              st_drop_geometry() %>% 
              select(geo_id, ward_name), 
            by = c("school_desc" = "ward_name")) %>% 
  mutate(geo_id = coalesce(geo_id, school_desc)) %>% 
  mutate(geo_id = case_when(#school_desc == "19th Ward Pittsburgh" ~ "City Council District 4",
    #school_desc == "Woodland Hills" ~ "Woodland Hills",
    #school_desc == "Chartiers Valley" ~ "Chartiers Valley",
    #school_desc == "Bethel Park" ~ "Bethel Park",
    #school_desc == "Moon Area" ~ "Moon Area",
    #school_desc == "West Jefferson Hills" ~ "West Jefferson Hills",
    school_desc == "Plum Boro" ~ "Plum Borough",
    #school_desc == "South Park" ~ "South Park",
    school_desc == "Upper St Clair" ~ "Upper St. Clair Area",
    TRUE ~ geo_id))

missing_geo_combined %>% 
  filter(is.na(geo_id)) %>% 
  distinct(school_desc, geo_id)

missing_geo_combined %>% 
  distinct(geo_id, school_desc, muni_desc) %>% 
  View()

joined_geo %>% 
  semi_join(missing_geo_combined, by = "par_id") %>% 
  nrow() #should be 0

updated_assessments <- bind_rows(joined_geo, missing_geo_combined)

missing_geo_combined %>% 
  anti_join(updated_assessments, by = "par_id") %>% 
  nrow() #should be 0

# updated_assessments %>% 
#   count(geo_id, sort = T) %>% 
#   View()

updated_assessments %>% 
  filter(is.na(geo_id)) %>% 
  distinct(school_desc)

updated_assessments %>% 
  count(geo_id, sort = T) %>% 
  left_join(unified_geo_ids) %>%
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c()

updated_assessments %>% 
  count(geo_id, sort = T) %>% 
  left_join(unified_geo_ids) %>%
  st_sf() %>% 
  leaflet() %>% 
  addPolygons(popup = ~geo_id)

updated_assessments %>% 
  write_csv("data/cleaned/big/clean_assessment_data_geocoded.csv")
