library(tidyverse)
library(janitor)
library(glue)
library(vroom)

assessments <- read_csv("data/cleaned/big/clean_assessment_data_geocoded.csv")
parcel_geo <- read_csv("data/cleaned/big/clean_parcel_geo.csv")

assessments %>% 
  count(geo_id, sort = T) %>% 
  write_csv("house_price_app/geo_id_distinct.csv")

assessments %>% 
  count(style_desc, sort = T) %>%
  write_csv("house_price_app/style_desc_distinct.csv")

assessments %>% 
  distinct(grade_desc) %>% 
  mutate(grade_desc = factor(grade_desc, levels = c("Highest Cost",
                                                    "Excellent",
                                                    "Very Good",
                                                    "Good",
                                                    "Average",
                                                    "Below Average",
                                                    "Poor"))) %>% 
  arrange(grade_desc) %>% 
  write_csv("house_price_app/grade_desc_distinct.csv")

assessments %>% 
  distinct(condition_desc) %>% 
  drop_na(condition_desc) %>% 
  mutate(condition_desc = factor(condition_desc, levels = c(
    "Excellent",
    "Very Good",
    "Good",
    "Average",
    "Fair",
    "Poor",
    "Very Poor",
    "Unsound"))) %>% 
  arrange(condition_desc) %>% 
  write_csv("house_price_app/condition_desc_distinct.csv")

#copy fit model from data/modelling/objects to shiny_app/
file.copy(from = "data/modelling/objects/bag_model_fit_v.03.rds",
          to = "house_price_app/bag_model_fit_v.03.rds")

copy_unified_geos_shapefiles <- function(){
  
  dir_exists_check <- dir.exists("house_price_app/unified_geo_ids")
  
  glue('"house_price_app/unified_geo_ids" exists:', dir_exists_check, .sep = " ") %>% 
    message()
  
  dir_populated <- list.files("house_price_app/unified_geo_ids") %>% 
    length() == 4
  
  glue('"house_price_app/unified_geo_ids" has 4 files:', dir_populated, .sep = " ") %>% 
    message()
  
  if(!dir_exists_check | !dir_populated){
    
    message("Creating directory and populating with shapefiles")
    
    dir.create("house_price_app/unified_geo_ids", recursive = T)
    
    list.files("data/cleaned/big/unified_geo_ids", full.names = T) %>% 
      map(file.copy, to = "house_price_app/unified_geo_ids")
  }
  
  else 
    
    message("Directory already exists with shapefiles")
  list.files("house_price_app/unified_geo_ids")
  
}

copy_unified_geos_shapefiles()

#trimmed model results
vroom("data/modelling/results/bag_full_model_results.csv") %>% 
  mutate(.pred_dollar = 10^.pred) %>% 
  select(geo_id, style_desc, grade_desc, condition_desc,
         bedrooms, full_baths, half_baths,
         lot_area, finished_living_area, year_built,
         heat_type, ac_flag, sale_month,
         sale_price_adj, .pred_dollar) %>% 
  write_csv("house_price_app/trimmed_full_model_results.csv")


# 
# assessments %>%
#   filter(lot_area == min(lot_area)) %>%
#   distinct(lot_area) %>%
#   write_csv("data/ui_input_values/lot_area_range_min.csv")
# 
# assessments %>%
#   filter(lot_area == quantile(lot_area, .7)) %>%
#   distinct(lot_area) %>%
#   write_csv("data/ui_input_values/lot_area_range_max.csv")
# 
# assessments %>%
#   filter(finished_living_area == min(finished_living_area)) %>%
#   distinct(finished_living_area) %>%
#   write_csv("data/ui_input_values/finished_living_area_min.csv")
# 
# assessments %>%
#   filter(finished_living_area == quantile(finished_living_area, .95)) %>%
#   distinct(finished_living_area) %>%
#   write_csv("data/ui_input_values/finished_living_area_max.csv")
# 
# assessments %>% 
#   filter(year_blt == min(year_blt)) %>% 
#   distinct(year_blt) %>% 
#   write_csv("data/ui_input_values/year_blt_min.csv")
# 
# assessments %>% 
#   filter(year_blt == max(year_blt)) %>% 
#   distinct(year_blt) %>% 
#   write_csv("data/ui_input_values/year_blt_max.csv")
# 
# assessments %>% 
#   select(geo_id, style_desc, sale_price_adj) %>% 
#   write_csv("data/ui_input_values/geo_id_style_desc.csv")
# 
# 
# #county shapefile
# list.files("data/ui_input_values/allegheny_county") %>% 
#   map(file.remove)
# 
# counties(state = "PA", cb = TRUE) %>% 
#   filter(NAME == "Allegheny") %>% 
#   st_write("data/ui_input_values/allegheny_county/allegheny_county.shp")
# 
# list.files("data/ui_input_values/allegheny_water") %>% 
#   map(file.remove)
# 
# tigris::area_water("PA", "Allegheny") %>% 
#   group_by(FULLNAME) %>% 
#   summarize(AWATER = sum(AWATER)) %>% 
#   ungroup() %>% 
#   arrange(desc(AWATER)) %>% 
#   slice(1:3) %>% 
#   st_write("data/ui_input_values/allegheny_water/allegheny_water.shp")
# 
# #create geo_id shapes 
# assessments_valid <- read_csv("data/clean_assessment_data.csv") %>% 
#   distinct(school_desc) %>% 
#   mutate(school_desc = str_squish(school_desc))
# 
# school_districts <- st_read("data/big/Allegheny_County_School_District_Boundaries-shp/schooldistricts.shp") %>% 
#   st_transform(4326) %>% 
#   mutate(center = st_centroid(geometry),
#          lng = map_dbl(center, 1),
#          lat = map_dbl(center, 2)) %>% 
#   select(school_district = SCHOOLD, geometry)
# 
# council_districts <- st_read("data/big/City_Council_Districts_2012-shp/Council_Districts.shp") %>% 
#   st_transform(4326) %>% 
#   mutate(center = st_centroid(geometry),
#          lng = map_dbl(center, 1),
#          lat = map_dbl(center, 2)) %>% 
#   select(council_district = council_di, geometry) %>% 
#   mutate(council_district = str_c("City Council District", council_district, sep = " "))
# 
# municipalities <- st_read("data/big/Allegheny_County_Municipal_Boundaries-shp/LandRecords_LANDRECORDS_OWNER_Municipalities.shp") %>% 
#   st_transform(4326) %>% 
#   clean_names() %>% 
#   select(label, geometry)
# 
# mt_oliver <- municipalities %>% 
#   filter(label == "Mount Oliver Borough")
# 
# unified_geo_ids <- school_districts %>% 
#   filter(school_district != "City of Pittsburgh") %>% 
#   bind_rows(council_districts) %>% 
#   bind_rows(mt_oliver) %>% 
#   mutate(geo_id = case_when(is.na(council_district) & is.na(label) ~ school_district,
#                             is.na(school_district) & is.na(label) ~ council_district,
#                             is.na(school_district) & is.na(council_district) ~ label)
#   ) %>% 
#   select(geo_id, geometry) %>% 
#   mutate(geo_id = str_squish(geo_id)) %>% 
#   mutate(center = map(geometry, st_centroid),
#          lng = map_dbl(center, 1),
#          lat = map_dbl(center, 2)) %>%
#   select(-center)
# 
# list.files("data/ui_input_values/unified_geo_ids", full.names = T) %>% 
#   map(file.remove)
# 
# unified_geo_ids %>% 
#   st_write("data/ui_input_values/unified_geo_ids/unified_geo_ids.shp")
# 

# st_read("data/ui_input_values/allegheny_county.shp") %>% 
#   ggplot() +
#   geom_sf()
# 
# st_read("data/ui_input_values/allegheny_water.shp") %>% 
#   ggplot() +
#   geom_sf()
# 
# #create school district shapefiles
# keystone_oaks_geo <- assessments %>% 
#   filter(school_desc == "Keystone Oaks") %>% 
#   left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
#   distinct(school_desc, muni_desc, longitude, latitude) %>% 
#   drop_na(longitude, latitude) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   group_by(school_desc, muni_desc) %>% 
#   summarize(geometry = st_combine(geometry)) %>%
#   ungroup() %>% 
#   st_convex_hull() %>% 
#   group_by(school_desc) %>%
#   summarize() %>% 
#   ungroup()
# 
# everything_else_geo <- assessments %>% 
#   filter(school_desc != "Keystone Oaks") %>% 
#   left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
#   distinct(school_desc, longitude, latitude) %>% 
#   drop_na(longitude, latitude) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   group_by(school_desc) %>% 
#   summarize(geometry = st_combine(geometry)) %>%
#   ungroup() %>% 
#   st_convex_hull() %>% 
#   group_by(school_desc) %>%
#   summarize() %>% 
#   ungroup()
# 
# school_district_shapes <- everything_else_geo %>%
#   bind_rows(keystone_oaks_geo) %>% 
#   st_difference()
# 
# list.files("data/ui_input_values/school_district_shapes/", full.names = TRUE) %>% 
#   set_names() %>% 
#   map(file.remove)
#   
# school_district_shapes %>% 
#   st_write("data/ui_input_values/school_district_shapes/school_district_shapes.shp")


#updated shapefiles
# 
# list.files("data/ui_input_values/school_district_shapes/", full.names = TRUE) %>%
#   set_names() %>%
#   map(file.remove)
# 
# keystone_oaks_geo <- assessments %>% 
#   filter(school_desc == "Keystone Oaks") %>% 
#   left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
#   distinct(school_desc, muni_desc, longitude, latitude) %>% 
#   drop_na(longitude, latitude) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# 
# everything_else_geo <- assessments %>% 
#   filter(school_desc != "Keystone Oaks") %>% 
#   left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
#   distinct(school_desc, longitude, latitude) %>% 
#   drop_na(longitude, latitude) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# 
# everything_else_shapes <- everything_else_geo %>% 
#   group_by(school_desc) %>% 
#   nest() %>% 
#   mutate(hulls = map(data, concaveman, concavity = 3)) %>%
#   unnest(cols = c(hulls)) %>% 
#   ungroup() %>% 
#   select(-data) %>% 
#   st_as_sf() %>% 
#   st_difference()
# 
# keystone_oaks_shapes <- keystone_oaks_geo %>% 
#   group_by(school_desc, muni_desc) %>% 
#   nest() %>% 
#   mutate(hulls = map(data, concaveman, concavity = 3)) %>%
#   unnest(cols = c(hulls)) %>% 
#   ungroup() %>% 
#   st_as_sf() %>% 
#   summarize(school_desc = unique(school_desc))
# 
# school_district_shapes <- everything_else_shapes %>% 
#   #st_difference(keystone_oaks_shapes) %>% 
#   bind_rows(keystone_oaks_shapes) %>% 
#   st_difference() %>% 
#   mutate(center = map(polygons, st_centroid),
#          lng = map_dbl(center, 1),
#          lat = map_dbl(center, 2)) %>% 
#   select(-center)
# 
# school_district_shapes %>% 
#   st_write("data/ui_input_values/school_district_shapes/school_district_shapes.shp")

