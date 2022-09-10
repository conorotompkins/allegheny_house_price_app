library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)
library(hrbrthemes)
library(priceR)
library(sf)
library(leaflet)
library(skimr)

#new data source
#with centroid lat lon
#https://data.wprdc.org/dataset/property-data-with-geographic-identifiers/resource/2072321e-aa7c-486d-8b14-8ae79363cb68

#old data source
#https://data.wprdc.org/dataset/property-assessments/resource/f2b8d575-e256-4718-94ad-1e12239ddb92

theme_set(theme_ipsum())

#options(scipen = 999, digits = 4)

assessments <- vroom("https://tools.wprdc.org/downstream/2072321e-aa7c-486d-8b14-8ae79363cb68",
                     col_types = cols(.default = "c")) %>% 
  clean_names() %>% 
  rename(par_id = parid)

glimpse(assessments)

names(assessments) <- names(assessments) %>% 
  make_clean_names(replace = c("property" = "property_",
                               "school" = "school_",
                               "muni" = "muni_",
                               "record" = "record_",
                               "sale" = "sale_",
                               "deed" = "deed_",
                               "instr" = "instr_",
                               "style" = "style_",
                               "year" = "year_",
                               "total" = "total_",
                               "finished" = "finished_",
                               "lot" = "lot_",
                               "grade" = "grade_",
                               "condition" = "condition_",
                               "finished_livingarea" = "finished_living_area",
                               "cdu" = "cdu_",
                               "basement" = "basement_",
                               "roof" = "roof_",
                               "heatingcooling" = "heating_cooling_",
                               "fullbaths" = "full_baths",
                               "halfbaths" = "half_baths",
                               "bsmtgarage" = "basement_garage"))


glimpse(assessments)

# assessments %>%
#   filter(muni_desc == "Mt. Oliver") %>% 
#   #filter(str_detect(muni_desc, "Oliver")) %>% 
#   count(muni_desc, school_desc, sort = T) %>% 
#   View()

usedesc_top10 <- assessments %>% 
  filter(sale_desc == "VALID SALE" | sale_desc == "OTHER VALID") %>% 
  count(usedesc, sort = T) %>% 
  slice(1:10)

usedesc_other <- assessments %>% 
  filter(sale_desc == "VALID SALE" | sale_desc == "OTHER VALID") %>% 
  anti_join(usedesc_top10) %>% 
  count(usedesc, sort = T) %>% 
  filter(usedesc %in% c("OFFICE/APARTMENTS OVER", "MOBILE HOME",
                        "APART:20-39 UNITS", "APART:40+ UNITS",
                        "CHARITABLE EXEMPTION/HOS/HOMES", "MOBILE HOME (IN PARK)",
                        "COMM APRTM CONDOS 5-19 UNITS"))

allowed_usedesc <- bind_rows(usedesc_top10, usedesc_other) %>% 
  select(-n)

assessments_valid <- assessments %>% 
  filter(sale_desc == "VALID SALE" | sale_desc == "OTHER VALID") %>% 
  semi_join(allowed_usedesc)

assessments_valid <- assessments_valid %>% 
  select(par_id, usedesc, school_desc, muni_desc, sale_desc, sale_price, sale_date,
         year_blt, style_desc, bedrooms, full_baths, half_baths, finished_living_area,
         lot_area, grade_desc, condition_desc,
         extfinish_desc, roof_desc, basement_desc, cdu_desc, heating_cooling_desc,
         fireplaces, basement_garage,
         latitude, longitude) %>% 
  mutate(sale_date = mdy(sale_date),
         sale_year = year(sale_date),
         sale_month = month(sale_date, label = T),
         year_blt = parse_number(year_blt),
         sale_price = parse_number(sale_price)) %>%
  rename(year_built = year_blt) %>% 
  mutate(muni_desc = str_trim(muni_desc),
         school_desc = str_trim(school_desc)) %>% 
  filter(sale_year > 1975,
         sale_price > 100) %>%
  filter(par_id != "0014G00199000000")

glimpse(assessments_valid)

assessments_valid %>%
  skimr::skim()

#simplify condo and row end style desc types
assessments_valid <- assessments_valid %>% 
  mutate(style_desc = case_when(str_detect(style_desc, "CONDO") ~ "CONDO",
                                TRUE ~ style_desc),
         style_desc = case_when(style_desc == "ROW END" | style_desc == "ROW INTERIOR" ~ "ROW",
                                TRUE ~ style_desc))

assessments_valid <- assessments_valid %>% 
  mutate(school_desc = case_when(muni_desc == "Mt. Oliver" ~ "Mt. Oliver",
                                 TRUE ~ school_desc)) %>% 
  mutate(school_desc_clean = case_when(str_detect(muni_desc, "Ward") & str_detect(muni_desc, "PITTSBURGH")  ~ muni_desc,
                                       TRUE ~ school_desc),
         school_desc_clean = str_to_title(school_desc_clean),
         school_desc_clean = str_replace(school_desc_clean, " - ", " ")) %>% 
  select(-c(school_desc)) %>% 
  rename(school_desc = school_desc_clean) %>% 
  mutate(house_age_at_sale = sale_year -  year_built) %>% 
  mutate(school_desc = str_to_title(school_desc)) %>% 
  select(-sale_date)

assessments_valid <- assessments_valid %>% 
  filter(house_age_at_sale >= 0)

#clean up grade_desc and condition_desc text
assessments_valid <- assessments_valid %>% 
  mutate(across(.cols = matches("_desc$"), str_to_title)) %>% 
  mutate(across(.cols = c(grade_desc, condition_desc), ~str_remove(.x, "\\+|\\-"))) %>% 
  mutate(across(.cols = c(grade_desc, condition_desc, style_desc), str_squish)) %>% 
  mutate(heating_cooling_desc = str_replace(heating_cooling_desc, "With Ac", "With AC"))

#clean up heating_cooling_desc and create ac_flag
assessments_valid %>% 
  count(heating_cooling_desc, sort = T)

assessments_valid <- assessments_valid %>% 
  mutate(ac_flag = str_detect(heating_cooling_desc, "With AC$"),
         heat_type = str_remove(heating_cooling_desc, "With AC$") %>% str_squish,
         heat_type = case_when(heat_type == "No Heat But" ~ "None",
                               TRUE ~ heat_type))

assessments_valid %>% 
  count(ac_flag, heat_type, heating_cooling_desc, sort = T)

inflation_lookup <- assessments_valid %>% 
  distinct(sale_price, sale_year) %>% 
  mutate(sale_price_adj = NA)

inflation_lookup$sale_price_adj <- adjust_for_inflation(inflation_lookup$sale_price, 
                                                         from_date = inflation_lookup$sale_year, 
                                                         country = "US", 
                                                         #set everything to 2020 dollars
                                                         to_date = 2020)
assessments_valid %>% 
  left_join(inflation_lookup) %>% 
  select(sale_price, sale_price_adj) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density() +
  facet_wrap(~name, ncol = 1)

glimpse(assessments_valid)

assessments_valid %>% 
  left_join(inflation_lookup) %>% 
  write_csv("data/cleaned/big/clean_assessment_data.csv")
