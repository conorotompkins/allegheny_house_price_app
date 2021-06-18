library(tidyverse)
library(tidymodels)
library(baguette)
library(recipes)

library(hrbrthemes)
library(scales)
library(sf)
library(vroom)

model_fit <- read_rds("bag_model_fit_v.03.rds")

style_desc_distinct <- read_csv("style_desc_distinct.csv")
grade_desc_distinct <- read_csv("grade_desc_distinct.csv")
condition_desc_distinct <- read_csv("condition_desc_distinct.csv")

full_model_results <- vroom("trimmed_full_model_results.csv")

geo_id_shapes <- st_read("unified_geo_ids/unified_geo_ids.shp")