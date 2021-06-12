library(tidyverse)
library(vroom)

vroom("data/modelling/results/bag_full_model_results.csv") %>% 
  glimpse()

full_model_results <- vroom("data/modelling/results/bag_full_model_results.csv") %>% 
  mutate(.pred_dollar = 10^.pred) %>% 
  select(geo_id, style_desc, grade_desc, condition_desc,
         bedrooms, full_baths, half_baths,
         lot_area, finished_living_area, year_built,
         heat_type, ac_flag, sale_month,
         sale_price_adj, .pred_dollar)

full_model_results %>% 
  write_csv("shiny_app/trimmed_full_model_results.csv")
