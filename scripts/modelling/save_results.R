library(tidyverse)
library(tidymodels)
library(baguette)
library(recipes)
library(hrbrthemes)

theme_set(theme_ipsum())

housing_sales <- read_csv("data/cleaned/big/clean_housing_sales.csv")

bag_fit <- read_rds("data/modelling/objects/bag_model_fit_v.03.rds")

full_results <- bag_fit %>%
  predict(housing_sales) %>%
  bind_cols(housing_sales) %>% 
  mutate(model = "bagged tree")

geo_id_rsq <- full_results %>% 
  mutate(geo_id = fct_lump_min(geo_id, 500, other_level = "Other")) %>% 
  group_by(geo_id) %>% 
  rsq(truth = sale_price_adj, estimate = 10^.pred) %>% 
  ungroup() %>% 
  left_join(housing_sales %>% 
              count(geo_id, sort = T), by = "geo_id")

geo_id_rsq %>% 
  ggplot(aes(n, .estimate)) +
  geom_point()

bagged_rsq_chart <- geo_id_rsq %>% 
  mutate(geo_id = fct_reorder(geo_id, .estimate)) %>% 
  ggplot(aes(.estimate, geo_id)) +
  geom_point() +
  labs(title = "R-squared by geo_id",
       subtitle = 'geo_id with < 500 lumped into "Other"',
       x = "R-squared")

bagged_rsq_chart %>% 
  ggsave(filename = "output/images/bagged_rsq_chart.png",
         width = 8, height = 12)

#bag model full results
full_results %>% 
  write_csv("data/modelling/results/bag_full_model_results.csv")
