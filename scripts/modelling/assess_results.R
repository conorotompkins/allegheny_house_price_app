library(tidyverse)
library(tidymodels)
library(hrbrthemes)
library(baguette)
library(vip)
library(butcher)

theme_set(theme_ipsum())

housing_sales <- read_csv("data/cleaned/big/clean_housing_sales.csv")

set.seed(1234)

# Put 3/4 of the data into the training set 
data_split <- initial_split(housing_sales, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

bag_fit <- read_rds("data/modelling/objects/bag_model_fit_v.03.rds")

bag_fit

#compare predictions against training data across models
test_predictions <- bag_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>% 
  mutate(model = "bagged tree")

test_predictions_scatter_bagged <- test_predictions %>% 
  ggplot(aes(sale_price_adj, .pred)) +
  geom_density_2d_filled() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1) +
  labs(title = "Test Data")

test_predictions_scatter_bagged %>% 
  ggsave(filename = "output/images/test_predictions_scatter_bagged.png",
         width = 12,
         height = 12)

#eda results
bagged_tree_vi <- bag_fit %>% 
  extract_fit_parsnip() %>% 
  .[['fit']] %>%
  var_imp()

bagged_tree_vi_chart <- bagged_tree_vi %>% 
  mutate(term_type = case_when(str_detect(term, "^geo_id") ~ "geo_id",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               str_detect(term, "^sale_month_") ~ "sale_month",
                               TRUE ~ "other")) %>% 
  mutate(term = str_remove(term, term_type),
         term = str_remove(term, "^_")) %>% 
  mutate(term_type = fct_reorder(term_type, value, .fun = max, .desc = T)) %>% 
  mutate(term = tidytext::reorder_within(term, value, term_type)) %>%
  ggplot(aes(value, term)) +
  geom_point() +
  facet_wrap(~term_type, scales = "free", nrow = 3) +
  tidytext::scale_y_reordered()

bagged_tree_vi_chart

bagged_tree_vi_chart %>% 
  ggsave(filename = "output/images/bagged_tree_vi_chart.png", 
         width = 12,
         height = 20)

bagged_tree_vi %>% 
  write_csv("output/bagged_tree_variable_importance.csv")

#predict bag against test data
bag_test_res <- bag_fit %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(.pred_dollar = 10^.pred)

model_metrics <- metric_set(rmse, rsq, mape)

test_metrics <- model_metrics(bag_test_res, truth = sale_price_adj, estimate = .pred_dollar)

test_metrics %>% 
  write_csv("data/modelling/metrics/test_metrics.csv")
