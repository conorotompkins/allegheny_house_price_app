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
lm_rmse_chart <- lm_fit %>%
  predict(train_data) %>%
  bind_cols(train_data) %>%
  group_by(geo_id) %>%
  rmse(log10(sale_price_adj), .pred) %>%
  mutate(geo_id = fct_reorder(geo_id, .estimate)) %>%
  ggplot(aes(.estimate, geo_id)) +
  geom_point()

lm_rmse_chart %>% 
  ggsave(filename = "output/images/lm_rmse_chart.png", height = 12)

lm_term_coefficient_chart <- lm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(term_type = case_when(str_detect(term, "^geo_id") ~ "geo_id",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               TRUE ~ "other")) %>% 
  add_count(term_type, name = "term_type_count") %>% 
  mutate(term = str_remove(term, term_type)) %>% 
  mutate(term_type = fct_reorder(term_type, term_type_count)) %>% 
  mutate(term = tidytext::reorder_within(term, estimate, term_type)) %>%
  ggplot(aes(estimate, term)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  facet_wrap(~term_type, scales = "free", nrow = 3) +
  tidytext::scale_y_reordered()

lm_term_coefficient_chart %>% 
  ggsave(filename = "output/images/lm_term_coefficient_chart.png", height = 18, width = 12)

#rf vi chart
rf_vi_chart <- rf_fit %>%
  pull_workflow_fit() %>%
  vi() %>% 
  rename(term = Variable,
         importance = Importance) %>% 
  mutate(term_type = case_when(str_detect(term, "^geo_id") ~ "geo_id",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               str_detect(term, "^sale_month_") ~ "sale_month",
                               TRUE ~ "other")) %>% 
  mutate(term = str_remove(term, term_type)) %>% 
  mutate(term_type = fct_reorder(term_type, importance, .fun = max, .desc = T)) %>% 
  mutate(term = tidytext::reorder_within(term, importance, term_type)) %>%
  ggplot(aes(importance, term)) +
  #geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  facet_wrap(~term_type, scales = "free", nrow = 3) +
  tidytext::scale_y_reordered()

rf_vi_chart %>% 
  ggsave(filename = "output/images/rf_vi_chart.png", height = 18, width = 12)

#need to find var imp in bagged tree model
bagged_tree_vi <- bag_fit %>% 
  pull_workflow_fit() %>% 
  .[['fit']] %>%
  var_imp()

bagged_tree_vi_chart <- bagged_tree_vi %>% 
  mutate(term_type = case_when(str_detect(term, "^geo_id") ~ "geo_id",
                               str_detect(term, "^grade_desc_") ~ "grade_desc",
                               str_detect(term, "^condition_desc_") ~ "condition_desc",
                               str_detect(term, "^style_desc_") ~ "style_desc",
                               str_detect(term, "^sale_month_") ~ "sale_month",
                               TRUE ~ "other")) %>% 
  mutate(term = str_remove(term, term_type)) %>% 
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
